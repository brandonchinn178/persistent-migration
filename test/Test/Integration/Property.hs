{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Integration.Property (testProperties) where

import Control.Monad (unless, (>=>))
import Control.Monad.Catch (SomeException(..), try)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import qualified Data.Text as Text
import Database.Persist.Migration.Internal
import Database.Persist.Sql
    (PersistValue(..), SqlBackend, SqlPersistT, SqlType(..), rawExecute)
import Test.Integration.Utils.RunSql (runSql)
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, monadicIO, pick, run, stop)
import Test.QuickCheck.Property (rejected)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Utils.QuickCheck (Identifier(..))

-- | A test suite for testing migration properties.
testProperties :: MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testProperties backend getPool = testGroup "properties"
  [ testProperty "Create and drop tables" $ withCreateTable $
      const $ return ()
  , testProperty "Rename table" $ withCreateTable $ \(table, fkTables) -> do
      let tableName = name table
          fkNames = map name fkTables
      Identifier newName <- pick $ arbitrary `suchThat`
        ((`notElem` tableName:fkNames) . unIdent)
      runSqlPool' $ do
        runOperation' $ RenameTable tableName newName
        runOperation' $ DropTable newName
  , testProperty "Add UNIQUE constraint" $ withCreateTable $ \(table, _) -> do
      let getUniqueCols = \case
            PrimaryKey _ -> []
            Unique _ cols -> cols
          tableCols = map colName $ schema table
          uniqueCols = concatMap getUniqueCols $ constraints table
          nonUniqueCols = take 32 $ filter (`notElem` uniqueCols) tableCols
      Identifier uniqueName <- pick arbitrary
      let uniqueName' = Text.take 63 $ "unique_" <> uniqueName
      runSqlPool' $
        runOperation' $ AddConstraint (name table) $ Unique uniqueName' nonUniqueCols
  , testProperty "Drop UNIQUE constraint" $ withCreateTable $ \(table, _) -> do
      let getUniqueName = \case
            PrimaryKey _ -> Nothing
            Unique n _ -> Just n
          uniqueNames = mapMaybe getUniqueName $ constraints table
      if null uniqueNames
        then return False
        else do
          uniqueName <- pick $ elements uniqueNames
          runSqlPool' $ runOperation' $ DropConstraint (name table) uniqueName
          return True
  , testProperty "Add column" $ withCreateTable $ \(table, fkTables) -> do
      -- generate a new column
      col <- pick arbitrary

      -- pick a new name for the column
      let cols = map colName $ schema table
      Identifier newName <- pick $ arbitrary `suchThat` ((`notElem` cols) . unIdent)

      -- if foreign key tables exist, update any foreign key references to point to one of those.
      -- otherwise, strip out any foreign key references.
      let splitProps [] = (False, [])
          splitProps (x:xs) =
            let (mRef, props) = splitProps xs
            in case x of
              References _ -> (True, props)
              AutoIncrement -> (mRef, props) -- don't test adding AutoIncrement columns
              _ -> (mRef, x:props)
      col' <-
        let (hasReference, props) = splitProps $ colProps col
        in if null fkTables || not hasReference
            then return col{colProps = props}
            else do
              fkTable <- pick $ name <$> elements fkTables
              return $ col{colProps = References (fkTable, "id") : props}

      -- pick a default value according to nullability and sqltype
      defaultVal <- fmap Just $ pick $ case colType col of
        SqlString -> PersistText <$> arbitrary
        SqlInt32 -> PersistInt64 <$> choose (-2147483648, 2147483647)
        SqlInt64 -> PersistInt64 <$> choose (-2147483648, 2147483647)
        SqlReal -> PersistDouble <$> arbitrary
        SqlNumeric _ _ -> PersistRational <$> arbitrary
        SqlBool -> PersistBool <$> arbitrary
        SqlDay -> PersistDay <$> arbitrary
        SqlTime -> PersistTimeOfDay <$> arbitrary
        SqlDayTime -> PersistUTCTime <$> arbitrary
        SqlBlob -> PersistByteString <$> arbitrary
        SqlOther _ -> fail "SqlOther not supported"
      defaultVal' <- if NotNull `elem` colProps col
        then return defaultVal
        else pick $ elements [Nothing, defaultVal]

      runSqlPool' $ runOperation' $ AddColumn (name table) col'{colName = newName} defaultVal'
  , testProperty "Drop column" $ withCreateTable $ \(table, _) -> do
      let cols = map colName $ schema table
      col <- pick $ elements cols
      runSqlPool' $ runOperation' $ DropColumn (name table, col)
  ]
  where
    runSqlPool' = runSqlPool getPool
    runOperation' :: Migrateable op => op -> SqlPersistT IO ()
    runOperation' = runOperation backend
    -- | Create a table and its foreign key dependencies, then run the given action, which should
    -- return False if the test case should be discarded (`()` == True). The tables will be dropped
    -- when finished
    withCreateTable :: PseudoBool a => ((CreateTable, [CreateTable]) -> PropertyM IO a) -> Property
    withCreateTable action = monadicIO $ do
      table <- pick arbitrary
      fkTables <- pick $ getForeignKeyTables table
      runSqlPool' $ mapM_ runOperation' (fkTables ++ [table])
      isSuccess <- toBool <$> action (table, fkTables)
      runSqlPool' $ mapM_ dropTable' (table:fkTables)
      unless isSuccess $ stop rejected
    dropTable' CreateTable{name} = runOperation' $ DropTable name

{- Helpers -}

-- | Run the given Sql query in the given SqlBackend.
runSqlPool :: IO (Pool SqlBackend) -> SqlPersistT IO () -> PropertyM IO ()
runSqlPool getPool f = run $ getPool >>= \pool -> runSql pool f

-- | Run the given operation.
runOperation :: Migrateable op => MigrateBackend -> op -> SqlPersistT IO ()
runOperation backend = getMigrationText backend >=> mapM_ rawExecutePrint
  where
    -- if rawExecute fails, show the sql query run
    rawExecutePrint sql = try (rawExecute sql []) >>= \case
      Right () -> return ()
      Left (SomeException e) -> do
        liftIO $ print sql
        fail $ show e

-- | Get the CreateTable operations that are necessary for the foreign keys in the
-- given CreateTable operation.
getForeignKeyTables :: CreateTable -> Gen [CreateTable]
getForeignKeyTables ct =
  zipWith modifyTable neededTables <$> vectorOf (length neededTables) arbitrary
  where
    neededTables = nub $ concatMap (mapMaybe getReferenceTable . colProps) $ schema ct
    getReferenceTable = \case
      References (table, _) -> Just table
      _ -> Nothing
    isReference = \case
      References _ -> True
      _ -> False
    noFKs = filter (not . isReference) . colProps
    modifyTable name ct' = ct'
      { name = name
      , schema = map (\col -> col{colProps = noFKs col}) $ schema ct'
      }

class PseudoBool a where
  toBool :: a -> Bool
instance PseudoBool () where
  toBool = const True
instance PseudoBool Bool where
  toBool = id
