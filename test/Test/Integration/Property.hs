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
import Database.Persist.Sql (SqlBackend, SqlPersistT, rawExecute)
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
  [ testProperty "Create and drop tables" $ withCreateTable' $
      const $ return ()
  , testProperty "Rename table" $ withCreateTable' $ \(table, fkTables) -> do
      let tableName = name table
          fkNames = map name fkTables
      Identifier newName <- pick $ arbitrary `suchThat`
        ((`notElem` tableName:fkNames) . unIdent)
      runSqlPool' $ do
        runOperation' $ RenameTable tableName newName
        runOperation' $ DropTable newName
  , testProperty "Add UNIQUE constraint" $ withCreateTable' $ \(table, _) -> do
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
  , testProperty "Drop UNIQUE constraint" $ withCreateTable' $ \(table, _) -> do
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
  ]
  where
    withCreateTable' :: PseudoBool a => ((CreateTable, [CreateTable]) -> PropertyM IO a) -> Property
    withCreateTable' = withCreateTable getPool backend
    runSqlPool' = runSqlPool getPool
    runOperation' :: Migrateable op => op -> SqlPersistT IO ()
    runOperation' = runOperation backend

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

-- | Create a table and its foreign key dependencies, run the given action, and drop the tables.
withCreateTable
  :: PseudoBool a
  => IO (Pool SqlBackend)
  -> MigrateBackend
  -> ((CreateTable, [CreateTable]) -> PropertyM IO a)
  -> Property
withCreateTable getPool backend action = monadicIO $ do
  table <- pick arbitrary
  fkTables <- pick $ getForeignKeyTables table
  runSqlPool' $ mapM_ runOperation' (fkTables ++ [table])
  isSuccess <- toBool <$> action (table, fkTables)
  runSqlPool' $ mapM_ dropTable' (table:fkTables)
  unless isSuccess $ stop rejected
  where
    dropTable' CreateTable{name} = runOperation' $ DropTable name
    runSqlPool' = runSqlPool getPool
    runOperation' :: Migrateable op => op -> SqlPersistT IO ()
    runOperation' = runOperation backend

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
