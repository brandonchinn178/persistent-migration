{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Integration.Property (testProperties) where

import Control.Monad ((>=>))
import Control.Monad.Catch (SomeException(..), try)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Pool (Pool)
import Database.Persist.Migration.Internal
import Database.Persist.Sql (SqlBackend, SqlPersistT, rawExecute)
import Test.Integration.Utils.RunSql (runSql)
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Utils.QuickCheck ()

-- | A test suite for testing migration properties.
testProperties :: MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testProperties backend getPool = testGroup "properties"
  [ testProperty "Create and drop tables" $ monadicIO $ do
      table <- pick arbitrary
      fkTables <- pick $ getForeignKeyTables table
      let dropTable CreateTable{name} = runOperation' $ DropTable name
      runSql' $ do
        mapM_ runOperation' fkTables
        runOperation' table
        dropTable table
        mapM_ dropTable fkTables
  ]
  where
    runSql' f = run $ getPool >>= \pool -> runSql pool f
    runOperation' :: Migrateable op => op -> SqlPersistT IO ()
    runOperation' = runOperation backend

{- Helpers -}

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
