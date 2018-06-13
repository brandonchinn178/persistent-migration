{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Integration.Property (testProperties) where

import Control.Monad ((>=>))
import Control.Monad.Catch (SomeException(..), try)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, rawExecute)
import Database.Persist.Migration
  ( CreateTable(..)
  , Migrateable(..)
  , MigrateBackend
  )
import Database.Persist.Migration.Sql (quote)
import Test.Integration.Utils.RunSql (runSql)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Utils.QuickCheck (genCreateTable)

-- | A test suite for testing migration properties.
testProperties :: MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testProperties backend getPool = testGroup "properties"
  [ testProperty "Create arbitrary tables" $ monadicIO $ do
      tables <- pick genCreateTable
      let createTable = getMigrationText backend >=> mapM_ rawExecute'
          dropTable = rawExecute' . ("DROP TABLE " <>) . quote . ctName
      runSql' $ do
        mapM_ createTable tables
        mapM_ dropTable $ reverse tables
  ]
  where
    runSql' f = run $ getPool >>= \pool -> runSql pool f
    rawExecute' sql = try (rawExecute sql []) >>= \case
      Right () -> return ()
      Left (SomeException e) -> do
        liftIO $ print sql
        fail $ show e
