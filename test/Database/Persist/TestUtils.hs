{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.TestUtils where

import Control.Monad.Reader (runReaderT)
import Data.Monoid ((<>))
import Database.Persist.Migration
import Database.Persist.Sql (SqlBackend(..), SqlPersistT)

-- | A migration backend for testing.
testMigrateBackend :: MigrateBackend
testMigrateBackend = MigrateBackend
  { setupMigrations = return ()
  , getCompletedOps = return []
  , saveMigration = const $ return ()
  , createTable = \CreateTable{ctName} ->
      return ["CREATE TABLE " <> ctName]
  , dropTable = \DropTable{dtName} ->
      return ["DROP TABLE " <> dtName]
  , addColumn = \AddColumn{acTable, acColumn} ->
      return ["ADD COLUMN " <> dot acTable (colName acColumn)]
  , dropColumn = \DropColumn{dcTable, dcColumn} ->
      return ["DROP COLUMN " <> dot dcTable dcColumn]
  }
  where
    dot tab col = tab <> "." <> col

-- | A backend database for testing.
testSqlBackend :: SqlBackend
testSqlBackend = SqlBackend
  { connPrepare = uninitialized
  , connStmtMap = uninitialized
  , connInsertSql = uninitialized
  , connUpsertSql = uninitialized
  , connInsertManySql = uninitialized
  , connClose = uninitialized
  , connMigrateSql = uninitialized
  , connBegin = uninitialized
  , connCommit = uninitialized
  , connRollback = uninitialized
  , connEscapeName = uninitialized
  , connNoLimit = uninitialized
  , connRDBMS = uninitialized
  , connLimitOffset = uninitialized
  , connLogFunc = uninitialized
  , connMaxParams = uninitialized
  }
  where
    uninitialized = error "Cannot query test backend"

-- | Run with the test backend.
runTestBackend :: SqlPersistT IO a -> IO a
runTestBackend m = runReaderT m testSqlBackend
