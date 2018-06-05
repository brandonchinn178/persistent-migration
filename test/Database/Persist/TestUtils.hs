{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.TestUtils where

import Data.Conduit.List (sourceList)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Database.Persist.Migration
import Database.Persist.Sql (PersistValue(..), SqlBackend(..), Statement(..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec.Expectations (Expectation)

{- Mock test database -}

-- | The mock database backend for testing.
newtype TestBackend = TestBackend
  { version :: Maybe Version
  }

-- | The global test backend.
testBackend :: IORef TestBackend
testBackend = unsafePerformIO $ newIORef (TestBackend Nothing)
{-# NOINLINE testBackend #-}

-- | Run the given test with an initialized backend.
withTestBackend :: Expectation -> Expectation
withTestBackend test = writeIORef testBackend (TestBackend Nothing) >> test

-- | Modify the mock database.
modifyTestBackend :: (TestBackend -> TestBackend) -> IO ()
modifyTestBackend = modifyIORef testBackend

{- Mock persistent backends -}

-- | A mock migration backend for testing.
testMigrateBackend :: MigrateBackend
testMigrateBackend = MigrateBackend
  { createTable = \ifNotExists CreateTable{ctName} ->
      return ["CREATE TABLE " <> (if ifNotExists then "IF NOT EXISTS " else "") <> ctName]
  , dropTable = \DropTable{dtName} ->
      return ["DROP TABLE " <> dtName]
  , addColumn = \AddColumn{acTable, acColumn} ->
      return ["ADD COLUMN " <> dotted (acTable, colName acColumn)]
  , dropColumn = \DropColumn{dcColumn} ->
      return ["DROP COLUMN " <> dotted dcColumn]
  }

-- | Initialize a mock backend database for testing.
initSqlBackend :: IO SqlBackend
initSqlBackend = do
  smap <- newIORef Map.empty
  return SqlBackend
    { connPrepare = \case
        "CREATE TABLE IF NOT EXISTS persistent_migration" -> return stmt
        "SELECT version FROM persistent_migration ORDER BY timestamp DESC LIMIT 1" -> do
          TestBackend{version} <- readIORef testBackend
          let result = pure . PersistInt64 . fromIntegral <$> maybeToList version

          return stmt
            { stmtQuery = const $ return $ sourceList result
            }
        _ -> error "connPrepare"
    , connStmtMap = smap
    , connInsertSql = error "connInsertSql"
    , connUpsertSql = error "connUpsertSql"
    , connPutManySql = error "connPutManySql"
    , connInsertManySql = error "connInsertManySql"
    , connClose = error "connClose"
    , connMigrateSql = error "connMigrateSql"
    , connBegin = error "connBegin"
    , connCommit = error "connCommit"
    , connRollback = error "connRollback"
    , connEscapeName = error "connEscapeName"
    , connNoLimit = error "connNoLimit"
    , connRDBMS = error "connRDBMS"
    , connLimitOffset = error "connLimitOffset"
    , connLogFunc = \_ _ _ _ -> return ()
    , connMaxParams = error "connMaxParams"
    }
  where
    stmt = Statement
      { stmtFinalize = return ()
      , stmtReset = return ()
      , stmtExecute = \_ -> return 0
      , stmtQuery = error "stmtQuery"
      }
