{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.TestUtils where

import Data.Conduit (yield)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Database.Persist.Migration
import Database.Persist.Sql (PersistValue(..), SqlBackend(..), Statement(..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec.Expectations (Expectation)

{- Mock test database -}

-- | The mock database backend for testing.
newtype TestBackend = TestBackend
  { doneOps :: [OperationId] -- ^ Operation IDs that have already been run
  }

-- | The global test backend.
testBackend :: IORef TestBackend
testBackend = unsafePerformIO $ newIORef (TestBackend [])
{-# NOINLINE testBackend #-}

-- | Run the given test with an initialized backend.
withTestBackend :: Expectation -> Expectation
withTestBackend test = writeIORef testBackend (TestBackend []) >> test

-- | Modify the mock database.
modifyTestBackend :: (TestBackend -> TestBackend) -> IO ()
modifyTestBackend = modifyIORef testBackend

{- Mock persistent backends -}

-- | A mock migration backend for testing.
testMigrateBackend :: MigrateBackend
testMigrateBackend = MigrateBackend
  { createTable = \ifExists CreateTable{ctName} ->
      return ["CREATE TABLE " <> (if ifExists then "IF EXISTS " else "") <> ctName]
  , dropTable = \DropTable{dtName} ->
      return ["DROP TABLE " <> dtName]
  , addColumn = \AddColumn{acTable, acColumn} ->
      return ["ADD COLUMN " <> dot acTable (colName acColumn)]
  , dropColumn = \DropColumn{dcTable, dcColumn} ->
      return ["DROP COLUMN " <> dot dcTable dcColumn]
  }
  where
    dot tab col = tab <> "." <> col

-- | Initialize a mock backend database for testing.
initSqlBackend :: IO SqlBackend
initSqlBackend = do
  smap <- newIORef Map.empty
  return SqlBackend
    { connPrepare = \case
        "CREATE TABLE IF EXISTS persistent_migration" -> return stmt
        "SELECT opId FROM persistent_migration" -> do
          TestBackend{doneOps} <- readIORef testBackend
          let result = map (pure . PersistInt64 . fromIntegral) doneOps
          return stmt
            { stmtQuery = const $ return $ mapM_ yield result
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
