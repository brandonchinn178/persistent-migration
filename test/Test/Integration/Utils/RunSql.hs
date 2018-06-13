module Test.Integration.Utils.RunSql
  ( runSql
  , runMigration
  ) where

import Control.Monad.Reader (runReaderT)
import Data.Pool (Pool, withResource)
import Database.Persist.Migration (MigrateBackend, Migration, defaultSettings)
import qualified Database.Persist.Migration as Migration
import Database.Persist.Sql (SqlBackend, SqlPersistT)

-- | Run the given persistent query.
runSql :: Pool SqlBackend -> SqlPersistT IO a -> IO a
runSql pool = withResource pool . runReaderT

-- | Run the given migration.
runMigration :: MigrateBackend -> Pool SqlBackend -> Migration -> IO ()
runMigration backend pool = runSql pool . Migration.runMigration backend defaultSettings
