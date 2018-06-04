{-|
Module      :  Database.Persist.Migration.Sqlite
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the SQLite backend for migrations.
-}

module Database.Persist.Migration.Sqlite
  ( backend
  , runMigration
  , getMigration
  ) where

import Data.Text (Text)
import Database.Persist.Migration.Internal
    ( AddColumn(..)
    , CreateTable(..)
    , DropColumn(..)
    , DropTable(..)
    , MigrateBackend(..)
    , Migration
    )
import qualified Database.Persist.Migration.Internal as Migration
import Database.Persist.Sql (SqlPersistT)

-- | 'Migration.runMigration' for SQLite.
runMigration :: Migration -> SqlPersistT IO ()
runMigration = Migration.runMigration backend

-- | 'Migration.getMigration' for SQLite.
getMigration :: Migration -> SqlPersistT IO [Text]
getMigration = Migration.getMigration backend

-- | The SQLite migration backend.
backend :: MigrateBackend
backend = MigrateBackend
  { createTable = createTable'
  , dropTable = dropTable'
  , addColumn = addColumn'
  , dropColumn = dropColumn'
  }

createTable' :: Bool -> CreateTable -> SqlPersistT IO [Text]
createTable' = undefined

dropTable' :: DropTable -> SqlPersistT IO [Text]
dropTable' = undefined

addColumn' :: AddColumn -> SqlPersistT IO [Text]
addColumn' = undefined

dropColumn' :: DropColumn -> SqlPersistT IO [Text]
dropColumn' = undefined

