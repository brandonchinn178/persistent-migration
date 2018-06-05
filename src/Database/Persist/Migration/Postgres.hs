{-|
Module      :  Database.Persist.Migration.Postgres
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the migration backend for PostgreSQL.
-}

module Database.Persist.Migration.Postgres
  ( backend
  , getMigration
  , runMigration
  ) where

import Data.Text (Text)
import Database.Persist.Migration
    ( AddColumn(..)
    , CreateTable(..)
    , DropColumn(..)
    , DropTable(..)
    , MigrateBackend(..)
    , Migration
    )
import qualified Database.Persist.Migration as Migration
import Database.Persist.Sql (SqlPersistT)

-- | Run a migration with the Postgres backend.
runMigration :: Migration -> SqlPersistT IO ()
runMigration = Migration.runMigration backend

-- | Get a migration with the Postgres backend.
getMigration :: Migration -> SqlPersistT IO [Text]
getMigration = Migration.getMigration backend

-- | The migration backend for Postgres.
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
