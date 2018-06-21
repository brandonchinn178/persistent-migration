{-|
Module      :  Database.Persist.Migration.Backend
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines `MigrateBackend`, the data constructor that each SQL backend will need to implement.
-}

module Database.Persist.Migration.Backend (MigrateBackend(..)) where

import Data.Text (Text)
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.Migration.Operation.Types

-- | The backend to migrate with.
data MigrateBackend = MigrateBackend
  { createTable    :: Bool -> CreateTable -> SqlPersistT IO [Text]
      -- ^ create a table (True = IF NOT EXISTS)
  , dropTable      :: DropTable -> SqlPersistT IO [Text]
  , renameTable    :: RenameTable -> SqlPersistT IO [Text]
  , addConstraint  :: AddConstraint -> SqlPersistT IO [Text]
  , dropConstraint :: DropConstraint -> SqlPersistT IO [Text]
  , addColumn      :: AddColumn -> SqlPersistT IO [Text]
  , dropColumn     :: DropColumn -> SqlPersistT IO [Text]
  }
