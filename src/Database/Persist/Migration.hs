{-|
Module      :  Database.Persist.Migration
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}

module Database.Persist.Migration
  ( hasMigration
  , checkMigration
  -- * Re-exports
  , module Database.Persist.Migration.Backend
  , module Database.Persist.Migration.Core
  , module Database.Persist.Migration.Operation
  , module Database.Persist.Migration.Operation.Types
  , module Database.Persist.Migration.Utils.Sql
  , PersistValue(..)
  , SqlType(..)
  , rawSql
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import Database.Persist.Migration.Backend
import Database.Persist.Migration.Core hiding (getMigration, runMigration)
import Database.Persist.Migration.Operation
import Database.Persist.Migration.Operation.Types
import Database.Persist.Migration.Utils.Sql
import Database.Persist.Sql (PersistValue(..), SqlType(..), rawSql)
import qualified Database.Persist.Sql as Persistent

-- | True if the persistent library detects more migrations unaccounted for.
hasMigration :: MonadIO m => Persistent.Migration -> Persistent.SqlPersistT m Bool
hasMigration = fmap (not . null) . Persistent.showMigration

-- | Fails if the persistent library detects more migrations unaccounted for.
checkMigration :: MonadIO m => Persistent.Migration -> Persistent.SqlPersistT m ()
checkMigration migration = do
  migrationText <- Persistent.showMigration migration
  unless (null migrationText) $ fail $
    unlines $ "More migrations detected:" : bullets migrationText
  where
    bullets = map ((" * " ++ ) . Text.unpack)
