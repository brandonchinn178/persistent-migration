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
  , module Backend
  , module Core
  , module Operation
  , module Operation.Class
  , module Operation.Types
  , PersistValue(..)
  , SqlType(..)
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import Database.Persist (PersistValue(..), SqlType(..))
import Database.Persist.Migration.Backend as Backend
import Database.Persist.Migration.Core as Core hiding (getMigration, runMigration)
import Database.Persist.Migration.Operation as Operation
import Database.Persist.Migration.Operation.Class as Operation.Class
import Database.Persist.Migration.Operation.Types as Operation.Types
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
