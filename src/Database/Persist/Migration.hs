{-|
Module      :  Database.Persist.Migration
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}

module Database.Persist.Migration
  -- * Operation types
  ( OperationId
  , Operation(..)
  , SubOperation(..)
  -- * Migration types
  , Migration
  , MigrateBackend(..)
  , Migrateable(..)
  , MigrateAction(..)
  , MigratePlan(..)
  -- * Migration functions
  , runMigration
  , getMigration
  , checkMigration
  -- * Core operations
  , CreateTable(..)
  , DropTable(..)
  , AddColumn(..)
  , DropColumn(..)
  , RawOperation(..)
  , Revert(..)
  , Squash(..)
  , NoOp(..)
  -- * Auxiliary types
  , Column(..)
  , ColumnProp(..)
  , TableConstraint(..)
  ) where

import Control.Monad (unless)
import qualified Data.Text as Text
import Database.Persist.Migration.Internal
import qualified Database.Persist.Sql as Persistent

-- | Fails if the persistent library detects more migrations unaccounted for.
checkMigration :: Persistent.Migration -> Persistent.SqlPersistT IO ()
checkMigration migration = do
  migrationText <- Persistent.showMigration migration
  unless (null migrationText) $ fail $
    unlines $ "More migrations detected:" : bullets migrationText
  where
    bullets = map ((" * " ++ ) . Text.unpack)
