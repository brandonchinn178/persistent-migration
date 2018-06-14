{-|
Module      :  Database.Persist.Migration
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}

module Database.Persist.Migration
  -- * Operation types
  ( Version
  , OperationPath
  , (~>)
  , Operation(..)
  -- * Migration types
  , Migration
  , MigrateBackend(..)
  , Migrateable(..)
  -- * Migration functions
  , MigrateSettings(..)
  , defaultSettings
  , hasMigration
  , checkMigration
  -- * Core operations
  , CreateTable(..)
  , DropTable(..)
  , AddColumn(..)
  , DropColumn(..)
  , RawOperation(..)
  , NoOp(..)
  -- * Auxiliary types
  , ColumnIdentifier
  , dotted
  , Column(..)
  , ColumnProp(..)
  , TableConstraint(..)
  ) where

import Control.Monad (unless)
import qualified Data.Text as Text
import Database.Persist.Migration.Internal
import qualified Database.Persist.Sql as Persistent

-- | True if the persistent library detects more migrations unaccounted for.
hasMigration :: Persistent.Migration -> Persistent.SqlPersistT IO Bool
hasMigration = fmap (not . null) . Persistent.showMigration

-- | Fails if the persistent library detects more migrations unaccounted for.
checkMigration :: Persistent.Migration -> Persistent.SqlPersistT IO ()
checkMigration migration = do
  migrationText <- Persistent.showMigration migration
  unless (null migrationText) $ fail $
    unlines $ "More migrations detected:" : bullets migrationText
  where
    bullets = map ((" * " ++ ) . Text.unpack)
