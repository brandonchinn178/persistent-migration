{-|
Module      :  Database.Persist.Migration
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}

module Database.Persist.Migration
  (
  -- * Operation types
    Operation(..)
  , SubOperation(..)
  -- * Migration types
  , Migration
  , MigrateBackend(..)
  , Migrateable(..)
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

import Database.Persist.Migration.Internal
