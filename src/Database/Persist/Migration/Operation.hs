{-|
Module      :  Database.Persist.Migration.Operation
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the Operation data types.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Persist.Migration.Operation
  ( Version
  , OperationPath
  , (~>)
  , Operation(..)
  ) where

import Database.Persist.Migration.Operation.Class (Migrateable)

-- | The version of a database. An operation migrates from the given version to another version.
--
-- The version must be increasing, such that the lowest version is the first version and the highest
-- version is the most up-to-date version.
type Version = Int

-- | The path that an operation takes.
type OperationPath = (Version, Version)

-- | An infix constructor for 'OperationPath'.
(~>) :: Int -> Int -> OperationPath
(~>) = (,)

-- | An operation that can be migrated.
data Operation =
  forall op. Migrateable op =>
  Operation
    { opPath :: OperationPath
    , opOp   :: op
    }

deriving instance Show Operation
