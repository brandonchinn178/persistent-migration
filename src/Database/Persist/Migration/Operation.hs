{-|
Module      :  Database.Persist.Migration.Operation
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the Operation data types.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Migration.Operation
  ( Version
  , OperationPath
  , (~>)
  , Migration
  , MigrationPath(..)
  , opPath
  , Operation(..)
  , validateOperation
  ) where

import Control.Monad (when)
import Data.List (nub)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Database.Persist.Migration.Operation.Types
import Database.Persist.Migration.Utils.Data (isConstr)
import Database.Persist.Migration.Utils.Sql (MigrateSql)
import Database.Persist.Sql (PersistValue, SqlPersistT)

-- | The version of a database. An operation migrates from the given version to another version.
--
-- The version must be increasing, such that the lowest version is the first version and the highest
-- version is the most up-to-date version.
--
-- A version represents a version of the database schema. In other words, any set of operations
-- taken to get to version X *MUST* all result in the same database schema.
type Version = Int

-- | The path that an operation takes.
type OperationPath = (Version, Version)

-- | An infix constructor for 'OperationPath'.
(~>) :: Version -> Version -> OperationPath
(~>) = (,)

-- | A migration list that defines operations to manually migrate a database schema.
type Migration = [MigrationPath]

-- | A path representing the operations needed to run to get from one version of the database schema
-- to the next.
data MigrationPath = OperationPath := [Operation]
  deriving (Show)

-- | Get the OperationPath in the MigrationPath.
opPath :: MigrationPath -> OperationPath
opPath (path := _) = path

-- | An operation that can be migrated.
data Operation
  = CreateTable
      { name        :: Text
      , schema      :: [Column]
      , constraints :: [TableConstraint]
      }
  | DropTable
      { table :: Text
      }
  | RenameTable
      { from :: Text
      , to   :: Text
      }
  | AddConstraint
      { table      :: Text
      , constraint :: TableConstraint
      }
  | DropConstraint
      { table          :: Text
      , constraintName :: Text
      }
  | AddColumn
      { table      :: Text
      , column     :: Column
      , colDefault :: Maybe PersistValue
        -- ^ The default for existing rows; required if the column is non-nullable
      }
  | RenameColumn
      { table :: Text
      , from  :: Text
      , to    :: Text
      }
  | DropColumn
      { columnId :: ColumnIdentifier
      }
  | RawOperation
      { message :: Text
      , rawOp   :: SqlPersistT IO [MigrateSql]
      }
    -- ^ A custom operation that can be defined manually.
    --
    -- RawOperations should primarily use 'rawSql' and 'rawExecute' from the persistent library. If the
    -- operation depends on the backend being run, query 'connRDBMS' from the 'SqlBackend':
    --
    -- @
    -- asks connRDBMS >>= \case
    --   "sqlite" -> ...
    --   _ -> return ()
    -- @
  deriving (Show)

instance Show (SqlPersistT m a) where
  show _ = "<SqlPersistT>"

-- | Validate that the given Operation is valid.
validateOperation :: Operation -> Either String ()
validateOperation ct@CreateTable{..} = do
  when (null schema) $
    fail' "No columns specified in the schema"

  mapM_ validateColumn schema

  case length . filter (isConstr "PrimaryKey") $ constraints of
    0 -> fail' "No primary key specified"
    1 -> return ()
    _ -> fail' "Multiple primary keys specified"

  let getUniqueName (Unique n _) = Just n
      getUniqueName _ = Nothing
      uniqueNames = mapMaybe getUniqueName constraints
  when (length (nub uniqueNames) /= length uniqueNames) $
    fail' "Multiple unique constraints with the same name detected"

  let constraintCols = concatMap getConstraintColumns constraints
      schemaCols = map colName schema
  when (any (`notElem` schemaCols) constraintCols) $
    fail' "Table constraint references non-existent column"
  where
    fail' = Left . (++ ": " ++ show ct)

validateOperation ac@AddColumn{..} = do
  validateColumn column
  when (NotNull `elem` colProps column && isNothing colDefault) $
    fail' "Adding a non-nullable column requires a default"
  where
    fail' = Left . (++ ": " ++ show ac)

validateOperation _ = return ()
