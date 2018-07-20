{-|
Module      :  Database.Persist.Migration.Operation.Types
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the data types that can be used in Operations.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Migration.Operation.Types
  ( -- * Core operations
    CreateTable(..)
  , DropTable(..)
  , RenameTable(..)
  , AddConstraint(..)
  , DropConstraint(..)
  , AddColumn(..)
  , RenameColumn(..)
  , DropColumn(..)
    -- * Special operations
  , RawOperation(..)
    -- * Auxiliary types
  , ColumnIdentifier
  , dotted
  , Column(..)
  , validateColumn
  , ColumnProp(..)
  , TableConstraint(..)
  , getConstraintColumns
  ) where

import Control.Monad (when)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Migration.Utils.Data (hasDuplicateConstrs)
import Database.Persist.Sql (PersistValue(..), SqlPersistT)
import Database.Persist.Types (SqlType)

-- | An operation to create a table according to the specified schema.
data CreateTable = CreateTable
  { name        :: Text
  , schema      :: [Column]
  , constraints :: [TableConstraint]
  } deriving (Show)

-- | An operation to drop the given table.
newtype DropTable = DropTable
  { table :: Text
  }
  deriving (Show)

-- | An operation to rename a table.
data RenameTable = RenameTable
  { from :: Text
  , to   :: Text
  } deriving (Show)

-- | An operation to add a table constraint.
data AddConstraint = AddConstraint
  { table      :: Text
  , constraint :: TableConstraint
  } deriving (Show)

-- | An operation to drop a table constraint.
data DropConstraint = DropConstraint
  { table      :: Text
  , constraint :: Text
  } deriving (Show)

-- | An operation to add the given column to an existing table.
data AddColumn = AddColumn
  { table      :: Text
  , column     :: Column
  , colDefault :: Maybe PersistValue
    -- ^ The default for existing rows; required if the column is non-nullable
  } deriving (Show)

-- | An operation to rename the given column.
data RenameColumn = RenameColumn
  { table :: Text
  , from  :: Text
  , to    :: Text
  } deriving (Show)

-- | An operation to drop the given column to an existing table.
newtype DropColumn = DropColumn
  { column :: ColumnIdentifier
  } deriving (Show)

-- | A custom operation that can be defined manually.
--
-- RawOperations should primarily use 'rawSql' and 'rawExecute' from the persistent library. If the
-- operation depends on the backend being run, query 'connRDBMS' from the 'SqlBackend':
--
-- @
-- asks connRDBMS >>= \case
--   "sqlite" -> ...
--   _ -> return ()
-- @
data RawOperation = RawOperation
  { message :: Text
  , rawOp   :: SqlPersistT IO [Text]
  }

instance Show RawOperation where
  show RawOperation{message} = "RawOperation: " ++ Text.unpack message

-- | A column identifier, table.column
type ColumnIdentifier = (Text, Text)

-- | Make a ColumnIdentifier displayable.
dotted :: ColumnIdentifier -> Text
dotted (tab, col) = tab <> "." <> col

-- | The definition for a Column in a SQL database.
data Column = Column
  { colName  :: Text
  , colType  :: SqlType
  , colProps :: [ColumnProp]
  } deriving (Show)

-- | Validate a Column.
validateColumn :: Column -> Either String ()
validateColumn col@Column{..} = when (hasDuplicateConstrs colProps) $
  Left $ "Duplicate column properties detected: " ++ show col

-- | A property for a 'Column'.
data ColumnProp
  = NotNull
    -- ^ Makes a column non-nullable (defaults to nullable)
  | References ColumnIdentifier
    -- ^ Mark this column as a foreign key to the given column
  | AutoIncrement
    -- ^ Makes a column auto-incrementing
  | Default PersistValue
    -- ^ Sets the default value for the column. Note that this doesn't matter when inserting
    -- data via Haskell; this property only sets the schema in the SQL backend.
    --
    -- See 'AddColumn' for setting the default value for existing rows in a migration.
    --
    -- More info: https://www.yesodweb.com/book/persistent#persistent_attributes
  deriving (Show,Eq,Data)

deriving instance Data PersistValue

-- | Table constraints in a CREATE query.
data TableConstraint
  = PrimaryKey [Text] -- ^ PRIMARY KEY (col1, col2, ...)
  | Unique Text [Text] -- ^ CONSTRAINT name UNIQUE (col1, col2, ...)
  deriving (Show,Data)

-- | Get the columns defined in the given TableConstraint.
getConstraintColumns :: TableConstraint -> [Text]
getConstraintColumns = \case
  PrimaryKey cols -> cols
  Unique _ cols -> cols
