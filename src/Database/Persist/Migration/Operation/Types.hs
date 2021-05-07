{-|
Module      :  Database.Persist.Migration.Operation.Types
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines auxiliary data types that can be used in Operations.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.Migration.Operation.Types
  ( ColumnIdentifier
  , dotted
  , Column(..)
  , validateColumn
  , ColumnProp(..)
  , TableConstraint(..)
  , isPrimaryKey
  , getConstraintColumns
  ) where

import Control.Monad (when)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (PersistValue(..))
import Database.Persist.Types (SqlType)

-- | A column identifier, table.column
type ColumnIdentifier = (Text, Text)

-- | Make a ColumnIdentifier displayable.
dotted :: ColumnIdentifier -> Text
dotted (tab, col) = Text.concat [tab, ".", col]

-- | The definition for a Column in a SQL database.
data Column = Column
  { colName  :: Text
  , colType  :: SqlType
  , colProps :: [ColumnProp]
  } deriving (Show)

-- | Validate a Column.
validateColumn :: Column -> Either String ()
validateColumn col@Column{..} = when (hasDuplicates $ map getColumnPropName colProps) $
  Left $ "Duplicate column properties detected: " ++ show col
  where
    hasDuplicates l = length l /= length (nub l)

    getColumnPropName :: ColumnProp -> String
    getColumnPropName = \case
      NotNull{} -> "NotNull"
      References{} -> "References"
      AutoIncrement{} -> "AutoIncrement"
      Default{} -> "Default"

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
  deriving (Show,Eq)

-- | Table constraints in a CREATE query.
data TableConstraint
  = PrimaryKey [Text] -- ^ PRIMARY KEY (col1, col2, ...)
  | Unique Text [Text] -- ^ CONSTRAINT name UNIQUE (col1, col2, ...)
  deriving (Show)

isPrimaryKey :: TableConstraint -> Bool
isPrimaryKey = \case
  PrimaryKey{} -> True
  _ -> False

-- | Get the columns defined in the given TableConstraint.
getConstraintColumns :: TableConstraint -> [Text]
getConstraintColumns = \case
  PrimaryKey cols -> cols
  Unique _ cols -> cols
