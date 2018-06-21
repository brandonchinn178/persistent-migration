{-|
Module      :  Database.Persist.Migration.Postgres
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the migration backend for PostgreSQL.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Persist.Migration.Postgres
  ( backend
  , getMigration
  , runMigration
  ) where

import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Migration
    ( AddColumn(..)
    , AddConstraint(..)
    , Column(..)
    , ColumnProp(..)
    , CreateTable(..)
    , DropColumn(..)
    , DropConstraint(..)
    , DropTable(..)
    , MigrateBackend(..)
    , MigrateSettings
    , Migration
    , RenameTable(..)
    , TableConstraint(..)
    )
import qualified Database.Persist.Migration.Core as Migration
import Database.Persist.Migration.Utils.Sql
    (quote, showValue, uncommas, uncommas')
import Database.Persist.Sql (SqlPersistT, SqlType(..))

-- | Run a migration with the Postgres backend.
runMigration :: MigrateSettings -> Migration -> SqlPersistT IO ()
runMigration = Migration.runMigration backend

-- | Get a migration with the Postgres backend.
getMigration :: MigrateSettings -> Migration -> SqlPersistT IO [Text]
getMigration = Migration.getMigration backend

-- | The migration backend for Postgres.
backend :: MigrateBackend
backend = MigrateBackend
  { createTable = createTable'
  , dropTable = dropTable'
  , renameTable = renameTable'
  , addConstraint = addConstraint'
  , dropConstraint = dropConstraint'
  , addColumn = addColumn'
  , dropColumn = dropColumn'
  }

createTable' :: Bool -> CreateTable -> SqlPersistT IO [Text]
createTable' ifNotExists CreateTable{..} = return
  ["CREATE TABLE " <> ifNotExists' <> quote name <> "(" <> uncommas tableDefs <> ")"]
  where
    ifNotExists' = if ifNotExists then "IF NOT EXISTS " else ""
    tableDefs = map showColumn schema ++ map showTableConstraint constraints

dropTable' :: DropTable -> SqlPersistT IO [Text]
dropTable' DropTable{..} = return ["DROP TABLE IF EXISTS " <> quote table]

renameTable' :: RenameTable -> SqlPersistT IO [Text]
renameTable' RenameTable{..} = return
  ["ALTER TABLE " <> quote from <> " RENAME TO " <> quote to]

addConstraint' :: AddConstraint -> SqlPersistT IO [Text]
addConstraint' AddConstraint{..} = return ["ALTER TABLE " <> quote table <> " " <> statement]
  where
    statement = case constraint of
      PrimaryKey cols -> "ADD PRIMARY KEY (" <> uncommas' cols <> ")"
      Unique label cols -> "ADD CONSTRAINT " <> quote label <> " UNIQUE (" <> uncommas' cols <> ")"

dropConstraint' :: DropConstraint -> SqlPersistT IO [Text]
dropConstraint' DropConstraint{..} = return
  ["ALTER TABLE " <> quote table <> " DROP CONSTRAINT " <> constraint]

addColumn' :: AddColumn -> SqlPersistT IO [Text]
addColumn' AddColumn{..} = return $ createQuery : maybeToList alterQuery
  where
    Column{..} = column
    alterTable = "ALTER TABLE " <> quote table <> " "
    -- The CREATE query with the default specified by AddColumn{colDefault}
    createQuery = alterTable <> "ADD COLUMN " <> showColumn column <> createDefault
    createDefault = case colDefault of
      Nothing -> ""
      Just def -> " DEFAULT " <> showValue def
    -- The ALTER query to drop the default (if colDefault was set)
    setJust v = fmap $ const v
    alterQuery =
      setJust (alterTable <> "ALTER COLUMN " <> quote colName <> " DROP DEFAULT") colDefault

dropColumn' :: DropColumn -> SqlPersistT IO [Text]
dropColumn' DropColumn{..} = return ["ALTER TABLE " <> quote tab <> " DROP COLUMN " <> quote col]
  where
    (tab, col) = column

{- Helpers -}

-- | Show a 'Column'.
showColumn :: Column -> Text
showColumn Column{..} =
  Text.unwords
    $ quote colName
    : sqlType
    : map showColumnProp colProps
  where
    sqlType = if AutoIncrement `elem` colProps
      then "SERIAL"
      else showSqlType colType

-- | Show a 'SqlType'. See `showSqlType` from `Database.Persist.Postgresql`.
showSqlType :: SqlType -> Text
showSqlType = \case
  SqlString -> "VARCHAR"
  SqlInt32 -> "INT4"
  SqlInt64 -> "INT8"
  SqlReal -> "DOUBLE PRECISION"
  SqlNumeric s prec -> "NUMERIC(" <> showT s <> "," <> showT prec <> ")"
  SqlDay -> "DATE"
  SqlTime -> "TIME"
  SqlDayTime -> "TIMESTAMP WITH TIME ZONE"
  SqlBlob -> "BYTEA"
  SqlBool -> "BOOLEAN"
  SqlOther (Text.toLower -> "integer") -> "INT4"
  SqlOther t -> t
  where
    showT = Text.pack . show

-- | Show a 'ColumnProp'.
showColumnProp :: ColumnProp -> Text
showColumnProp = \case
  NotNull -> "NOT NULL"
  References (tab, col) -> "REFERENCES " <> quote tab <> "(" <> quote col <> ")"
  AutoIncrement -> ""

-- | Show a `TableConstraint`.
showTableConstraint :: TableConstraint -> Text
showTableConstraint = \case
  PrimaryKey cols -> "PRIMARY KEY (" <> uncommas' cols <> ")"
  Unique name cols -> "CONSTRAINT " <> quote name <> " UNIQUE (" <> uncommas' cols <> ")"
