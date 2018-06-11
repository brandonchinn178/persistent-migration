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
    , Column(..)
    , ColumnProp(..)
    , CreateTable(..)
    , DropColumn(..)
    , DropTable(..)
    , MigrateBackend(..)
    , MigrateSettings
    , Migration
    , TableConstraint(..)
    )
import qualified Database.Persist.Migration as Migration
import Database.Persist.Migration.Sql (quote, uncommas)
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
  , addColumn = addColumn'
  , dropColumn = dropColumn'
  }

createTable' :: Bool -> CreateTable -> SqlPersistT IO [Text]
createTable' ifNotExists CreateTable{..} = return
  ["CREATE TABLE " <> ifNotExists' <> quote ctName <> "(" <> uncommas tableDefs <> ")"]
  where
    ifNotExists' = if ifNotExists then "IF NOT EXISTS " else ""
    tableDefs = map showColumn ctSchema ++ map showTableConstraint ctConstraints

dropTable' :: DropTable -> SqlPersistT IO [Text]
dropTable' DropTable{..} = return ["DROP TABLE " <> quote dtName]

addColumn' :: AddColumn -> SqlPersistT IO [Text]
addColumn' AddColumn{..} = return $ createQuery : maybeToList alterQuery
  where
    Column{..} = acColumn
    alterTable = "ALTER TABLE " <> quote acTable <> " "
    -- The CREATE query with the default specified by AddColumn{acDefault}
    createQuery = alterTable <> "ADD COLUMN " <> showColumn acColumn <> createDefault
    createDefault = case acDefault of
      Nothing -> ""
      Just def -> " DEFAULT " <> def
    -- The ALTER query to drop the default (if acDefault was set)
    setJust v = fmap $ const v
    alterQuery =
      setJust (alterTable <> "ALTER COLUMN " <> quote colName <> " DROP DEFAULT") acDefault

dropColumn' :: DropColumn -> SqlPersistT IO [Text]
dropColumn' DropColumn{..} = return ["ALTER TABLE " <> quote tab <> " DROP COLUMN " <> quote col]
  where
    (tab, col) = dcColumn

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
  PrimaryKey cols -> "PRIMARY KEY (" <> uncommas cols <> ")"
  Unique cols -> "UNIQUE (" <> uncommas cols <> ")"
