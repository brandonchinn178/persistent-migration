{-|
Module      :  Database.Persist.Migration.Sqlite
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the SQLite backend for migrations.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.Migration.Sqlite
  ( backend
  , runMigration
  , getMigration
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Migration.Internal
    ( AddColumn(..)
    , Column(..)
    , ColumnProp(..)
    , CreateTable(..)
    , DropColumn(..)
    , DropTable(..)
    , MigrateBackend(..)
    , Migration
    , TableConstraint(..)
    )
import qualified Database.Persist.Migration.Internal as Migration
import Database.Persist.Migration.Sql (quote, uncommas)
import Database.Persist.Sql (SqlPersistT, SqlType(..))

-- | 'Migration.runMigration' for SQLite.
runMigration :: MonadIO m => Migration -> SqlPersistT m ()
runMigration = Migration.runMigration backend

-- | 'Migration.getMigration' for SQLite.
getMigration :: MonadIO m => Migration -> SqlPersistT m [Text]
getMigration = Migration.getMigration backend

-- | The SQLite migration backend.
backend :: MigrateBackend
backend = MigrateBackend
  { createTable = createTable'
  , dropTable = dropTable'
  , addColumn = addColumn'
  , dropColumn = dropColumn'
  }

createTable' :: Bool -> CreateTable -> SqlPersistT IO [Text]
createTable' ifNotExists CreateTable{..} = return
  ["CREATE TABLE " <> ifNotExists' <> quote ctName <> "(" <> uncommas defs <> ")"]
  where
    ifNotExists' = if ifNotExists then "IF NOT EXISTS " else ""
    defs = map showColumn ctSchema ++ map showConstraint ctConstraints

dropTable' :: DropTable -> SqlPersistT IO [Text]
dropTable' = undefined

addColumn' :: AddColumn -> SqlPersistT IO [Text]
addColumn' = undefined

dropColumn' :: DropColumn -> SqlPersistT IO [Text]
dropColumn' = undefined

{- Helpers -}

-- | Render a 'Column' into its SQL representation.
showColumn :: Column -> Text
showColumn Column{..} = Text.unwords $ quote colName : showSqlType colType : map showProp colProps

-- | Render a 'TableConstraint' into its SQL representation.
showConstraint :: TableConstraint -> Text
showConstraint = \case
  PrimaryKey cols -> "PRIMARY KEY (" <> uncommas cols <> ")"
  Unique cols -> "UNIQUE (" <> uncommas cols <> ")"

-- | Render a 'ColumnProp' into its SQL representation.
showProp :: ColumnProp -> Text
showProp = \case
  NotNull -> "NOT NULL"
  Defaults def -> "DEFAULT " <> def
  ForeignKey (tab, col) -> "REFERENCES " <> tab <> "(" <> col <> ")"

-- | Render a 'SqlType' into its SQL representation. Copied from the
-- 'showSqlType' function in the persistent-sqlite library.
showSqlType :: SqlType -> Text
showSqlType = \case
  SqlString -> "VARCHAR"
  SqlInt32 -> "INTEGER"
  SqlInt64 -> "INTEGER"
  SqlReal -> "REAL"
  SqlNumeric precision scale -> "NUMERIC(" <> showT precision <> "," <> showT scale <> ")"
  SqlDay -> "DATE"
  SqlTime -> "TIME"
  SqlDayTime -> "TIMESTAMP"
  SqlBlob -> "BLOB"
  SqlBool -> "BOOLEAN"
  SqlOther t -> t
  where
    showT = Text.pack . show
