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
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Migration
import qualified Database.Persist.Migration.Core as Migration
import Database.Persist.Sql (SqlPersistT)

-- | Run a migration with the Postgres backend.
runMigration :: MigrateSettings -> Migration -> SqlPersistT IO ()
runMigration = Migration.runMigration backend

-- | Get a migration with the Postgres backend.
getMigration :: MigrateSettings -> Migration -> SqlPersistT IO [MigrateSql]
getMigration = Migration.getMigration backend

-- | The migration backend for Postgres.
backend :: MigrateBackend
backend = MigrateBackend
  { getMigrationSql = getMigrationSql'
  }

getMigrationSql' :: Operation -> SqlPersistT IO [MigrateSql]

getMigrationSql' CreateTable{..} = fromMigrateSql $ mapSql
  (\sql -> Text.unwords ["CREATE TABLE IF NOT EXISTS", quote name, "(", sql, ")"])
  $ concatSql uncommas tableDefs
  where
    tableDefs = map showColumn schema ++ map showTableConstraint constraints

getMigrationSql' DropTable{..} = fromWords
  ["DROP TABLE IF EXISTS", quote table]

getMigrationSql' RenameTable{..} = fromWords
  ["ALTER TABLE", quote from, "RENAME TO", quote to]

getMigrationSql' AddConstraint{..} = fromWords
  ["ALTER TABLE", quote table, statement]
  where
    statement = case constraint of
      PrimaryKey cols -> Text.unwords ["ADD PRIMARY KEY (", uncommas' cols, ")"]
      Unique label cols -> Text.unwords
        ["ADD CONSTRAINT", quote label, "UNIQUE (", uncommas' cols, ")"]

getMigrationSql' DropConstraint{..} = fromWords
  ["ALTER TABLE", quote table, "DROP CONSTRAINT", constraintName]

getMigrationSql' AddColumn{..} = return $ createQuery : maybeToList alterQuery
  where
    Column{..} = column
    alterTable = Text.unwords ["ALTER TABLE", quote table]
    -- The CREATE query with the default specified by AddColumn{colDefault}
    withoutDefault = showColumn $ column { colProps = filter (not . isDefault) colProps }
    createDefault = case colDefault of
      Nothing -> MigrateSql "" []
      Just def -> MigrateSql "DEFAULT ?" [def]
    createQuery = concatSql
      (\sqls -> Text.unwords $ [alterTable, "ADD COLUMN"] ++ sqls)
      [withoutDefault, createDefault]
    -- The ALTER query to drop/set the default (if colDefault was set)
    alterQuery =
      let action = case getDefault colProps of
            Nothing -> pureSql "DROP DEFAULT"
            Just v -> MigrateSql "SET DEFAULT ?" [v]
          alterQuery' = mapSql
            (\sql -> Text.unwords [alterTable, "ALTER COLUMN", quote colName, sql])
            action
      in alterQuery' <$ colDefault

getMigrationSql' RenameColumn{..} = fromWords
  ["ALTER TABLE", quote table, "RENAME COLUMN", quote from, "TO", quote to]

getMigrationSql' DropColumn{..} = fromWords
  ["ALTER TABLE", quote tab, "DROP COLUMN", quote col]
  where
    (tab, col) = columnId

getMigrationSql' RawOperation{..} = rawOp

{- Helpers -}

fromMigrateSql :: Monad m => MigrateSql -> m [MigrateSql]
fromMigrateSql = return . pure

fromWords :: Monad m => [Text] -> m [MigrateSql]
fromWords = fromMigrateSql . pureSql . Text.unwords

-- | True if the given ColumnProp sets a default.
isDefault :: ColumnProp -> Bool
isDefault (Default _) = True
isDefault _ = False

-- | Get the default value from the given ColumnProps.
getDefault :: [ColumnProp] -> Maybe PersistValue
getDefault [] = Nothing
getDefault (Default v : _) = Just v
getDefault (_:props) = getDefault props

-- | Show a 'Column'.
showColumn :: Column -> MigrateSql
showColumn Column{..} = concatSql
  (\sqls -> Text.unwords $ [quote colName, sqlType] ++ sqls)
  $ map showColumnProp colProps
  where
    sqlType = case (AutoIncrement `elem` colProps, colType) of
      (True, SqlInt32) -> "SERIAL"
      (True, SqlInt64) -> "BIGSERIAL"
      _ -> showSqlType colType

-- | Show a 'SqlType'. See `showSqlType` from `Database.Persist.Postgresql`.
showSqlType :: SqlType -> Text
showSqlType = \case
  SqlString -> "VARCHAR"
  SqlInt32 -> "INT4"
  SqlInt64 -> "INT8"
  SqlReal -> "DOUBLE PRECISION"
  SqlNumeric s prec -> Text.concat ["NUMERIC(", showT s, ",", showT prec, ")"]
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
showColumnProp :: ColumnProp -> MigrateSql
showColumnProp = \case
  NotNull -> pureSql "NOT NULL"
  References (tab, col) -> pureSql $ Text.unwords
    ["REFERENCES", quote tab, "(", quote col, ")"]
  AutoIncrement -> pureSql ""
  Default v -> MigrateSql "DEFAULT ?" [v]

-- | Show a `TableConstraint`.
showTableConstraint :: TableConstraint -> MigrateSql
showTableConstraint = pureSql . \case
  PrimaryKey cols -> Text.unwords ["PRIMARY KEY (", uncommas' cols, ")"]
  Unique name cols -> Text.unwords ["CONSTRAINT", quote name, "UNIQUE (", uncommas' cols, ")"]
