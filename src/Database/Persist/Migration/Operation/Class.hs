{-|
Module      :  Database.Persist.Migration.Operation.Class
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the Migrateable type class.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.Migration.Operation.Class (Migrateable(..)) where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Database.Persist.Migration.Backend (MigrateBackend(..))
import Database.Persist.Migration.Operation.Types
import Database.Persist.Migration.Utils.Data (hasDuplicateConstrs)
import Database.Persist.Sql (SqlPersistT)

-- | The type class for data types that can be migrated.
class Show op => Migrateable op where
  -- | Validate any checks for the given operation.
  validateOperation :: op -> Either String ()
  validateOperation _ = Right ()

  -- | Get the SQL queries to run the migration.
  getMigrationText :: MigrateBackend -> op -> SqlPersistT IO [Text]

instance Migrateable CreateTable where
  validateOperation ct@CreateTable{..} = do
    mapM_ validateColumn schema
    when (hasDuplicateConstrs constraints) $
      Left $ "Duplicate table constraints detected: " ++ show ct

    let constraintCols = concatMap getConstraintColumns constraints
        schemaCols = map colName schema
    when (any (`notElem` schemaCols) constraintCols) $
      Left $ "Table constraint references non-existent column: " ++ show ct

  getMigrationText backend = createTable backend False

instance Migrateable DropTable where
  getMigrationText = dropTable

instance Migrateable RenameTable where
  getMigrationText = renameTable

instance Migrateable AddConstraint where
  getMigrationText = addConstraint

instance Migrateable DropConstraint where
  getMigrationText = dropConstraint

instance Migrateable AddColumn where
  validateOperation ac@AddColumn{..} = do
    validateColumn column
    when (NotNull `elem` colProps column && isNothing colDefault) $
      Left $ "Adding a non-nullable column requires a default: " ++ show ac

  getMigrationText = addColumn

instance Migrateable DropColumn where
  getMigrationText = dropColumn

instance Migrateable RawOperation where
  getMigrationText _ RawOperation{rawOp} = rawOp

instance Migrateable NoOp where
  getMigrationText _ _ = return []
