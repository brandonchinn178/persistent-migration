{-|
Module      :  Database.Persist.Migration.Internal
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Database.Persist.Migration.Internal where

import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Database.Persist.Class (BackendCompatible)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Types (SqlType(..))

{- Operation types -}

-- | The ID of an operation. Should be unique and not change, ever.
type OperationId = Int

-- | An operation that can be migrated.
data Operation =
  forall m. Migrateable m =>
  Operation
    { opId :: OperationId
    , opOp :: m
    }

-- | An operation nested within another operation.
data SubOperation = forall m. Migrateable m => SubOperation m

-- | Create an operation from a suboperation.
fromSub :: OperationId -> SubOperation -> Operation
fromSub opId (SubOperation opOp) = Operation{..}

{- Migration types -}

-- | A migration is simply a list of operations.
type Migration = [Operation]

-- | A list of operations containing a Bool that should be False to make the operation a noop.
type MigrationInfo = [(Bool, Operation)]

-- | The type for the underlying operation.
--
-- RawOperations should primarily use 'rawSql' and 'rawExecute' from the persistent library. If the
-- operation depends on the backend being run, query 'connRDBMS' from the 'SqlBackend':
--
-- @
-- asks connRDBMS >>= \case
--   "sqlite" -> ...
--   _ -> return ()
-- @
type MigrateT m a = forall backend. BackendCompatible SqlBackend backend => ReaderT backend m a

-- | The backend to migrate with.
data MigrateBackend = MigrateBackend
  { createTable :: CreateTable -> MigrateT IO [Text]
  , dropTable :: DropTable -> MigrateT IO [Text]
  , addColumn :: AddColumn -> MigrateT IO [Text]
  , dropColumn :: DropColumn -> MigrateT IO [Text]
  }

class Migrateable m where
  -- | Get the SQL queries to run the migration.
  getMigrationText :: MigrateBackend -> m -> MigrateT IO [Text]

  -- | Modify the list of pending operations prior to migrating.
  modifyMigration :: OperationId -> m -> MigrationInfo -> MigrationInfo
  modifyMigration _ _ = id

{- Core Operations -}

-- | An operation to create a table according to the specified schema.
data CreateTable = CreateTable
  { ctName :: Text
  , ctSchema :: [Column]
  , ctConstraints :: [TableConstraint]
  }

instance Migrateable CreateTable where
  getMigrationText = createTable

-- | An operation to drop the given table.
data DropTable = DropTable Text

instance Migrateable DropTable where
  getMigrationText = dropTable

-- | An operation to add the given column to an existing table.
data AddColumn = AddColumn
  { acTable :: Text
  , acColumn :: Column
  , acDefault :: Maybe Text
    -- ^ if the column is non-nullable and doesn't have a default, need to define a default for
    -- existing rows.
  }

instance Migrateable AddColumn where
  getMigrationText = addColumn

-- | An operation to drop the given column to an existing table.
data DropColumn = DropColumn
  { dcTable :: Text
  , dcColumn :: Text
  }

instance Migrateable DropColumn where
  getMigrationText = dropColumn

-- | A custom operation that can be defined manually.
newtype RawOperation = RawOperation (MigrateT IO [Text])

instance Migrateable RawOperation where
  getMigrationText _ (RawOperation op) = op

-- | A noop operation.
data NoOp = NoOp

instance Migrateable NoOp where
  getMigrationText _ NoOp = return []

{- Nested operations -}

-- | If the given OperationId has not been run, don't run it. Otherwise, run the given operations.
--
-- e.g. given:
-- @
-- migrations =
--   [ Operation 0 $ CreateTable "person" ...
--   , Operation 1 $ DropColumn "person" "name"
--   , Operation 2 $ Revert 1
--       [ SubOperation $ AddColumn "person" (Column "name" ...) ...
--       ]
--   ]
-- @
--
-- * Someone migrating an empty database will only run operation 0
-- * Someone who only ran 0 will not run anything
-- * Someone who ran 0 and 1 will run the AddColumn operation in 2
data Revert = Revert OperationId [SubOperation]

instance Migrateable Revert where
  getMigrationText backend (Revert _ (Operation _ op)) = getMigrationText backend op

  modifyMigration newId (Revert oldId _) migrations =
    if any ((== oldId) . opId . snd) migrations
      then setNoop [oldId, newId] migrations
      else migrations

-- | If none of the given OperationIds have been run, run the given operations instead.
--
-- e.g. given:
-- @
-- migrations =
--   [ Operation 0 $ CreateTable "person" ...
--   , Operation 1 $ AddColumn "person" (Column "height" ...) ...
--   , Operation 2 $ DropColumn "person" "height"
--   , Operation 3 $ Squash [1,2] []
--   ]
-- @
--
-- * Someone migrating an empty database will run operations 0 and 3
-- * Someone who only ran 0 will only run 3
-- * Someone who ran up to 1 will run 2, but not 3
-- * Someone who ran up to 2 will not run anything
data Squash = Squash [OperationId] [SubOperation]

instance Migrateable Squash where
  getMigrationText backend (Squash _ ops) = fmap concat . mapM helper $ ops
    where
      helper (Operation _ op) = getMigrationText backend op

  modifyMigration newId (Squash ids _) migrations =
    if all (`elem` map (opId . snd) migrations) ids
      then setNoop [newId] migrations
      else setNoop ids migrations

{- Auxiliary types -}

-- | The definition for a Column in a SQL database.
data Column = Column
  { colName :: Text
  , colType :: SqlType
  , colProps :: [ColumnProp]
  }

-- | A property for a 'Column'.
data ColumnProp
  = Nullable -- ^ Makes a 'Column' nullable (defaults to non-nullable)
  | Defaults Text -- ^ Set the default for inserted rows without a value specified for the column
  | ForeignKey (Text, Text) -- ^ Mark this column as a foreign key to the given table.column

-- | Table constraints in a CREATE query.
data TableConstraint
  = PrimaryKey [Text] -- ^ PRIMARY KEY (col1, col2, ...)
  | Unique [Text] -- ^ UNIQUE (col1, col2, ...)
