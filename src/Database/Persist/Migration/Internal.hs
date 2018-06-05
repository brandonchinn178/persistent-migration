{-|
Module      :  Database.Persist.Migration.Internal
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Database.Persist.Migration.Internal where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (mapReaderT)
import Data.List (nub)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Migration.Plan (getPath)
import Database.Persist.Sql
    (PersistValue(..), Single(..), SqlPersistT, rawExecute, rawSql)
import Database.Persist.Types (SqlType(..))

{- Operation types -}

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

{- Migration types -}

-- | A migration is simply a list of operations.
type Migration = [Operation]

-- | The backend to migrate with.
data MigrateBackend = MigrateBackend
  { createTable :: Bool -> CreateTable -> SqlPersistT IO [Text]
      -- ^ create a table (True = IF NOT EXISTS)
  , dropTable   :: DropTable -> SqlPersistT IO [Text]
  , addColumn   :: AddColumn -> SqlPersistT IO [Text]
  , dropColumn  :: DropColumn -> SqlPersistT IO [Text]
  }

-- | An action for an operation in a MigratePlan.
data MigrateAction
  = DONE -- ^ The operation was already done in a previous migration
  | RUN  -- ^ The operation should be run in this migration
  deriving (Eq,Show)

type OperationPlan = (MigrateAction, Operation)

-- | The tentative plan for migration.
data MigratePlan = MigratePlan
  { original :: Migration -- ^ The original, full migration defined by the user
  , plan     :: [OperationPlan] -- ^ The tentative list of operations
  }

-- | The type class for data types that can be migrated.
class Show op => Migrateable op where
  -- | Get the SQL queries to run the migration.
  getMigrationText :: MigrateBackend -> op -> SqlPersistT IO [Text]

-- | Get all operations whose MigrateAction matches the given predicate.
getOpsWith :: (MigrateAction -> Bool) -> [OperationPlan] -> [Operation]
getOpsWith f = map snd . filter (f . fst)

-- | Get the migrate plan for the given migration.
getMigratePlan :: MonadIO m => MigrateBackend -> Migration -> SqlPersistT m MigratePlan
getMigratePlan backend migration = do
  -- create the persistent_migration table if it doesn't already exist
  rawExecute' =<< mapReaderT liftIO (createTable backend True migrationSchema)

  -- get all of the operation IDs in the database
  doneOps <- map unSingle <$> rawSql "SELECT opId FROM persistent_migration" []
  let opPlans = map (\op -> if opId op `elem` doneOps then (DONE, op) else (RUN, op)) migration

  return
    MigratePlan
      { original = migration
      , plan = opPlans
      }
  where
    migrationSchema = CreateTable
      { ctName = "persistent_migration"
      , ctSchema =
          [ Column "id" SqlInt32 []
          , Column "opId" SqlInt32 []
          , Column "operation" SqlString []
          ]
      , ctConstraints =
          [ PrimaryKey ["id"]
          ]
      }

-- | Run the given migration. After successful completion, saves the migration to the database.
runMigration :: MonadIO m => MigrateBackend -> Migration -> SqlPersistT m ()
runMigration backend migration = do
  migratePlan <- getMigratePlan backend migration
  getMigration' backend migration migratePlan >>= rawExecute'
  forM_ (getOpsWith (/= DONE) $ plan migratePlan) $ \Operation{..} ->
    rawExecute "INSERT INTO persistent_migration(opId, operation) VALUES (?, ?)"
      [ PersistInt64 $ fromIntegral opId
      , PersistText $ Text.pack $ show opOp
      ]

-- | Get the SQL queries for the given migration.
getMigration :: MonadIO m => MigrateBackend -> Migration -> SqlPersistT m [Text]
getMigration backend migration =
  getMigratePlan backend migration >>= getMigration' backend migration

-- | Get the migration for the given MigrateBackend.
getMigration' :: MonadIO m => MigrateBackend -> Migration -> MigratePlan -> SqlPersistT m [Text]
getMigration' backend migration migratePlan = do
  when (hasDuplicates $ map opId migration) $
    fail "Migration has multiple operations with the same ID"
  concatMapM getMigrationText' . fromMigratePlan $ migratePlan
  where
    -- Utilities
    hasDuplicates l = length (nub l) /= length l
    concatMapM f = fmap concat . mapM f
    -- MigratePlan helpers
    fromMigratePlan = getOpsWith (== RUN) . plan
    -- Operation helpers
    getMigrationText' Operation{opOp} = mapReaderT liftIO $ getMigrationText backend opOp

-- | Execute the given SQL strings.
rawExecute' :: MonadIO m => [Text] -> SqlPersistT m ()
rawExecute' = mapM_ $ \s -> rawExecute s []

{- Core Operations -}

-- | An operation to create a table according to the specified schema.
data CreateTable = CreateTable
  { ctName        :: Text
  , ctSchema      :: [Column]
  , ctConstraints :: [TableConstraint]
  } deriving (Show)

instance Migrateable CreateTable where
  getMigrationText backend = createTable backend False

-- | An operation to drop the given table.
newtype DropTable = DropTable
  { dtName :: Text
  }
  deriving (Show)

instance Migrateable DropTable where
  getMigrationText = dropTable

-- | An operation to add the given column to an existing table.
data AddColumn = AddColumn
  { acTable   :: Text
  , acColumn  :: Column
  , acDefault :: Maybe Text
    -- ^ if the column is non-nullable and doesn't have a default, need to define a default for
    -- existing rows.
  } deriving (Show)

instance Migrateable AddColumn where
  getMigrationText backend ac@AddColumn{..} = do
    when (isNotNullAndNoDefault acColumn && isNothing acDefault) $
      fail $ Text.unpack $ "A non-nullable column requires a default: " <> colName acColumn
    addColumn backend ac

-- | An operation to drop the given column to an existing table.
newtype DropColumn = DropColumn
  { dcColumn :: ColumnIdentifier
  } deriving (Show)

instance Migrateable DropColumn where
  getMigrationText = dropColumn

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

instance Migrateable RawOperation where
  getMigrationText _ RawOperation{rawOp} = rawOp

-- | A noop operation.
data NoOp = NoOp
  deriving (Show)

instance Migrateable NoOp where
  getMigrationText _ _ = return []

{- Auxiliary types -}

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

-- | Return whether the given column is NOT NULL and doesn't have a default specified.
isNotNullAndNoDefault :: Column -> Bool
isNotNullAndNoDefault Column{..} = NotNull `elem` colProps && any isDefault colProps
  where
    isDefault (Defaults _) = True
    isDefault _ = False

-- | A property for a 'Column'.
data ColumnProp
  = NotNull -- ^ Makes a 'Column' non-nullable (defaults to nullable)
  | Defaults Text -- ^ Set the default for inserted rows without a value specified for the column
  | ForeignKey ColumnIdentifier -- ^ Mark this column as a foreign key to the given column
  deriving (Show,Eq)

-- | Table constraints in a CREATE query.
data TableConstraint
  = PrimaryKey [Text] -- ^ PRIMARY KEY (col1, col2, ...)
  | Unique [Text] -- ^ UNIQUE (col1, col2, ...)
  deriving (Show)
