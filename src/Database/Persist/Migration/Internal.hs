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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Database.Persist.Migration.Internal where

import Control.Monad (when)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (SqlPersistT, rawExecute)
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

deriving instance Show Operation

-- | An operation nested within another operation.
data SubOperation = forall m. Migrateable m => SubOperation m

deriving instance Show SubOperation

{- Migration types -}

-- | A migration is simply a list of operations.
type Migration = [Operation]

-- | The backend to migrate with.
data MigrateBackend = MigrateBackend
  { setupMigrations :: SqlPersistT IO ()
      -- ^ set up the migration table if it hasn't been already
  , getCompletedOps :: SqlPersistT IO [OperationId]
      -- ^ get a list of operation IDs that have already been completed
  , saveMigration :: Migration -> SqlPersistT IO ()
      -- ^ save the operations in the given migration to the database as completed
  , createTable :: CreateTable -> SqlPersistT IO [Text]
  , dropTable :: DropTable -> SqlPersistT IO [Text]
  , addColumn :: AddColumn -> SqlPersistT IO [Text]
  , dropColumn :: DropColumn -> SqlPersistT IO [Text]
  }

-- | An action for an operation in a MigratePlan.
data MigrateAction
  = DONE     -- ^ The operation was already done in a previous migration
  | RUN      -- ^ The operation should be run in this migration
  | REVERTED -- ^ The operation was reverted by another operation
  | SQUASHED -- ^ The operation was squashed by another operation
  deriving (Eq,Show)

type OperationPlan = (MigrateAction, Operation)

-- | The tentative plan for migration.
data MigratePlan = MigratePlan
  { original :: Migration -- ^ The original, full migration defined by the user
  , plan :: [OperationPlan] -- ^ The tentative list of operations
  , todo :: [Operation] -- ^ The unprocessed list of operations to add to plan
  }

-- | The type class for data types that can be migrated.
class Show m => Migrateable m where
  -- | Get the SQL queries to run the migration.
  getMigrationText :: MigrateBackend -> m -> SqlPersistT IO [Text]

  -- | Given the tentative migration and the operations left to process, return the updated
  -- tentative migration and the possibly modified todo list.
  modifyMigration :: m -> Operation -> MigratePlan -> MigratePlan
  modifyMigration _ _ = id

-- | Modify the migration according to the given operation.
modifyMigration' :: Operation -> MigratePlan -> MigratePlan
modifyMigration' op@Operation{opOp} = modifyMigration opOp op

-- | Run the given migration. After successful completion, saves the migration to the database.
runMigration :: MigrateBackend -> Migration -> SqlPersistT IO ()
runMigration backend migration = do
  migrateQueries <- getMigration backend migration
  mapM_ (\s -> rawExecute s []) migrateQueries
  saveMigration backend migration

-- | Get the SQL queries for the given migration.
getMigration :: MigrateBackend -> Migration -> SqlPersistT IO [Text]
getMigration backend migration = do
  when (hasDuplicates $ map opId migration) $
    fail "Migration has multiple operations with the same ID"
  setupMigrations backend
  doneOps <- getCompletedOps backend
  let migratePlan = MigratePlan
        { original = migration
        , plan = map (\op -> if opId op `elem` doneOps then (DONE, op) else (RUN, op)) migration
        , todo = migration
        }
  concatMapM getMigrationText' . fromMigratePlan . finalizeMigratePlan $ migratePlan
  where
    -- Utilities
    hasDuplicates l = length (nub l) /= length l
    concatMapM f = fmap concat . mapM f
    -- MigratePlan helpers
    finalizeMigratePlan migratePlan =
      case todo migratePlan of
        [] -> migratePlan
        (op:todo') -> finalizeMigratePlan $ modifyMigration' op migratePlan{todo = todo'}
    fromMigratePlan = map snd . filter ((== RUN) . fst) . plan
    -- Operation helpers
    getMigrationText' Operation{opOp} = getMigrationText backend opOp

{- Core Operations -}

-- | An operation to create a table according to the specified schema.
data CreateTable = CreateTable
  { ctName :: Text
  , ctSchema :: [Column]
  , ctConstraints :: [TableConstraint]
  } deriving (Show)

instance Migrateable CreateTable where
  getMigrationText = createTable

-- | An operation to drop the given table.
newtype DropTable = DropTable
  { dtName :: Text
  }
  deriving (Show)

instance Migrateable DropTable where
  getMigrationText = dropTable

-- | An operation to add the given column to an existing table.
data AddColumn = AddColumn
  { acTable :: Text
  , acColumn :: Column
  , acDefault :: Maybe Text
    -- ^ if the column is non-nullable and doesn't have a default, need to define a default for
    -- existing rows.
  } deriving (Show)

instance Migrateable AddColumn where
  getMigrationText = addColumn

-- | An operation to drop the given column to an existing table.
data DropColumn = DropColumn
  { dcTable :: Text
  , dcColumn :: Text
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

{- Nested operations -}

-- | Check if the given operation is not DONE and the id matches the given id.
isPlannedOp :: OperationId -> (MigrateAction, Operation) -> Bool
isPlannedOp opId' (action, Operation{opId}) = action /= DONE && opId == opId'

-- | Set all operations with the given ID to the given MigrateAction.
setAction :: MigrateAction -> [OperationId] -> [OperationPlan] -> [OperationPlan]
setAction action opIds = map setAction'
  where
    setAction' opPlan@(_, op@Operation{opId}) = if opId `elem` opIds then (action, op) else opPlan

-- | Insert the given suboperations into the plan and todo list for the given operation ID.
insertOperations :: OperationId -> [SubOperation] -> MigratePlan -> MigratePlan
insertOperations opId' subOps mp = mp
  { plan = concatMap expandOp (plan mp)
  , todo = newOps ++ todo mp
  }
  where
    newOps = map (\(SubOperation op) -> Operation opId' op) subOps
    expandOp item@(_, Operation{opId}) = item : if opId == opId' then map (RUN,) newOps else []

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
  deriving (Show)

instance Migrateable Revert where
  getMigrationText _ _ = return []

  modifyMigration (Revert oldId ops) Operation{opId} mp@MigratePlan{plan} =
    if any (isPlannedOp oldId) plan
      then mp{plan = setAction REVERTED [oldId] plan}
      else insertOperations opId ops mp

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
  deriving (Show)

instance Migrateable Squash where
  getMigrationText _ _ = return []

  modifyMigration (Squash oldIds ops) Operation{opId} mp@MigratePlan{plan} =
    if all isPlanned oldIds
      then insertOperations opId ops mp{plan = setAction SQUASHED oldIds plan}
      else mp
    where
      isPlanned oldId = any (isPlannedOp oldId) plan

{- Auxiliary types -}

-- | The definition for a Column in a SQL database.
data Column = Column
  { colName :: Text
  , colType :: SqlType
  , colProps :: [ColumnProp]
  } deriving (Show)

-- | A property for a 'Column'.
data ColumnProp
  = Nullable -- ^ Makes a 'Column' nullable (defaults to non-nullable)
  | Defaults Text -- ^ Set the default for inserted rows without a value specified for the column
  | ForeignKey (Text, Text) -- ^ Mark this column as a foreign key to the given table.column
  deriving (Show)

-- | Table constraints in a CREATE query.
data TableConstraint
  = PrimaryKey [Text] -- ^ PRIMARY KEY (col1, col2, ...)
  | Unique [Text] -- ^ UNIQUE (col1, col2, ...)
  deriving (Show)
