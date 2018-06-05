{-|
Module      :  Database.Persist.Migration.Internal
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Database.Persist.Migration.Internal where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (mapReaderT)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
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

-- | The type class for data types that can be migrated.
class Show op => Migrateable op where
  -- | Get the SQL queries to run the migration.
  getMigrationText :: MigrateBackend -> op -> SqlPersistT IO [Text]

-- | Get the current version of the database, or Nothing if none exists.
getCurrVersion :: MonadIO m => MigrateBackend -> SqlPersistT m (Maybe Version)
getCurrVersion backend = do
  -- create the persistent_migration table if it doesn't already exist
  mapReaderT liftIO (createTable backend True migrationSchema) >>= rawExecute'
  extractVersion <$> rawSql queryVersion []
  where
    migrationSchema = CreateTable
      { ctName = "persistent_migration"
      , ctSchema =
          [ Column "id" SqlInt32 []
          , Column "version" SqlInt32 []
          , Column "timestamp" SqlDayTime []
          ]
      , ctConstraints =
          [ PrimaryKey ["id"]
          ]
      }
    queryVersion = "SELECT version FROM persistent_migration ORDER BY timestamp DESC LIMIT 1"
    extractVersion = \case
      [] -> Nothing
      [Single v] -> Just v
      _ -> error "Invalid response from the database."

-- | Get the migration plan given the current state of the database.
getMigratePlan :: Migration -> Maybe Version -> Migration
getMigratePlan migration mVersion = getPath edges start end
  where
    edges = map (\op@Operation{opPath} -> (opPath, op)) migration
    start = case mVersion of
      Nothing -> minimum . map (fst . opPath) $ migration
      Just v -> v
    end = getLatestVersion migration

-- | Get the most up-to-date version in the given migration.
getLatestVersion :: Migration -> Version
getLatestVersion = maximum . map (snd . opPath)

-- | Run the given migration. After successful completion, saves the migration to the database.
runMigration :: MonadIO m => MigrateBackend -> Migration -> SqlPersistT m ()
runMigration backend migration = do
  getMigration backend migration >>= rawExecute'
  now <- liftIO getCurrentTime
  rawExecute "INSERT INTO persistent_migration(version, timestamp) VALUES (?, ?)"
    [ PersistInt64 $ fromIntegral $ getLatestVersion migration
    , PersistUTCTime now
    ]

-- | Get the SQL queries for the given migration.
getMigration :: MonadIO m => MigrateBackend -> Migration -> SqlPersistT m [Text]
getMigration backend migration = do
  currVersion <- getCurrVersion backend
  let migratePlan = getMigratePlan migration currVersion
  concatMapM getMigrationText' migratePlan
  where
    -- Utilities
    concatMapM f = fmap concat . mapM f
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
