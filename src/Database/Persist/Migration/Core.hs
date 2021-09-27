{-|
Module      :  Database.Persist.Migration.Core
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Database.Persist.Migration.Core
  ( Version
  , OperationPath
  , (~>)
  , Migration
  , MigrationPath(..)
  , opPath
  , MigrateSettings(..)
  , defaultSettings
  , validateMigration
  , runMigration
  , getMigration
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (mapReaderT)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Migration.Backend (MigrateBackend(..))
import Database.Persist.Migration.Operation (Operation(..), validateOperation)
import Database.Persist.Migration.Operation.Types
    (Column(..), ColumnProp(..), TableConstraint(..))
import Database.Persist.Migration.Utils.Plan (getPath)
import Database.Persist.Migration.Utils.Sql (MigrateSql, executeSql)
import Database.Persist.Sql
    (PersistValue(..), Single(..), SqlPersistT, rawExecute, rawSql)
import Database.Persist.Types (SqlType(..))

-- | The version of a database. An operation migrates from the given version to another version.
--
-- The version must be increasing, such that the lowest version is the first version and the highest
-- version is the most up-to-date version.
--
-- A version represents a version of the database schema. In other words, any set of operations
-- taken to get to version X *MUST* all result in the same database schema.
type Version = Int

-- | The path that an operation takes.
type OperationPath = (Version, Version)

-- | An infix constructor for 'OperationPath'.
(~>) :: Version -> Version -> OperationPath
(~>) = (,)

-- | A migration list that defines operations to manually migrate a database schema.
type Migration = [MigrationPath]

-- | A path representing the operations needed to run to get from one version of the database schema
-- to the next.
data MigrationPath = OperationPath := [Operation]
  deriving (Show)

-- | Get the OperationPath in the MigrationPath.
opPath :: MigrationPath -> OperationPath
opPath (path := _) = path

-- | Get the current version of the database, or Nothing if none exists.
getCurrVersion :: MonadIO m => MigrateBackend -> SqlPersistT m (Maybe Version)
getCurrVersion backend = do
  -- create the persistent_migration table if it doesn't already exist
  mapReaderT liftIO (getMigrationSql backend migrationSchema) >>= mapM_ executeSql
  extractVersion <$> rawSql queryVersion []
  where
    migrationSchema = CreateTable
      { name = "persistent_migration"
      , schema =
          [ Column "id" SqlInt32 [NotNull, AutoIncrement]
          , Column "version" SqlInt32 [NotNull]
          , Column "label" SqlString []
          , Column "timestamp" SqlDayTime [NotNull]
          ]
      , constraints =
          [ PrimaryKey ["id"]
          ]
      }
    queryVersion = "SELECT version FROM persistent_migration ORDER BY timestamp DESC LIMIT 1"
    extractVersion = \case
      [] -> Nothing
      [Single v] -> Just v
      _ -> error "Invalid response from the database."

-- | Get the list of operations to run, given the current state of the database.
getOperations :: Migration -> Maybe Version -> Either (Version, Version) [Operation]
getOperations migration mVersion = case getPath edges start end of
  Just path -> Right $ concat path
  Nothing -> Left (start, end)
  where
    edges = map (\(path := ops) -> (path, ops)) migration
    start = fromMaybe (getFirstVersion migration) mVersion
    end = getLatestVersion migration

-- | Get the first version in the given migration.
getFirstVersion :: Migration -> Version
getFirstVersion = minimum . map (fst . opPath)

-- | Get the most up-to-date version in the given migration.
getLatestVersion :: Migration -> Version
getLatestVersion = maximum . map (snd . opPath)

{- Migration plan and execution -}

-- | Settings to customize migration steps.
newtype MigrateSettings = MigrateSettings
  { versionToLabel :: Version -> Maybe String
      -- ^ A function to optionally label certain versions
  }

-- | Default migration settings.
defaultSettings :: MigrateSettings
defaultSettings = MigrateSettings
  { versionToLabel = const Nothing
  }

-- | Validate the given migration.
validateMigration :: Migration -> Either String ()
validateMigration migration = do
  unless (allIncreasing opVersions) $
    Left "Operation versions must be monotonically increasing"
  when (hasDuplicates opVersions) $
    Left "There may only be one operation per pair of versions"
  where
    opVersions = map opPath migration
    allIncreasing = all (uncurry (<))
    hasDuplicates l = length (nub l) < length l

-- | Run the given migration. After successful completion, saves the migration to the database.
runMigration :: MonadIO m => MigrateBackend -> MigrateSettings -> Migration -> SqlPersistT m ()
runMigration backend settings@MigrateSettings{..} migration = do
  currVersion <- getCurrVersion backend
  let latestVersion = getLatestVersion migration
  case currVersion of
    Just current | current >= latestVersion -> pure ()
    _ -> do
      getMigration backend settings migration >>= mapM_ executeSql
      now <- liftIO getCurrentTime
      rawExecute "INSERT INTO persistent_migration(version, label, timestamp) VALUES (?, ?, ?)"
        [ PersistInt64 $ fromIntegral latestVersion
        , PersistText $ Text.pack $ fromMaybe (show latestVersion) $ versionToLabel latestVersion
        , PersistUTCTime now
        ]

-- | Get the SQL queries for the given migration.
getMigration :: MonadIO m
  => MigrateBackend
  -> MigrateSettings
  -> Migration
  -> SqlPersistT m [MigrateSql]
getMigration backend _ migration = do
  either error return $ validateMigration migration
  currVersion <- getCurrVersion backend
  operations <- either badPath return $ getOperations migration currVersion
  either error return $ mapM_ validateOperation operations
  concatMapM (mapReaderT liftIO . getMigrationSql backend) operations
  where
    badPath (start, end) = error $ "Could not find path: " ++ show start ++ " ~> " ++ show end
    -- Utilities
    concatMapM f = fmap concat . mapM f
