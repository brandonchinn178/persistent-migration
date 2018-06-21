{-|
Module      :  Database.Persist.Migration.Core
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines a migration framework for the persistent library.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Database.Persist.Migration.Core
  ( Migration
  , MigrateSettings(..)
  , defaultSettings
  , validateMigration
  , runMigration
  , getMigration
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (mapReaderT)
import Data.Data (Data)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Migration.Backend (MigrateBackend(..))
import Database.Persist.Migration.Operation (Operation(..), Version)
import Database.Persist.Migration.Operation.Class (Migrateable(..))
import Database.Persist.Migration.Operation.Types
    (Column(..), ColumnProp(..), CreateTable(..), TableConstraint(..))
import Database.Persist.Migration.Utils.Data (hasDuplicateConstrs)
import Database.Persist.Migration.Utils.Plan (getPath)
import Database.Persist.Sql
    (PersistValue(..), Single(..), SqlPersistT, rawExecute, rawSql)
import Database.Persist.Types (SqlType(..))

-- | A migration is simply a list of operations.
type Migration = [Operation]

-- | Get the current version of the database, or Nothing if none exists.
getCurrVersion :: MonadIO m => MigrateBackend -> SqlPersistT m (Maybe Version)
getCurrVersion backend = do
  -- create the persistent_migration table if it doesn't already exist
  mapReaderT liftIO (createTable backend True migrationSchema) >>= rawExecute'
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

-- | Get the migration plan given the current state of the database.
getMigratePlan :: Migration -> Maybe Version -> Either (Version, Version) Migration
getMigratePlan migration mVersion = case getPath edges start end of
  Just path -> Right path
  Nothing -> Left (start, end)
  where
    edges = map (\op@Operation{opPath} -> (opPath, op)) migration
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
  getMigration backend settings migration >>= rawExecute'
  now <- liftIO getCurrentTime
  let version = getLatestVersion migration
  rawExecute "INSERT INTO persistent_migration(version, label, timestamp) VALUES (?, ?, ?)"
    [ PersistInt64 $ fromIntegral version
    , PersistText $ Text.pack $ fromMaybe (show version) $ versionToLabel version
    , PersistUTCTime now
    ]

-- | Get the SQL queries for the given migration.
getMigration :: MonadIO m => MigrateBackend -> MigrateSettings -> Migration -> SqlPersistT m [Text]
getMigration backend _ migration = do
  either fail return $ validateMigration migration
  either fail return $ mapM_ (\Operation{opOp} -> validateOperation opOp) migration
  currVersion <- getCurrVersion backend
  migratePlan <- either badPath return $ getMigratePlan migration currVersion
  concatMapM getMigrationText' migratePlan
  where
    badPath (start, end) = fail $ "Could not find path: " ++ show start ++ " ~> " ++ show end
    -- Utilities
    concatMapM f = fmap concat . mapM f
    -- Operation helpers
    getMigrationText' Operation{opOp} = mapReaderT liftIO $ getMigrationText backend opOp

-- | Execute the given SQL strings.
rawExecute' :: MonadIO m => [Text] -> SqlPersistT m ()
rawExecute' = mapM_ $ \s -> rawExecute s []
