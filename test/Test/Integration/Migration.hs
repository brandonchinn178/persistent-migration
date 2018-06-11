{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Test.Integration.Migration (testIntegration) where

import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Maybe (mapMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.Yaml (array, encode, object, (.=))
import Database.Persist.Migration
import Database.Persist.Migration.Sql (interpolate)
import Database.Persist (Entity(..), insert, insertMany_, selectList)
import Database.Persist.Sql
    (PersistValue(..), Single(..), SqlBackend, SqlPersistT, SqlType(..), rawExecute, rawSql)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Test.Tasty (TestTree, testGroup)
import Test.Utils.Goldens (goldenVsString)

{- Schema and migration -}

share [mkPersist sqlSettings, mkMigrate "autoMigration"] [persistLowerCase|
Person
  name String
  hometown CityId
  gender String Maybe
  colorblind Bool
  deriving Show
City
  name String
  state String
  UniqueCity name state
  deriving Show
|]

manualMigration :: Migration
manualMigration =
  -- create tables
  [ Operation (0 ~> 1) $
      CreateTable
        { ctName = "city"
        , ctSchema =
            [ Column "id" SqlInt64 [NotNull, AutoIncrement]
            , Column "name" SqlString [NotNull]
            , Column "state" SqlString [NotNull]
            ]
        , ctConstraints =
            [ PrimaryKey ["id"]
            , Unique "unique_city" ["state", "name"]
            ]
        }
  , Operation (1 ~> 2) $
      CreateTable
        { ctName = "person"
        , ctSchema =
            [ Column "id" SqlInt64 [NotNull, AutoIncrement]
            , Column "name" SqlString [NotNull]
            , Column "hometown" SqlInt64 [NotNull, References ("city", "id")]
            ]
        , ctConstraints =
            [ PrimaryKey ["id"]
            ]
        }

  -- add binary sex column
  , Operation (2 ~> 3) $ AddColumn "person" (Column "sex" SqlInt32 []) Nothing

  -- change binary sex to stringly gender
  , Operation (3 ~> 4) $ AddColumn "person" (Column "gender" SqlString []) Nothing
  , Operation (4 ~> 5) $ migrateGender
  , Operation (5 ~> 6) $ DropColumn ("person", "sex")
  -- shortcut for databases that hadn't already added the sex column
  , Operation (2 ~> 6) $ AddColumn "person" (Column "gender" SqlString []) Nothing

  -- add colorblind column, with everyone currently in the database being not colorblind
  , Operation (6 ~> 7) $
      AddColumn "person" (Column "colorblind" SqlBool [NotNull]) (Just "FALSE")
  ]
  where
    migrateGender = RawOperation "Convert binary sex column into stringly gender column" $
      mapMaybe migrateGender' <$> rawSql "SELECT id, sex FROM person" []
    migrateGender' = \case
      (_, Single Nothing) -> Nothing
      (Single id', Single (Just sex)) ->
        Just $ interpolate "UPDATE person SET gender = ? WHERE id = ?"
          [ PersistText $ sexToGender sex
          , PersistInt64 id'
          ]
    sexToGender :: Int -> Text
    sexToGender sex = if sex == 0 then "Male" else "Female"

{- Test suite -}

-- | Build a test suite running integration tests for the given MigrateBackend.
testIntegration :: String -> MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testIntegration label backend getPool = testGroup label
  [ testMigration' 0 []
  ]
  where
    testMigration' = testMigration label backend getPool

{- Helpers -}

-- | Run the given migration.
runMigration' :: MigrateBackend -> Pool SqlBackend -> Migration -> IO ()
runMigration' backend pool = runSql pool . runMigration backend defaultSettings

-- | Run the given persistent query.
runSql :: Pool SqlBackend -> SqlPersistT IO a -> IO a
runSql pool = withResource pool . runReaderT

-- | Run a test where:
--    * the first N operations have been migrated
--    * the given query is run to populate the database
--    * the remaining operations are migrated
--    * insert some data into the database
--    * output "SELECT * FROM person" to goldens file
--    * clean up database
testMigration
  :: String
  -> MigrateBackend
  -> IO (Pool SqlBackend)
  -> Int
  -> [SqlPersistT IO ()]
  -> TestTree
testMigration label backend getPool n populateDb = goldenVsString "integration" label name $ do
  pool <- getPool
  let doMigration = runMigration' backend pool

  -- test setup
  unless (null setupMigration) $ doMigration setupMigration
  mapM_ (runSql pool) populateDb

  -- run migrations and check inserting current models works
  doMigration manualMigration
  res <- runSql pool $ do
    checkMigration autoMigration
    berkeley <- insert $ City "Berkeley" "CA"
    insertMany_
      [ Person "Alice" berkeley (Just "Female") False
      , Person "Bob" berkeley (Just "Male") True
      , Person "Courtney" berkeley Nothing False
      ]
    map entityVal <$> selectList [] []

  -- test cleanup
  runSql pool $ rawExecute "DROP TABLE person" []
  runSql pool $ rawExecute "DROP TABLE city" []

  return $ showPersons res
  where
    setupMigration = take n manualMigration
    name = "Migrate from " ++ show n

-- | Display a Person as a YAML object.
showPersons :: [Person] -> ByteString
showPersons = fromStrict . encode . array . map showPerson
  where
    showPerson Person{..} = object
      [ "name" .= personName
      , "hometown" .= personHometown
      , "gender" .= personGender
      , "colorblind" .= personColorblind
      ]
