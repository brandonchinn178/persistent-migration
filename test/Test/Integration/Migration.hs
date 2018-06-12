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

module Test.Integration.Migration (testMigrations) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Yaml (array, encode, object, (.=))
import Database.Persist (Entity(..), get, insertKey, insertMany_, selectList)
import Database.Persist.Migration
    ( AddColumn(..)
    , Column(..)
    , ColumnProp(..)
    , CreateTable(..)
    , DropColumn(..)
    , MigrateBackend
    , Migration
    , Operation(..)
    , RawOperation(..)
    , TableConstraint(..)
    , checkMigration
    , hasMigration
    , (~>)
    )
import Database.Persist.Migration.Sql (interpolate, uncommas)
import Database.Persist.Sql
    ( PersistValue(..)
    , Single(..)
    , SqlBackend
    , SqlPersistT
    , SqlType(..)
    , rawExecute
    , rawSql
    )
import Database.Persist.TH
    (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Test.Integration.Utils.RunSql (runMigration, runSql)
import Test.Tasty (TestTree, testGroup)
import Test.Utils.Goldens (TestGoldenString)

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
  , Operation (4 ~> 5) migrateGender
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

-- | A test suite for running migrations.
testMigrations :: TestGoldenString -> MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testMigrations testGolden backend getPool = testGroup "migrations"
  [ testMigration' "Migrate from empty" 0 []
  , testMigration' "Migrate after CREATE city" 1 []
  , testMigration' "Migrate with v1 person" 2 [insertPerson "David" []]
  , testMigration' "Migrate from sex to gender" 3
      [ insertPerson "David" [("sex", "0")]
      , insertPerson "Elizabeth" [("sex", "1")]
      , insertPerson "Foster" [("sex", "NULL")]
      ]
  , testMigration' "Migrate with default colorblind" 7 [insertPerson "David" []]
  , testMigration' "Migrations are idempotent" 8 [insertPerson "David" [("colorblind", "TRUE")]]
  ]
  where
    testMigration' = testMigration testGolden backend getPool
    insertPerson name extra =
      let cols = ["name", "hometown"] ++ map fst extra
          vals = ["'" <> name <> "'", "1"] ++ map snd extra
      in rawExecute
          ("INSERT INTO person(" <> uncommas cols <> ") VALUES (" <> uncommas vals <> ")")
          []

-- | Run a test where:
--    * the first N operations have been migrated
--    * the given query is run to populate the database
--    * the remaining operations are migrated
--    * insert some data into the database
--    * output "SELECT * FROM person" to goldens file
--    * clean up database
testMigration
  :: TestGoldenString
  -> MigrateBackend
  -> IO (Pool SqlBackend)
  -> String
  -> Int
  -> [SqlPersistT IO ()]
  -> TestTree
testMigration testGolden backend getPool name n populateDb = testGolden name $ do
  pool <- getPool
  let runMigration' = runMigration backend pool
      city = CityKey 1
      insertCity = insertKey city $ City "Berkeley" "CA"

  res <- (`finally` cleanup pool) $ do
    -- test setup
    unless (null setupMigration) $ do
      runMigration' setupMigration
      -- populateDb scripts can use hometown=1
      runSql pool insertCity
    needsMore <- runSql pool $ hasMigration autoMigration
    if n >= length manualMigration
      then when needsMore $ fail "More migrations are detected"
      else unless needsMore $ fail "No more migrations detected"
    mapM_ (runSql pool) populateDb

    -- run migrations and check inserting current models works
    runMigration' manualMigration
    runSql pool $ do
      checkMigration autoMigration
      get city >>= \case
        Just _ -> return ()
        Nothing -> insertCity
      insertMany_
        [ Person "Alice" city (Just "Female") False
        , Person "Bob" city (Just "Male") True
        , Person "Courtney" city Nothing False
        ]
      map entityVal <$> selectList [] []

  return $ showPersons res
  where
    setupMigration = take n manualMigration
    cleanup pool = runSql pool $ do
      rawExecute "DROP TABLE persistent_migration" []
      rawExecute "DROP TABLE person" []
      rawExecute "DROP TABLE city" []

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