{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Migration (testMigrations) where

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
import Database.Persist.Migration.Utils.Sql (uncommas, uncommas')
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
import Test.Tasty (TestTree, testGroup)

import Utils.Goldens (goldenVsString)
import Utils.RunSql (runMigration, runSql)

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
  [ 0 ~> 1 :=
    [ CreateTable
        { name = "city"
        , schema =
            [ Column "id" SqlInt64 [NotNull, AutoIncrement]
            , Column "name" SqlString [NotNull]
            , Column "state" SqlString [NotNull]
            ]
        , constraints =
            [ PrimaryKey ["id"]
            , Unique "unique_city" ["state", "name"]
            ]
        }
    , CreateTable
        { name = "person"
        , schema =
            [ Column "id" SqlInt64 [NotNull, AutoIncrement]
            , Column "name" SqlString [NotNull]
            , Column "hometown" SqlInt64 [NotNull, References ("city", "id")]
            ]
        , constraints =
            [ PrimaryKey ["id"]
            ]
        }
    ]

  -- add binary sex column
  , 1 ~> 2 :=
    [ AddColumn "person" (Column "sex" SqlInt32 []) Nothing
    ]

  -- change binary sex to stringly gender
  , 2 ~> 3 :=
    [ AddColumn "person" (Column "gender" SqlString []) Nothing
    , migrateGender
    , DropColumn ("person", "sex")
    ]

  -- shortcut for databases that hadn't already added the sex column
  , 1 ~> 3 :=
    [ AddColumn "person" (Column "gender" SqlString []) Nothing
    ]

  -- add colorblind column, with everyone currently in the database being not colorblind
  , 3 ~> 4 :=
    [ AddColumn "person" (Column "colorblind" SqlBool [NotNull])
        (Just $ PersistBool False)
    ]
  ]
  where
    migrateGender = RawOperation "Convert binary sex column into stringly gender column" $
      mapMaybe migrateGender' <$> rawSql "SELECT id, sex FROM person" []
    migrateGender' = \case
      (_, Single Nothing) -> Nothing
      (Single id', Single (Just sex)) ->
        Just $ MigrateSql  "UPDATE person SET gender = ? WHERE id = ?"
          [ PersistText $ sexToGender sex
          , PersistInt64 id'
          ]
    sexToGender :: Int -> Text
    sexToGender sex = if sex == 0 then "Male" else "Female"

{- Test suite -}

-- | A test suite for running migrations.
testMigrations :: FilePath -> MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testMigrations dir backend getPool = testGroup "goldens"
  [ testMigration' "Migrate from empty" 0 []
  , testMigration' "Migrate with v1 person" 1
      [ insertPerson "David" []
      ]
  , testMigration' "Migrate from sex to gender" 2
      [ insertPerson "David" [("sex", "0")]
      , insertPerson "Elizabeth" [("sex", "1")]
      , insertPerson "Foster" [("sex", "NULL")]
      ]
  , testMigration' "Migrate with default colorblind" 4
      [ insertPerson "David" []
      ]
  , testMigration' "Migrations are idempotent" (length manualMigration)
      [ insertPerson "David" [("colorblind", "TRUE")]
      ]
  ]
  where
    testMigration' = testMigration dir backend getPool
    insertPerson name extra =
      let cols = ["name", "hometown"] ++ map fst extra
          vals = ["'" <> name <> "'", "1"] ++ map snd extra
      in rawExecute
          ("INSERT INTO person(" <> uncommas' cols <> ") VALUES (" <> uncommas vals <> ")")
          []

-- | Run a test where:
--    * the first N migration paths have been migrated (in the `manualMigration` list, NOT the
--      version number)
--    * the given query is run to populate the database
--    * the remaining migration paths are migrated
--    * insert some data into the database
--    * output "SELECT * FROM person" to goldens file
--    * clean up database
testMigration
  :: FilePath
  -> MigrateBackend
  -> IO (Pool SqlBackend)
  -> String
  -> Int
  -> [SqlPersistT IO ()]
  -> TestTree
testMigration dir backend getPool name n populateDb = goldenVsString dir name $ do
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
      rawExecute "DROP TABLE IF EXISTS persistent_migration" []
      rawExecute "DROP TABLE IF EXISTS person" []
      rawExecute "DROP TABLE IF EXISTS city" []

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
