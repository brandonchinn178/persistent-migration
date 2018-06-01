{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.MigrationTest where

import Data.Text (Text)
import Database.Persist.Migration
import Database.Persist.Sql (SqlType(..))
import Database.Persist.TestUtils
import Test.Hspec.Expectations

getTestMigration :: MigrateBackend -> Migration -> IO [Text]
getTestMigration backend = runTestBackend . getMigration backend

getTestMigration' :: Migration -> IO [Text]
getTestMigration' = getTestMigration testMigrateBackend

unit_basic_migration :: Expectation
unit_basic_migration = getTestMigration' migration `shouldReturn` migrationText
  where
    migration =
      [ Operation 0 $
          CreateTable
            { ctName = "person"
            , ctSchema =
                [ Column "id" SqlInt32 []
                , Column "name" SqlString []
                , Column "age" SqlInt32 []
                , Column "alive" SqlBool [Defaults "TRUE"]
                , Column "hometown" SqlInt64 [Nullable, ForeignKey ("cities", "id")]
                ]
            , ctConstraints =
                [ PrimaryKey ["id"]
                , Unique ["name"]
                ]
            }
      , Operation 1 $ AddColumn "person" (Column "gender" SqlString [Nullable]) Nothing
      , Operation 2 $ DropColumn "person" "alive"
      , Operation 3 $ DropTable "person"
      ]
    migrationText =
      [ "CREATE TABLE person"
      , "ADD COLUMN person.gender"
      , "DROP COLUMN person.alive"
      , "DROP TABLE person"
      ]

unit_duplicate_operation_ids :: Expectation
unit_duplicate_operation_ids = getTestMigration' migration `shouldThrow` anyException
  where
    migration =
      [ Operation 0 $ CreateTable "person" [] []
      , Operation 0 $ DropTable "person"
      ]

unit_some_done :: Expectation
unit_some_done = getTestMigration backend migration `shouldReturn` migrationText
  where
    backend = testMigrateBackend{getCompletedOps = return [0]}
    migration =
      [ Operation 0 $ CreateTable "person" [] []
      , Operation 1 $ DropTable "person"
      ]
    migrationText = ["DROP TABLE person"]

unit_all_done :: Expectation
unit_all_done = getTestMigration backend migration `shouldReturn` []
  where
    backend = testMigrateBackend{getCompletedOps = return [0, 1]}
    migration =
      [ Operation 0 $ CreateTable "person" [] []
      , Operation 1 $ DropTable "person"
      ]

unit_revert_no_run :: Expectation
unit_revert_no_run = getTestMigration' migration `shouldReturn` []
  where
    migration =
      [ Operation 0 $ CreateTable "person" [] []
      , Operation 1 $ Revert 0
          [ SubOperation $ DropTable "person"
          ]
      ]

unit_revert_run :: Expectation
unit_revert_run = getTestMigration backend migration `shouldReturn` migrationText
  where
    backend = testMigrateBackend{getCompletedOps = return [0]}
    migration =
      [ Operation 0 $ CreateTable "person" [] []
      , Operation 1 $ Revert 0
          [ SubOperation $ DropTable "person"
          ]
      ]
    migrationText = ["DROP TABLE person"]
