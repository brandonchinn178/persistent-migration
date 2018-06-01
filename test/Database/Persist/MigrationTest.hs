{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.MigrationTest where

import Database.Persist.Migration
import Database.Persist.Sql (SqlType(..))
import Database.Persist.TestUtils
import Test.Hspec.Expectations

unit_basicMigration :: Expectation
unit_basicMigration = do
  migrationText' <- runTestBackend (getMigration testMigrateBackend migration)
  migrationText' `shouldBe` migrationText
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
