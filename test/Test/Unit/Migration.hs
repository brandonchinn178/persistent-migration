{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Migration (testMigrations) where

import Control.Monad.Reader (runReaderT)
import qualified Data.Text as Text
import Database.Persist.Migration.Internal
import Database.Persist.Sql (SqlType(..))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Unit.Backends
    (MockDatabase(..), defaultDatabase, setDatabase, withTestBackend)
import Test.Utils.Goldens (goldenVsText)

-- | Build a test suite for the given MigrateBackend.
testMigrations :: FilePath -> MigrateBackend -> TestTree
testMigrations dir backend = testGroup "migrations"
  [ goldenMigration' "Basic migration" defaultDatabase
      [ Operation (0 ~> 1) $
          CreateTable
            { ctName = "person"
            , ctSchema =
                [ Column "id" SqlInt32 []
                , Column "name" SqlString [NotNull]
                , Column "age" SqlInt32 [NotNull]
                , Column "alive" SqlBool [NotNull]
                , Column "hometown" SqlInt64 [References ("cities", "id")]
                ]
            , ctConstraints =
                [ PrimaryKey ["id"]
                , Unique "unique_name" ["name"]
                ]
            }
      , Operation (1 ~> 2) $ AddColumn "person" (Column "gender" SqlString []) Nothing
      , Operation (2 ~> 3) $ DropColumn ("person", "alive")
      , Operation (3 ~> 4) $ DropTable "person"
      ]
  , goldenMigration' "Partial migration" (withVersion 1)
      [ Operation (0 ~> 1) $ CreateTable "person" [] []
      , Operation (1 ~> 2) $ DropTable "person"
      ]
  , goldenMigration' "Complete migration" (withVersion 2)
      [ Operation (0 ~> 1) $ CreateTable "person" [] []
      , Operation (1 ~> 2) $ DropTable "person"
      ]
  , goldenMigration' "Migration with shorter path" defaultDatabase
      [ Operation (0 ~> 1) $ CreateTable "person" [] []
      , Operation (1 ~> 2) $ AddColumn "person" (Column "gender" SqlString []) Nothing
      , Operation (0 ~> 2) $ CreateTable "person" [Column "gender" SqlString []] []
      ]
  , goldenMigration' "Partial migration avoids shorter path" (withVersion 1)
      [ Operation (0 ~> 1) $ CreateTable "person" [] []
      , Operation (1 ~> 2) $ AddColumn "person" (Column "gender" SqlString []) Nothing
      , Operation (0 ~> 2) $ CreateTable "person" [Column "gender" SqlString []] []
      ]
  , testOperation "Duplicate ColumnProps in CreateTable" $
      CreateTable "person" [Column "age" SqlInt32 [NotNull, NotNull]] []
  , testOperation "Duplicate Constraints in CreateTable" $
      CreateTable "person"
        [Column "id1" SqlInt32 [], Column "id2" SqlInt32 []]
        [PrimaryKey ["id1"], PrimaryKey ["id2"]]
  , testOperation "Constraint references non-existent column" $
      CreateTable "person" [] [PrimaryKey ["id"]]
  , testOperation "Duplicate ColumnProps in AddColumn" $
      AddColumn "person" (Column "age" SqlInt32 [NotNull, NotNull]) Nothing
  , testOperation "Non-null AddColumn without default" $
      AddColumn "person" (Column "age" SqlInt32 [NotNull]) Nothing
  ]
  where
    goldenMigration' = goldenMigration dir backend
    goldenVsShow' :: Show a => String -> a -> TestTree
    goldenVsShow' name = goldenVsShow dir name . pure
    testOperation name = goldenVsShow' name . validateOperation

{- Helpers -}

-- | Run a goldens test for a migration.
goldenMigration
  :: FilePath -> MigrateBackend -> TestName -> MockDatabase -> Migration -> TestTree
goldenMigration dir backend name testBackend migration = goldenVsText dir name $ do
  setDatabase testBackend
  Text.unlines <$> getMigration' migration
  where
    getMigration' = withTestBackend . runReaderT . getMigration backend defaultSettings

-- | Set the version in the MockDatabase.
withVersion :: Version -> MockDatabase
withVersion v = defaultDatabase{version = Just v}
