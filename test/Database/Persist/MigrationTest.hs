{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.MigrationTest (testMigrations) where

import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as Text
import Database.Persist.Migration
import Database.Persist.Sql (SqlType(..))
import Database.Persist.TestBackends
    (MockDatabase(..), defaultDatabase, setDatabase, withTestBackend)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

-- | Build a test suite for the given MigrateBackend.
testMigrations :: String -> MigrateBackend -> TestTree
testMigrations label backend = testGroup label
  [ testGoldens' "Basic migration" defaultDatabase
      [ Operation (0 ~> 1) $
          CreateTable
            { ctName = "person"
            , ctSchema =
                [ Column "id" SqlInt32 []
                , Column "name" SqlString [NotNull]
                , Column "age" SqlInt32 [NotNull]
                , Column "alive" SqlBool [NotNull, Defaults "TRUE"]
                , Column "hometown" SqlInt64 [ForeignKey ("cities", "id")]
                ]
            , ctConstraints =
                [ PrimaryKey ["id"]
                , Unique ["name"]
                ]
            }
      , Operation (1 ~> 2) $ AddColumn "person" (Column "gender" SqlString []) Nothing
      , Operation (2 ~> 3) $ DropColumn ("person", "alive")
      , Operation (3 ~> 4) $ DropTable "person"
      ]
  , testGoldens' "Partial migration" (withVersion 1)
      [ Operation (0 ~> 1) $ CreateTable "person" [] []
      , Operation (1 ~> 2) $ DropTable "person"
      ]
  , testGoldens' "Complete migration" (withVersion 2)
      [ Operation (0 ~> 1) $ CreateTable "person" [] []
      , Operation (1 ~> 2) $ DropTable "person"
      ]
  ]
  where
    testGoldens' = testGoldens label backend

{- Helpers -}

-- | Run a goldens test.
testGoldens :: String -> MigrateBackend -> TestName -> MockDatabase -> Migration -> TestTree
testGoldens label backend name testBackend migration = goldenVsString name goldenFile $ do
  setDatabase testBackend
  textsToByteString <$> getMigration' migration
  where
    goldenFile = "test/goldens/" ++ label ++ "/" ++ map slugify name ++ ".txt"
    slugify = \case
      ' ' -> '-'
      x -> toLower x

    getMigration' :: Migration -> IO [Text]
    getMigration' = withTestBackend . runReaderT . getMigration backend

    textsToByteString :: [Text] -> ByteString
    textsToByteString = Text.encodeUtf8 . fromStrict . Text.unlines

-- | Set the version in the MockDatabase.
withVersion :: Version -> MockDatabase
withVersion v = defaultDatabase{version = Just v}
