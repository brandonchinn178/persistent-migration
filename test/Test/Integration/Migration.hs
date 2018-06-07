module Test.Integration.Migration (testIntegration) where

import Data.Pool (Pool, withResource)
import qualified Data.Text as Text
import Database.Persist.Migration
import Database.Persist.Sql (SqlBackend)
import Test.Tasty (TestTree, testGroup)
import Test.Utils.Goldens (goldenVsString)

-- | Build a test suite running integration tests for the given MigrateBackend.
testIntegration :: String -> MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testIntegration label backend getPool = testGroup label
  [ goldenShow' "Basic migration" $ return 1
  ]
  where
    goldenShow' = goldenShow label

{- Helpers -}

-- | Run a goldens test for an IO action returning a Showable value.
goldenShow :: Show a => String -> String -> IO a -> TestTree
goldenShow label name = goldenVsString "integration" label name . fmap (Text.pack . show)
