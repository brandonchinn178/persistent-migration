import Database.Persist.Migration (MigrateBackend)
import qualified Database.Persist.Migration.Postgres as Postgres
import Test.Tasty

import Migration (testMigrations)
import Property (testProperties)
import Utils.Goldens (goldenDir)

unitDir :: String -> FilePath
unitDir = goldenDir "unit"

main :: IO ()
main = defaultMain $ testGroup "persistent-migration-unit"
  [ testBackend "postgresql" Postgres.backend
  , testProperties
  ]

-- | Build a test suite running unit tests for the given MigrateBackend.
testBackend :: String -> MigrateBackend -> TestTree
testBackend label backend = testGroup label
  [ testMigrations (unitDir label) backend
  ]
