import Database.Persist.Migration.Internal (MigrateBackend)
import qualified Database.Persist.Migration.Postgres as Postgres
import Test.Tasty
import Test.Unit.Migration (testMigrations)
import Test.Unit.Validation (testValidation)
import Test.Utils.Goldens (goldenDir)

unitDir :: String -> FilePath
unitDir = goldenDir "unit"

main :: IO ()
main = defaultMain $ testGroup "persistent-migration-goldens"
  [ testUnit "postgresql" Postgres.backend
  , testValidation (unitDir "validation")
  ]

-- | Build a test suite running unit tests for the given MigrateBackend.
testUnit :: String -> MigrateBackend -> TestTree
testUnit label backend = testGroup label
  [ testMigrations (unitDir label) backend
  ]
