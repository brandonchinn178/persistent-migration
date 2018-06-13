import Data.Pool (Pool)
import Database.Persist.Migration (MigrateBackend)
import qualified Database.Persist.Migration.Postgres as Postgres
import Database.Persist.Sql (SqlBackend)
import System.IO.Temp (withTempDirectory)
import Test.Integration.Backends (withPostgres)
import Test.Integration.Migration (testMigrations)
import Test.Integration.Property (testProperties)
import Test.Tasty
import Test.Utils.Goldens (goldenVsString)

main :: IO ()
main = withTempDirectory "/tmp" "persistent-migration-integration" $ \dir ->
  defaultMain $ testGroup "persistent-migration-integration"
    [ withPostgres dir $ testIntegration "postgresql" Postgres.backend
    ]

-- | Build a test suite running integration tests for the given MigrateBackend.
testIntegration :: String -> MigrateBackend -> IO (Pool SqlBackend) -> TestTree
testIntegration label backend getPool = testGroup label
  [ testMigrations goldenVsString' backend getPool
  , testProperties backend getPool
  ]
  where
    goldenVsString' = goldenVsString "integration" label
