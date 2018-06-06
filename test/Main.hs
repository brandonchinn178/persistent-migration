import Database.Persist.MigrationTest (testMigrations)
import qualified Database.Persist.Migration.Postgres as Postgres
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "persistent-migration-goldens"
  [ testMigrations "postgresql" Postgres.backend
  ]
