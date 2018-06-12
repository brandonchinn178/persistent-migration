import qualified Database.Persist.Migration.Postgres as Postgres
import Test.Tasty
import Test.Unit.Migration (testMigrations)

main :: IO ()
main = defaultMain $ testGroup "persistent-migration-goldens"
  [ testMigrations "postgresql" Postgres.backend
  ]
