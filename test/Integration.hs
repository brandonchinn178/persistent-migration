import Control.Concurrent (threadDelay)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as ByteString
import Data.Pool (Pool, destroyAllResources)
import qualified Database.Persist.Migration.Postgres as Postgres
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withTempDirectory)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , createProcess
    , proc
    , readProcessWithExitCode
    , terminateProcess
    )
import Test.Integration.Migration (testIntegration)
import Test.Tasty

main :: IO ()
main = withTempDirectory "/tmp" "persistent-migration-integration" $ \dir ->
  defaultMain $ testGroup "persistent-migration-integration"
    [ withPostgres dir $ testIntegration "postgresql" Postgres.backend
    ]

-- | Run a function with the PostgreSQL backend.
withPostgres :: FilePath -> (IO (Pool SqlBackend) -> TestTree) -> TestTree
withPostgres dir f = withResource startPostgres stopPostgres $ f . fmap snd
  where
    dir' = dir ++ "/postgresql"
    -- running postgres
    startPostgres = do
      -- initialize local postgres server
      callProcess' "initdb" ["-D", dir']
      ph <- spawnProcess' "postgres" ["-h", "", "-k", dir', "-D", dir']
      threadDelay 100000
      callProcess' "createdb" ["-h", dir', "test_db"]
      -- create a connection Pool
      let connString = ByteString.pack $ "postgresql:///test_db?host=" ++ dir'
      pool <- runNoLoggingT $ createPostgresqlPool connString 4
      return (ph, pool)
    stopPostgres (ph, pool) = do
      terminateProcess ph
      destroyAllResources pool
    -- calling processes
    callProcess' cmd args = do
      (code, out, err) <- readProcessWithExitCode cmd args ""
      case code of
        ExitSuccess -> return ()
        ExitFailure _ -> do
          hPutStrLn stderr out
          hPutStrLn stderr err
          exitWith code
    spawnProcess' cmd args = do
      (_, _, _, ph) <- createProcess (proc cmd args)
        { std_out = NoStream
        , std_err = NoStream
        }
      return ph
