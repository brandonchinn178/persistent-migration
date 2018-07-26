{-# LANGUAGE OverloadedStrings #-}

module Utils.Backends
  ( withPostgres
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as ByteString
import Data.Monoid ((<>))
import Data.Pool (Pool, destroyAllResources)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, withResource)

-- | Run a function with the PostgreSQL backend.
withPostgres :: FilePath -> (IO (Pool SqlBackend) -> TestTree) -> TestTree
withPostgres dir = withResource startPostgres stopPostgres
  where
    dir' = dir ++ "/postgresql/"
    -- running postgres
    startPostgres = do
      -- initialize local postgres server
      callProcess' "pg_ctl" ["-D", dir', "init"]
      -- modify configuration
      let confFile = dir' ++ "postgresql.conf"
      conf <- Text.readFile confFile
      Text.writeFile confFile . Text.unlines . modifyConf . Text.lines $ conf
      -- start postgres server
      callProcess' "pg_ctl"
        [ "-D", dir'
        , "-l", dir' ++ "postgres.log"
        , "-o", "-h '' -k '" ++ dir' ++ "'"
        , "start"
        ]
      threadDelay 1000000
      callProcess' "createdb" ["-h", dir', "test_db"]
      -- create a connection Pool
      let connString = ByteString.pack $ "postgresql:///test_db?host=" ++ dir'
      runNoLoggingT $ createPostgresqlPool connString 4
    stopPostgres pool = do
      callProcess' "pg_ctl" ["-D", dir', "stop"]
      destroyAllResources pool
    -- utilities
    callProcess' cmd args = do
      (code, out, err) <- readProcessWithExitCode cmd args ""
      case code of
        ExitSuccess -> return ()
        ExitFailure _ -> do
          hPutStrLn stderr out
          hPutStrLn stderr err
          exitWith code
    modifyConf conf = foldl modifyConf' conf
      [ ("client_min_messages", "warning")
      , ("log_min_messages", "info")
      , ("log_statement", "all")
      ]
    modifyConf' conf (k, v) =
      let setting = Text.unwords [k, "=", v]
      in case conf of
        [] -> [setting]
        line:conf' -> if ("#" <> k) `Text.isPrefixOf` line
          then setting : conf'
          else modifyConf' conf' (k, v)
