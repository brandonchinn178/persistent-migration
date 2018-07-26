{-|
Module      :  Database.Persist.Migration.Utils.Sql
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines helper functions for writing SQL queries.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.Migration.Utils.Sql
  ( commas
  , uncommas
  , uncommas'
  , quote
  , MigrateSql(..)
  , executeSql
  , pureSql
  , mapSql
  , concatSql
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (PersistValue(..), SqlPersistT)
import qualified Database.Persist.Sql as Persist

-- | Split the given line by commas, ignoring commas within parentheses.
--
-- > commas "a,b,c" == ["a", "b", "c"]
-- > commas "a,b,c (d,e),z" == ["a", "b", "c (d,e)", "z"]
-- > commas "a,b,c (d,e,(f,g)),z" == ["a", "b", "c (d,e,(f,g))", "z"]
commas :: Text -> [Text]
commas t = go (Text.unpack t) "" [] (0 :: Int)
  where
    go src buffer result level =
      let result' = result ++ [Text.pack buffer]
      in case src of
        "" -> result'
        ',':xs | level == 0 -> go xs "" result' level
        '(':xs -> go xs (buffer ++ "(") result (level + 1)
        ')':xs -> go xs (buffer ++ ")") result (max 0 $ level - 1)
        x:xs -> go xs (buffer ++ [x]) result level

-- | Join the given Text with commas separating each item.
uncommas :: [Text] -> Text
uncommas = Text.intercalate ","

-- | Join the given Text with commas separating each item and quoting them.
uncommas' :: [Text] -> Text
uncommas' = uncommas . map quote

-- | Quote the given Text.
quote :: Text -> Text
quote t = "\"" <> t <> "\""

-- | A SQL query (with placeholders) and values to replace those placeholders.
data MigrateSql = MigrateSql
  { sqlText :: Text
  , sqlVals :: [PersistValue]
  } deriving (Show)

-- | Execute a SQL query.
executeSql :: MonadIO m => MigrateSql -> SqlPersistT m ()
executeSql MigrateSql{..} = Persist.rawExecute sqlText sqlVals

-- | Create a MigrateSql from the given Text.
pureSql :: Text -> MigrateSql
pureSql sql = MigrateSql sql []

-- | Map the SQL text with the given function.
mapSql :: (Text -> Text) -> MigrateSql -> MigrateSql
mapSql f sql = sql { sqlText = f $ sqlText sql }

-- | Concatenate the given MigrateSql queries with the given combining function.
concatSql :: ([Text] -> Text) -> [MigrateSql] -> MigrateSql
concatSql f queries = MigrateSql
  { sqlText = f $ map sqlText queries
  , sqlVals = concatMap sqlVals queries
  }
