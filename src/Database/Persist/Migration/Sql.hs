{-|
Module      :  Database.Persist.Migration.Sql
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines helper functions for writing SQL queries.
-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Migration.Sql where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

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

-- | Quote the given Text.
quote :: Text -> Text
quote t = "\"" <> t <> "\""
