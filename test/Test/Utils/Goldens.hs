{-# LANGUAGE LambdaCase #-}

module Test.Utils.Goldens
  ( goldenVsString
  ) where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as Text
import Test.Tasty (TestTree)
import qualified Test.Tasty.Golden as Golden

-- | Run a goldens test where the goldens file is generated from the name.
goldenVsString :: String -> String -> String -> IO Text -> TestTree
goldenVsString testType label name action = Golden.goldenVsString name goldenFile $
  toByteString <$> action
  where
    goldenFile = "test/goldens-" ++ testType ++ "/" ++ label ++ "/" ++ map slugify name ++ ".txt"
    slugify = \case
      ' ' -> '-'
      x -> toLower x
    toByteString = Text.encodeUtf8 . fromStrict
