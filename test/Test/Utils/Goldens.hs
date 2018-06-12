{-# LANGUAGE LambdaCase #-}

module Test.Utils.Goldens
  ( goldenVsString
  , goldenVsText
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as Text
import Test.Tasty (TestTree)
import qualified Test.Tasty.Golden as Golden

-- | Run a goldens test where the goldens file is generated from the name.
goldenVsString :: String -> String -> String -> IO ByteString -> TestTree
goldenVsString testType label name = Golden.goldenVsString name goldenFile
  where
    goldenFile = "test/goldens-" ++ testType ++ "/" ++ label ++ "/" ++ map slugify name ++ ".txt"
    slugify = \case
      ' ' -> '-'
      x -> toLower x

-- | Run a goldens test against a Text.
goldenVsText :: String -> String -> String -> IO Text -> TestTree
goldenVsText testType label name = goldenVsString testType label name . fmap toByteString
  where
    toByteString = Text.encodeUtf8 . fromStrict
