{-# LANGUAGE LambdaCase #-}

module Test.Utils.Goldens
  ( TestGoldenString
  , goldenVsString
  , TestGoldenText
  , goldenVsText
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as Text
import Test.Tasty (TestTree)
import qualified Test.Tasty.Golden as Golden

type TestGoldenString = String -> IO ByteString -> TestTree

-- | Run a goldens test where the goldens file is generated from the name.
goldenVsString :: String -> String -> TestGoldenString
goldenVsString testType label name = Golden.goldenVsString name goldenFile
  where
    goldenFile = "test/goldens-" ++ testType ++ "/" ++ label ++ "/" ++ map slugify name ++ ".txt"
    slugify = \case
      ' ' -> '-'
      x -> toLower x

type TestGoldenText = String -> IO Text -> TestTree

-- | Run a goldens test against a Text.
goldenVsText :: String -> String -> TestGoldenText
goldenVsText testType label name = goldenVsString testType label name . fmap toByteString
  where
    toByteString = Text.encodeUtf8 . fromStrict
