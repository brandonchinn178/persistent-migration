{-# LANGUAGE LambdaCase #-}

module Utils.Goldens
  ( goldenDir
  , goldenVsString
  , goldenVsText
  , goldenVsShow
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as Text
import Test.Tasty (TestTree)
import qualified Test.Tasty.Golden as Golden

{- Golden files -}

-- | Get the directory of the golden files for the given test type and label.
goldenDir :: String -> String -> FilePath
goldenDir testType label = "test/goldens-" ++ testType ++ "/" ++ label ++ "/"

{- Golden test -}

-- | Run a goldens test where the goldens file is generated from the name.
goldenVsString :: FilePath -> String -> IO ByteString -> TestTree
goldenVsString dir name = Golden.goldenVsString name goldenFile
  where
    goldenFile = dir ++ map slugify name ++ ".txt"
    slugify = \case
      ' ' -> '-'
      x -> toLower x

-- | Run a goldens test against a Text.
goldenVsText :: FilePath -> String -> IO Text -> TestTree
goldenVsText dir name = goldenVsString dir name . fmap toByteString
  where
    toByteString = Text.encodeUtf8 . fromStrict

-- | Run a goldens test against a Showable value.
goldenVsShow :: Show a => FilePath -> String -> IO a -> TestTree
goldenVsShow dir name = goldenVsText dir name . fmap (Text.pack . show)
