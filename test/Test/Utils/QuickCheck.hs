{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Utils.QuickCheck
  ( Identifier(..)
  , genPersistValue
  -- * Utilities
  , DistinctList(..)
  , mapSome
  , group
  ) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.LocalTime (TimeOfDay(..))
import Database.Persist.Migration
    (Column(..), ColumnProp(..), CreateTable(..), TableConstraint(..))
import Database.Persist.Sql (PersistValue(..), SqlType(..))
import Test.QuickCheck hiding (scale)

instance Arbitrary CreateTable where
  arbitrary = do
    Identifier name <- arbitrary

    -- get names of tables this table can have foreign keys towards
    DistinctList colNames <- arbitrary
    -- max out at 100 names
    let colNames' = take 100 $ map unIdent colNames

    -- generate schema
    DistinctList tableNames <- arbitrary
    let tableNames' = filter (/= Identifier name) tableNames
    cols <- vectorOf (length colNames') $ genColumn tableNames'
    let idCol = Column "id" SqlInt32 [NotNull, AutoIncrement]
        cols' = map (\(colName', col) -> col{colName = colName'}) $ zip colNames' cols
        schema = idCol : cols'

    -- all of the columns that will be unique
    uniqueCols <- sublistOf $ map colName cols'
    let mkUnique names =
          -- constraint name can be max 63 characters
          let constraintName = Text.take 63 $ "unique_" <> Text.intercalate "_" names
          in Unique constraintName names
        -- unique constraints should not have more than 32 columns
        max32 l = if length l > 32
          then take 32 l : max32 (drop 32 l)
          else [l]
    uniqueConstraints <- map mkUnique . concatMap max32 <$> group uniqueCols

    let constraints = PrimaryKey ["id"] : uniqueConstraints

    return CreateTable{..}

-- | Generate an arbitrary Column with a possibly pre-determined name.
--
-- Also given the set of table names that can be referenced by foreign keys.
genColumn :: [Identifier] -> Gen Column
genColumn tableNames = do
  colName <- fmap unIdent arbitrary

  references <- if null tableNames
    then return []
    else do
      Identifier table <- elements tableNames
      arbitrarySingleton 10 $ References (table, "id")

  colType <- if null references
    then arbitrary
    else return SqlInt32

  autoIncrement <- arbitrarySingleton 1 AutoIncrement
  notNull <- arbitrarySingleton 50 NotNull

  let colProps = notNull ++ autoIncrement ++ references

  return Column{..}
  where
    -- get list with x% chance of having the given element and (100 - x)% chance of
    -- being an empty list
    arbitrarySingleton x v = frequency [(x, pure [v]), (100 - x, pure [])]

instance Arbitrary Column where
  arbitrary = listOf arbitrary >>= genColumn

newtype Identifier = Identifier { unIdent :: Text }
  deriving (Show,Eq)

instance Arbitrary Identifier where
  arbitrary = do
    first <- elements underletter
    rest <- listOf $ elements $ underletter ++ ['0'..'9']
    let ident = first : rest
    if ident == "id"
      then arbitrary
      else
        -- identifiers max 63 characters long
        return . Identifier . Text.pack . take 63 $ first : rest
    where
      underletter = '_':['a'..'z']

instance Arbitrary SqlType where
  arbitrary = do
    numPrecision <- choose (1, 1000)
    numScale <- choose (0, numPrecision)
    elements
      [ SqlString
      , SqlInt32
      , SqlInt64
      , SqlReal
      , SqlNumeric numPrecision numScale
      , SqlBool
      , SqlDay
      , SqlTime
      , SqlDayTime
      , SqlBlob
      ]

-- | Generate an arbitrary PersistValue for the given SqlType.
genPersistValue :: SqlType -> Gen PersistValue
genPersistValue = \case
  SqlString -> PersistText <$> arbitrary
  SqlInt32 -> PersistInt64 <$> choose (-2147483648, 2147483647)
  SqlInt64 -> PersistInt64 <$> choose (-2147483648, 2147483647)
  SqlReal -> PersistDouble <$> arbitrary
  SqlNumeric precision scale -> do
    v <- choose (0, 1) :: Gen Double
    let v' = truncate (v * (10 ^ precision)) :: Integer
    return . PersistRational $ fromIntegral v' / (10 ^ scale)
  SqlBool -> PersistBool <$> arbitrary
  SqlDay -> PersistDay <$> arbitrary
  SqlTime -> PersistTimeOfDay <$> arbitrary
  SqlDayTime -> PersistUTCTime <$> arbitrary
  SqlBlob -> PersistByteString <$> arbitrary
  SqlOther _ -> fail "SqlOther not supported"

{- Utilities -}

newtype DistinctList a = DistinctList { unDistinctList :: [a] }
  deriving (Show)

instance (Arbitrary a, Eq a) => Arbitrary (DistinctList a) where
  arbitrary = DistinctList . nub <$> listOf arbitrary

instance Arbitrary Text where
  arbitrary = Text.pack . getUnicodeString <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = Text.encodeUtf8 <$> arbitrary

instance Arbitrary Day where
  arbitrary = fromGregorian <$> choose (1, 294276) <*> choose (1, 12) <*> choose (1, 31)

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*> genSeconds
    where
      genSeconds = fromRational . toRational <$> (choose (0, 60) :: Gen Double)

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> (secondsToDiffTime <$> choose (0, 86400))

-- | Randomly modify at least one element in the list with the given function.
mapSome :: (a -> a) -> [a] -> Gen [a]
mapSome _ [] = return []
mapSome f l = do
  i <- choose (0, length l - 1)
  let (half1, half2) = splitAt i l
      modify = mapM $ \x -> do
        shouldModify <- arbitrary
        return $ if shouldModify then f x else x
  half1' <- modify half1
  half2' <- modify $ tail half2
  return $ half1' ++ [f $ head half2] ++ half2'

-- | Randomly group the given list.
group :: [a] -> Gen [[a]]
group = shuffle >=> partition []
  where
    partition res [] = return res
    partition res l = do
      i <- choose (1, length l)
      let (first, rest) = splitAt i l
      partition (first : res) rest
