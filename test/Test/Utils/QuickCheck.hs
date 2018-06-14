{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Utils.QuickCheck
  ( Identifier(..)
  -- * Utilities
  , DistinctList(..)
  , mapSome
  , group
  ) where

import Control.Monad ((>=>))
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Migration
    (Column(..), ColumnProp(..), CreateTable(..), TableConstraint(..))
import Database.Persist.Sql (SqlType(..))
import Test.QuickCheck

instance Arbitrary CreateTable where
  arbitrary = do
    Identifier ctName <- arbitrary

    -- get names of tables this table can have foreign keys towards
    DistinctList colNames <- arbitrary
    -- max out at 100 names
    let colNames' = take 100 $ map unIdent colNames

    -- generate schema
    DistinctList tableNames <- arbitrary
    let tableNames' = filter (/= Identifier ctName) tableNames
    cols <- vectorOf (length colNames') $ genColumn tableNames'
    let idCol = Column "id" SqlInt32 [NotNull, AutoIncrement]
        cols' = map (\(name, col) -> col{colName = name}) $ zip colNames' cols
        ctSchema = idCol : cols'

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

    let ctConstraints = PrimaryKey ["id"] : uniqueConstraints

    return CreateTable{..}

-- | Generate an arbitrary Column with a possibly pre-determined name.
--
-- Also given the set of table names that can be referenced by foreign keys.
genColumn :: [Identifier] -> Gen Column
genColumn tableNames = do
  colName <- fmap unIdent arbitrary `suchThat` (/= "id")

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

{- Utilities -}

newtype DistinctList a = DistinctList { unDistinctList :: [a] }
  deriving (Show)

instance (Arbitrary a, Eq a) => Arbitrary (DistinctList a) where
  arbitrary = DistinctList . nub <$> listOf arbitrary

instance Arbitrary Text where
  arbitrary = Text.pack . getUnicodeString <$> arbitrary

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
