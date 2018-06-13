{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Utils.QuickCheck
  ( genCreateTable
  , genColumn
  ) where

import Control.Monad ((>=>))
import Control.Monad.Extra (concatMapM)
import Data.Function (on)
import Data.List (nub, nubBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (SqlType(..))
import Database.Persist.Migration
  ( Column(..)
  , ColumnProp(..)
  , CreateTable(..)
  , TableConstraint(..)
  )
import Test.QuickCheck

-- | Generate an arbitrary CREATE TABLE query. Also generates tables necessary for foreign keys.
genCreateTable :: Gen [CreateTable]
genCreateTable = genCreateTable' True Nothing
  where
    genCreateTable' withFKs maybeName = do
      ctName <- maybe genIdent return maybeName

      -- get names of tables this table can have foreign keys towards
      numTables <- choose (0, if withFKs then 2 else 0)
      tableNames <- vectorOf numTables $ genIdent `suchThat` (/= ctName)

      -- generate schema
      let idCol = Column "id" SqlInt32 [NotNull, AutoIncrement]
          nubCols = nubBy ((==) `on` colName) . filter ((/= "id") . colName)
      -- max at 100 columns
      cols <- fmap (nubCols . take 100) . listOf $ genColumn tableNames
      let ctSchema = idCol : cols

      -- generate needed tables for foreign keys
      let getFK = \case
            References fk -> Just fk
            _ -> Nothing
          neededTables = nub . map fst . concatMap (mapMaybe getFK . colProps) $ ctSchema
          mkCreateTable name = genCreateTable' False $ Just name
      otherTables <- concatMapM mkCreateTable neededTables

      -- all of the columns that will be unique
      uniqueCols <- sublistOf $ map colName cols
      let mkUnique names = Unique (Text.take 63 $ Text.intercalate "_" names) names
          -- unique constraints should not have more than 32 columns
          max32 l = if length l > 32
            then take 32 l : max32 (drop 32 l)
            else [l]
      uniqueConstraints <- (map mkUnique . concatMap max32) <$> group uniqueCols

      let ctConstraints = PrimaryKey ["id"] : uniqueConstraints

      return $ otherTables ++ [CreateTable{..}]

-- | Generate an arbitrary Column with a possibly pre-determined name.
--
-- Also given the set of table names that can be referenced by foreign keys.
genColumn :: [Text] -> Gen Column
genColumn tableNames = do
  colName <- genIdent `suchThat` (/= "id")

  -- determine foreign key
  hasFK <- frequency [(1, pure True), (5, pure False)]
  references <- if hasFK && not (null tableNames)
    then do
      foreignKeyTable <- elements tableNames
      return [References (foreignKeyTable, "id")]
    else return []

  colType <- if null references
    then arbitrary
    else return SqlInt32

  notNull <- sublistOf [NotNull]
  let colProps = notNull ++ references

  return Column{..}

{- Helpers -}

-- | Generate a random, valid identifier.
genIdent :: Gen Text
genIdent = do
  first <- elements underletter
  rest <- listOf $ elements $ underletter ++ ['0'..'9']
  -- identifiers max 63 characters long
  return . Text.pack . take 63 $ first : rest
  where
    underletter = '_':['a'..'z']

-- | Randomly group the given list.
group :: [a] -> Gen [[a]]
group = shuffle >=> partition []
  where
    partition res [] = return res
    partition res l = do
      i <- choose (1, length l)
      let (first, rest) = splitAt i l
      partition (first : res) rest

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
