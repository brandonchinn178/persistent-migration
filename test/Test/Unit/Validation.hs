{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Validation (testValidation) where

import Database.Persist.Migration.Internal
import Database.Persist.Sql (SqlType(..))
import Test.Tasty (TestTree, testGroup)
import Test.Utils.Goldens (goldenVsShow)

-- | Run tests related to migration validaton.
testValidation :: FilePath -> TestTree
testValidation dir = testGroup "validation"
  [ testOperation "Duplicate ColumnProps in CreateTable" $
      CreateTable "person" [Column "age" SqlInt32 [NotNull, NotNull]] []
  , testOperation "Duplicate Constraints in CreateTable" $
      CreateTable "person"
        [Column "id1" SqlInt32 [], Column "id2" SqlInt32 []]
        [PrimaryKey ["id1"], PrimaryKey ["id2"]]
  , testOperation "Constraint references non-existent column" $
      CreateTable "person" [] [PrimaryKey ["id"]]
  , testOperation "Duplicate ColumnProps in AddColumn" $
      AddColumn "person" (Column "age" SqlInt32 [NotNull, NotNull]) Nothing
  , testOperation "Non-null AddColumn without default" $
      AddColumn "person" (Column "age" SqlInt32 [NotNull]) Nothing
  ]
  where
    goldenVsShow' :: Show a => String -> a -> TestTree
    goldenVsShow' name = goldenVsShow dir name . pure
    testOperation name = goldenVsShow' name . validateOperation
