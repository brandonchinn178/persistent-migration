{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Validation (testValidation) where

import Database.Persist.Migration.Internal
import Database.Persist.Sql (SqlType(..))
import Test.Tasty (TestTree, testGroup)
import Test.Utils.Goldens (goldenVsShow)

-- | Run tests related to migration validaton.
testValidation :: FilePath -> TestTree
testValidation dir = testGroup "validation"
  [ testMigrationPath "Valid migration"
      [0 ~> 1, 1 ~> 2, 2 ~> 3]
  , testMigrationPath "Valid re-ordered migration"
      [1 ~> 2, 0 ~> 1, 2 ~> 3]
  , testMigrationPath "Invalid decreasing migration"
      [0 ~> 1, 1 ~> 3, 3 ~> 2, 2 ~> 4]
  , testMigrationPath "Invalid duplicate operation paths"
      [0 ~> 1, 1 ~> 2, 1 ~> 2]
  , testOperation "Duplicate ColumnProps in CreateTable" $
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
    testMigrationPath name = goldenVsShow' name . validateMigrationPath
    testOperation name = goldenVsShow' name . validateOperation

-- | Validate OperationPaths in a Migration.
validateMigrationPath :: [OperationPath] -> Either String ()
validateMigrationPath = validateMigration . map (\path -> Operation{opPath = path, opOp = NoOp})
