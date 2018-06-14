{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Unit.Property (testProperties) where

import Control.Applicative (liftA2)
import Data.Either (isRight)
import Database.Persist.Migration.Internal
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Utils.QuickCheck (Identifier(..), mapSome)

-- | Run tests related to migration validaton.
testProperties :: TestTree
testProperties = testGroup "properties"
  [ testProperty "Valid migration" $ forAll arbitrary isValidMigrationPath
  , testProperty "Invalid decreasing migration" $
      forAll arbitrary $ \(MigrationPath opPaths) -> do
        opPaths' <- mapSome (\(a, b) -> (b, a)) opPaths
        return . not . isValidMigrationPath $ MigrationPath opPaths'
  , testProperty "Invalid duplicate operation path" $
      forAll arbitrary $ \(MigrationPath opPaths) -> do
        opPaths' <- mapSomeDupl opPaths
        return . not . isValidMigrationPath $ MigrationPath opPaths'
  , testProperty "Duplicate ColumnProps in CreateTable" $ do
      colsMaybeProps <- listOf arbitrary
      colsWithProps <- listOf1 (arbitrary `suchThat` (not . null . colProps))
      let duplProps = mapM $ \col@Column{colProps} -> do
            colProps' <- mapSomeDupl colProps
            return col{colProps = colProps'}
      cols <- liftA2 (++) (duplProps colsMaybeProps) (duplProps colsWithProps)
      let ct = CreateTable "foo" cols []
      return . not . isValidOperation $ ct
  , testProperty "Duplicate Constraints in CreateTable" $
      forAll arbitrary $ \ct@CreateTable{ctConstraints} -> do
        ctConstraints' <- mapSomeDupl ctConstraints
        return . not . isValidOperation $ ct{ctConstraints = ctConstraints'}
  , testProperty "Constraint references non-existent column" $
      forAll arbitrary $ \ct@CreateTable{..} -> do
        let existing = map (Identifier . colName) ctSchema
            genConstraint = do
              Identifier name <- arbitrary
              cols <- map unIdent <$> listOf1 (arbitrary `suchThat` (`notElem` existing))
              elements [PrimaryKey cols, Unique name cols]
        newConstraints <- listOf1 genConstraint
        return . not . isValidOperation $ ct{ctConstraints = ctConstraints ++ newConstraints}
  , testProperty "Duplicate ColumnProps in AddColumn" $
      forAll arbitrary $ \col@Column{colProps} -> do
        Identifier acTable <- arbitrary
        acDefault <- arbitrary
        colProps' <- mapSomeDupl colProps
        let acColumn = col{colProps = colProps'}
        return $ not (null colProps) ==> not (isValidOperation AddColumn{..})
  , testProperty "Non-null AddColumn without default" $
      forAll arbitrary $ \col@Column{colProps} -> do
        Identifier acTable <- arbitrary
        let colProps' = if NotNull `elem` colProps then colProps else NotNull : colProps
            acColumn = col{colProps = colProps'}
            acDefault = Nothing
        return . not . isValidOperation $ AddColumn{..}
  ]

newtype MigrationPath = MigrationPath { getOpPaths :: [OperationPath] }
  deriving (Show)

instance Arbitrary MigrationPath where
  arbitrary = do
    Positive maxVersion <- arbitrary
    let versions = [0..maxVersion]
        opPaths = zip versions $ tail versions
    -- order should not matter
    MigrationPath <$> shuffle opPaths

-- | Validate an Operation.
isValidOperation :: Migrateable op => op -> Bool
isValidOperation = isRight . validateOperation

-- | Validate OperationPaths in a Migration.
isValidMigrationPath :: MigrationPath -> Bool
isValidMigrationPath = isRight . validateMigration . map mkOperation . getOpPaths
  where
    mkOperation path = Operation path NoOp

-- | Duplicate some elements in the given list.
mapSomeDupl :: [a] -> Gen [a]
mapSomeDupl = fmap concat . mapSome dupl . map pure
  where
    dupl [x] = [x, x]
    dupl _ = error "unreachable"
