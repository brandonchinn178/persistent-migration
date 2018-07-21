{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Property (testProperties) where

import Control.Applicative (liftA2)
import Data.Either (isRight)
import Database.Persist.Migration
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Utils.QuickCheck (CreateTable'(..), Identifier(..), mapSome, toOperation)

-- | Run tests related to migration validaton.
testProperties :: TestTree
testProperties = testGroup "properties"
  [ testProperty "Valid migration" $ forAll arbitrary isValidOperationPaths
  , testProperty "Invalid decreasing migration" $
      forAll arbitrary $ \(OperationPaths opPaths) -> do
        opPaths' <- mapSome (\(a, b) -> (b, a)) opPaths
        return . not . isValidOperationPaths $ OperationPaths opPaths'
  , testProperty "Invalid duplicate operation path" $
      forAll arbitrary $ \(OperationPaths opPaths) -> do
        opPaths' <- mapSomeDupl opPaths
        return . not . isValidOperationPaths $ OperationPaths opPaths'
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
      forAll arbitrary $ \ct@CreateTable'{..} -> do
        constraints <- mapSomeDupl ctConstraints
        return . not . isValidOperation . toOperation $ ct{ctConstraints = constraints}
  , testProperty "Constraint references non-existent column" $
      forAll arbitrary $ \ct@CreateTable'{..} -> do
        let existing = map (Identifier . colName) ctSchema
            genConstraint = do
              Identifier name' <- arbitrary
              cols <- map unIdent <$> listOf1 (arbitrary `suchThat` (`notElem` existing))
              elements [PrimaryKey cols, Unique name' cols]
        newConstraints <- listOf1 genConstraint
        return . not . isValidOperation . toOperation $
          ct{ctConstraints = ctConstraints ++ newConstraints}
  , testProperty "Duplicate ColumnProps in AddColumn" $
      forAll arbitrary $ \col@Column{colProps} -> do
        Identifier table <- arbitrary
        colProps' <- mapSomeDupl colProps
        let column = col{colProps = colProps'}
            colDefault = Nothing
        return $ not (null colProps) ==> not (isValidOperation AddColumn{..})
  , testProperty "Non-null AddColumn without default" $
      forAll arbitrary $ \col@Column{colProps} -> do
        Identifier table <- arbitrary
        let colProps' = if NotNull `elem` colProps then colProps else NotNull : colProps
            column = col{colProps = colProps'}
            colDefault = Nothing
        return . not . isValidOperation $ AddColumn{..}
  ]

newtype OperationPaths = OperationPaths { getOpPaths :: [OperationPath] }
  deriving (Show)

instance Arbitrary OperationPaths where
  arbitrary = do
    Positive maxVersion <- arbitrary
    let versions = [0..maxVersion]
        opPaths = zip versions $ tail versions
    -- order should not matter
    OperationPaths <$> shuffle opPaths

-- | Validate an Operation.
isValidOperation :: Operation -> Bool
isValidOperation = isRight . validateOperation

-- | Validate OperationPaths in a Migration.
isValidOperationPaths :: OperationPaths -> Bool
isValidOperationPaths = isRight . validateMigration . map mkOperation . getOpPaths
  where
    mkOperation path = path := []

-- | Duplicate some elements in the given list.
mapSomeDupl :: [a] -> Gen [a]
mapSomeDupl = fmap concat . mapSome dupl . map pure
  where
    dupl [x] = [x, x]
    dupl _ = error "unreachable"
