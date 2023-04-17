{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.IlcTest (ilcTests) where

import Data.Incremental
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Test.Cardano.Data (plusBinary, plusUnary, propExtend, propZero)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- ==================================================================================
-- These are standins for Coin and DRep which we can't import here

newtype MockCoin = MockCoin Integer
  deriving (Eq, Show, Ord)

instance Semigroup MockCoin where
  (MockCoin n) <> (MockCoin m) = MockCoin (n + m)

instance Monoid MockCoin where
  mempty = MockCoin 0

instance ILC MockCoin where
  newtype Diff MockCoin = DeltaMockCoin Integer
    deriving (Eq, Show)
  applyDiff (MockCoin n) (DeltaMockCoin m) = MockCoin (n + m)
  zero = DeltaMockCoin 0
  extend (DeltaMockCoin n) (DeltaMockCoin m) = DeltaMockCoin (n + m)
  totalDiff _ = zero

newtype Rep = Rep String
  deriving (Eq, Ord, Show)

instance Arbitrary Rep where
  arbitrary =
    Rep <$> do
      a <- choose ('A', 'Z')
      b <- choose ('a', 'z')
      c <- choose ('0', '9')
      pure [a, b, c]

instance Arbitrary (Diff MockCoin) where
  arbitrary = DeltaMockCoin <$> arbitrary

instance Arbitrary MockCoin where
  arbitrary = MockCoin <$> arbitrary

-- ==================================================================================
-- derivative of a unary function

sumCoins :: Map Int MockCoin -> MockCoin
sumCoins xs = Map.foldl' accum (MockCoin 0) xs
  where
    accum (MockCoin i) (MockCoin j) = MockCoin (i + j)

sumCoins' :: Map Int MockCoin -> Diff (Map Int MockCoin) -> Diff MockCoin
sumCoins' m (Dn mb) = DeltaMockCoin $ Map.foldlWithKey' accum 0 mb
  where
    accum ans k DeleteD = case Map.lookup k m of
      Nothing -> ans
      Just (MockCoin i) -> ans - i
    accum ans k (WriteD (MockCoin i)) = case Map.lookup k m of
      Nothing -> ans + i
      Just (MockCoin j) -> ans + i - j

-- ==================================================================================
-- derivative of a binary function

changeMockCoin :: MockCoin -> MockCoin -> MockCoin
changeMockCoin (MockCoin n) (MockCoin m) = MockCoin (m * n)

changeCoin' :: MockCoin -> Diff MockCoin -> MockCoin -> Diff MockCoin -> Diff MockCoin
changeCoin' (MockCoin n) (DeltaMockCoin i) (MockCoin m) (DeltaMockCoin j) =
  DeltaMockCoin (n * j + m * i + i * j)

-- ================================================

insertDTest :: Int -> MockCoin -> Map Int MockCoin -> Expectation
insertDTest k v m = applyDiff m (insertD k v) `shouldBe` Map.insert k v m

deleteDTest :: Int -> Map Int MockCoin -> Expectation
deleteDTest k m = applyDiff m (deleteD k) `shouldBe` Map.delete k m

lookupDTest :: Int -> Map Int MockCoin -> Diff (Map Int MockCoin) -> Expectation
lookupDTest k m md = lookupD k m md `shouldBe` Map.lookup k (applyDiff m md)

insertMTest :: Int -> Diff MockCoin -> Map Int MockCoin -> Expectation
insertMTest k v m = applyDiff (MM m) (insertM k v) `shouldBe` monoidInsert k (applyDiff mempty v) (MM m)

deleteMTest :: Int -> Map Int MockCoin -> Expectation
deleteMTest k m = applyDiff (MM m) (deleteM k) `shouldBe` MM (Map.delete k m)

combMTest :: Int -> Diff MockCoin -> Map Int MockCoin -> Expectation
combMTest k v m = applyDiff (MM m) (combineM k v) `shouldBe` monoidInsertWith k (applyDiff mempty v) (MM m)

-- ================================================
-- Property tests

ilcTests :: Spec
ilcTests = describe "ILC tests" $ do
  describe "Coin" $ do
    propZero (arbitrary @MockCoin)
    propExtend (arbitrary @MockCoin) (arbitrary @(Diff MockCoin))

  describe "Map cred Coin" $ do
    propZero (arbitrary @(Map Int MockCoin))
    propExtend (arbitrary @(Map Int MockCoin)) (arbitrary @(Diff (Map Int MockCoin)))

  describe "MonoidMap cred Coin" $ do
    propZero (arbitrary @(MonoidMap Int MockCoin))
    propExtend (arbitrary @(MonoidMap Int MockCoin)) (arbitrary @(Diff (MonoidMap Int MockCoin)))

  describe "Map cred Rep" $ do
    propZero (arbitrary @(Map Int Rep))
    propExtend (arbitrary @(Map Int Rep)) (arbitrary @(Diff (Map Int Rep)))

  describe "Total (Int,Bool)" $ do
    propZero (arbitrary @(Total (Int, Bool)))
    propExtend (arbitrary @(Total (Int, Bool))) (arbitrary @(Diff (Total (Int, Bool))))

  describe "Unary functions" $
    prop "sumCoins' is derivative of unary function sumCoins" $
      plusUnary sumCoins sumCoins'

  describe "Binary functions" $ do
    prop "changeCoin' is derivative of changeCoin" $
      plusBinary changeMockCoin changeCoin' arbitrary arbitrary arbitrary arbitrary

  describe "Functions on Diff(Map k v)" $ do
    prop "insertD test" insertDTest
    prop "deleteD test" deleteDTest
    prop "lookupD test" lookupDTest

  describe "Functions on Diff(MonoidMap k v)" $ do
    prop "insertM test" insertMTest
    prop "deleteM test" deleteMTest
    prop "combineM test" combMTest

-- To run theses tests in ghci, uncomment and type 'main'
-- main = hspec $ ilcTests
