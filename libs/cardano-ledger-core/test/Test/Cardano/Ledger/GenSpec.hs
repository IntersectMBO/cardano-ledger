{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.GenSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.List (sort)
import Data.Ratio ((%))
import Data.Semigroup (Sum (..))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Mutable qualified as VM
import Test.Cardano.Ledger.Core.Gen
import Test.Cardano.Ledger.Imp.Common hiding (sample, scale, tolerance)
import Test.ImpSpec (ImpM (..), impAnn)

spec :: Spec
spec = do
  withImpInit $ do
    describe "Generation" $ do
      describe "Coin" $ do
        it @DefaultImpSpec "has appropriate statistical behavior" $ do
          impAnn "Check `sampleMeanVariance` implementation" $ do
            sampleMeanVariance [1 .. 20] `shouldBe` (10.5, 33.25)

          impAnn "Check `sampleQuantile` implementation" $ do
            let deciles xs =
                  let sample = SortedVector . V.fromList $ sort xs :: SortedVector Integer
                   in [sampleQuantile q sample | q <- [0.0, 0.1 .. 1.0]]
            deciles [1 .. 3] `shouldBe` [0, 0.3 .. 3]
            deciles [1 .. 4] `shouldBe` [0, 0.4 .. 4]
            deciles [1 .. 19] `shouldBe` [0, 1.9 .. 19]
            deciles [1 .. 20] `shouldBe` [0, 2.0 .. 20]

          -- Generate coins to be used in multiple checks
          ocs <- sortedSample 100_000 $ unCoin <$> genOutputCoin
          bcs <- sortedSample 100_000 $ unCoin <$> genBalanceCoin

          impAnn "Values are in bounds" $ do
            let outputRange = bimap unCoin unCoin outputCoinGenRange
                balanceRange = bimap unCoin unCoin balanceCoinGenRange
            minimum ocs `shouldSatisfy` inRange outputRange
            maximum ocs `shouldSatisfy` inRange outputRange
            minimum bcs `shouldSatisfy` inRange balanceRange
            maximum bcs `shouldSatisfy` inRange balanceRange

          let ada = (/ 1_000_000)

          let (ada -> mocs, ada -> socs) = sampleMeanStdev ocs
              (ada -> mbcs, ada -> sbcs) = sampleMeanStdev bcs

          impAnn "Sum of many won't exceed supply" $ do
            let supply = 45e9
            mocs * 1_000_000 `shouldSatisfy` (<= supply)
            mbcs * 1_000 `shouldSatisfy` (<= supply)

          -- If these fail occasionally, just increase the tolerance factors
          impAnn "Mean and standard deviation are reasonable" $ do
            impAnn "mocs" $ (mocs, 26_000) `shouldSatisfy` relativelyClose 0.25
            impAnn "socs" $ (socs, 165_000) `shouldSatisfy` relativelyClose 0.25
            impAnn "mbcs" $ (mbcs, 26_000_000) `shouldSatisfy` relativelyClose 0.25
            impAnn "sbcs" $ (sbcs, 165_000_000) `shouldSatisfy` relativelyClose 0.25

          -- These tolerance factors should be adequate
          impAnn "Quantiles are sufficiently accurate" $ do
            for_ ([0.1, 0.2 .. 0.9] <> [0.99]) $ \p -> do
              impAnn (show p) $ do
                (round $ sampleQuantile p ocs, unCoin $ outputCoinGenQuantile p)
                  `shouldSatisfy` relativelyClose 0.25
                (round $ sampleQuantile p bcs, unCoin $ balanceCoinGenQuantile p)
                  `shouldSatisfy` relativelyClose 0.25

-- Testing helpers

relativelyClose :: Real a => Rational -> (a, a) -> Bool
relativelyClose tolerance (x, y) = inRange (recip scale, scale) $ toRational x / toRational y
  where
    scale = 1 + tolerance

inRange :: Ord a => (a, a) -> a -> Bool
inRange (lo, hi) x = lo <= x && x <= hi

-- Stats helpers

sampleMeanStdev :: Foldable f => f Integer -> (Double, Double)
sampleMeanStdev = bimap fromRational (sqrt . fromRational) . sampleMeanVariance

sampleMeanVariance :: Foldable f => f Integer -> (Rational, Rational)
sampleMeanVariance xs
  | null xs = error "Cannot calculate the mean and variance of an empty sample"
  | otherwise =
      let (getSum -> n, getSum -> sx, getSum -> sx2) = foldMap (\x -> (Sum 1, Sum x, Sum $ x * x)) xs
          mean = sx % n
          variance = sx2 % n - mean * mean
       in (mean, variance)

newtype SortedVector a = SortedVector (Vector a)
  deriving newtype (Foldable)

sampleQuantile :: (Enum a, Real a) => Rational -> SortedVector a -> Rational
sampleQuantile q (SortedVector v)
  | null v = error "Cannot calculate a quantile of an empty sample"
  | otherwise = lo + interpolate * (hi - lo)
  where
    n = V.length v
    (indexFloor, interpolate) = properFraction $ q * toRational n - 1
    indexCeiling = if interpolate /= 0 then indexFloor + 1 else indexFloor
    lo = toRational $ element indexFloor
    hi = toRational $ element indexCeiling
    element j
      | j < 0 = pred $ V.head v
      | j > n - 1 = succ $ V.last v
      | otherwise = v `V.unsafeIndex` j

deriving newtype instance PrimMonad (ImpM t)

sortedSample :: (PrimMonad m, Ord a) => Int -> m a -> m (SortedVector a)
sortedSample n gen
  | n <= 0 = error "Sample sizes must be positive"
  | otherwise = do
      v <- VM.replicateM n gen
      VA.sort v
      SortedVector <$> V.unsafeFreeze v
