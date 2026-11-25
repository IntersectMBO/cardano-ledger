{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.State.SnapShotsSpec (spec) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  knownNonZeroBounded,
  nonZeroOr,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.State (maxPool')
import Data.Ratio ((%))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.State.SnapShots

spec :: Spec
spec = describe "maxPool'" $ do
  prop "without a leverage cap the pre-Dijkstra reward pot is reproduced exactly" $ do
    a0 <- arbitrary
    nOpt <- genNOpt
    r <- arbitrary
    (sigma, pR) <- genSigmaAndRelativePledge nOpt
    pure $ maxPool' a0 nOpt r sigma pR noLeverageCap === preDijkstraMaxPool a0 nOpt r sigma pR

  prop "a leverage cap of L is the same as capping the relative stake at L times the pledge" $ do
    a0 <- arbitrary
    nOpt <- genNOpt
    r <- arbitrary
    (sigma, pR) <- genSigmaAndRelativePledge nOpt
    l <- genMaxPledgeLeverage
    pure $
      maxPool' a0 nOpt r sigma pR (leverageCap l)
        === maxPool' a0 nOpt r (min sigma (unboundRational l * pR)) pR noLeverageCap

  prop "a leverage cap never increases the reward pot" $ do
    a0 <- arbitrary
    nOpt <- genNOpt
    r <- arbitrary
    (sigma, pR) <- genSigmaAndRelativePledge nOpt
    l <- genMaxPledgeLeverage
    pure $ maxPool' a0 nOpt r sigma pR (leverageCap l) <= maxPool' a0 nOpt r sigma pR noLeverageCap

  prop "the reward pot is monotonic in the leverage cap" $ do
    a0 <- arbitrary
    nOpt <- genNOpt
    r <- arbitrary
    (sigma, pR) <- genSigmaAndRelativePledge nOpt
    l1 <- genMaxPledgeLeverage
    l2 <- genMaxPledgeLeverage
    pure $
      maxPool' a0 nOpt r sigma pR (leverageCap (min l1 l2))
        <= maxPool' a0 nOpt r sigma pR (leverageCap (max l1 l2))

  prop "a cap that does not bind gives the same reward pot as no cap at all" $ do
    a0 <- arbitrary
    nOpt <- genNOpt
    r <- arbitrary
    (sigma, pR) <- genSigmaAndRelativePledge nOpt
    l <- genMaxPledgeLeverage
    let nonBinding = unboundRational l * pR >= min sigma (z0Of nOpt)
        capped = maxPool' a0 nOpt r sigma pR (leverageCap l)
        uncapped = maxPool' a0 nOpt r sigma pR noLeverageCap
    pure $
      checkCoverage $
        cover 20 nonBinding "non-binding cap" $
          cover 20 (not nonBinding) "binding cap" $
            if nonBinding then capped === uncapped else property True

  prop "a pool with no pledge gets no rewards whenever a cap is in place" $ do
    a0 <- arbitrary
    nOpt <- genNOpt
    r <- arbitrary
    (sigma, _) <- genSigmaAndRelativePledge nOpt
    l <- genMaxPledgeLeverage
    pure $ maxPool' a0 nOpt r sigma 0 (leverageCap l) === Coin 0

  it "the cap binds at exactly L times the relative pledge" $ do
    -- With a0 = 0 the whole pot collapses to floor(r * σ'), so the effect of the
    -- cap can be read straight off:
    --   k = 10          => z0 = 1/10
    --   σ = z0          => saturated, so without a cap σ' = 1/10
    --   relativePledge  = 1/1000
    --   L = 50          => cap = L·relativePledge = 50/1000 = 1/20, which binds below z0
    let a0 = 0 %! 1
        nOpt = 10 `nonZeroOr` knownNonZeroBounded @1
        r = Coin 1000
        sigma = 1 % 10
        pR = 1 % 1000
        l = 50 %! 1
    -- uncapped: floor(1000 * 1/10) = 100
    maxPool' a0 nOpt r sigma pR noLeverageCap `shouldBe` Coin 100
    -- capped:   floor(1000 * 1/20) = 50
    maxPool' a0 nOpt r sigma pR (leverageCap l) `shouldBe` Coin 50
