{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.State.SnapShotsSpec (spec) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  NonNegativeInterval,
  NonZero,
  StrictMaybe (..),
  addEpochInterval,
  knownNonZeroBounded,
  nonZeroOr,
  unNonZero,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (MaxPledgeLeverage (..))
import Cardano.Ledger.State (
  BlsKeyState (..),
  IndividualPoolStake (..),
  PoolDistr (..),
  SnapShot (..),
  StakePoolSnapShot (..),
  expireBlsKeys,
  maxPool',
 )
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.VMap as VMap
import Data.Word (Word16)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Rational ((%!))

-- | A frozen copy of the reward pot formula as it was before the maximum pledge
-- leverage was introduced in Dijkstra. Used to ensure that a pool that is not
-- subject to a leverage cap is rewarded exactly as it was in the previous eras.
preDijkstraMaxPool ::
  NonNegativeInterval ->
  NonZero Word16 ->
  Coin ->
  Rational ->
  Rational ->
  Coin
preDijkstraMaxPool a0 nOpt (Coin r) sigma pR = Coin $ floor (factor1 * factor2)
  where
    z0 = z0Of nOpt
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = (r % 1) / (1 + unboundRational a0)
    factor2 = sigma' + p' * unboundRational a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | @z0 = 1/k@, the global saturation point.
z0Of :: NonZero Word16 -> Rational
z0Of nOpt = 1 % toInteger (unNonZero nOpt)

genNOpt :: Gen (NonZero Word16)
genNOpt = do
  n <- choose (1, 2000)
  pure (n `nonZeroOr` knownNonZeroBounded @1)

-- | A pool's relative stake σ together with its relative pledge.
--
-- Both are generated the way they actually occur on chain, otherwise the leverage
-- cap almost never binds and the properties below become vacuous:
--
-- * σ is generated around the saturation point @z0 = 1/k@, rather than uniformly in
--   [0, 1], since @z0@ is where the reward pot stops growing.
-- * the pledge is a fraction of the stake, since a pool that pledges more than its
--   own stake receives no rewards at all. The leverage of the pool, σ over the
--   pledge, ranges over the whole interval that the maximum pledge leverage can be
--   set to, and beyond.
genSigmaAndRelativePledge :: NonZero Word16 -> Gen (Rational, Rational)
genSigmaAndRelativePledge nOpt = do
  saturation <- choose (0, 2_000)
  leverage <- choose (1, 20_000)
  let sigma = z0Of nOpt * (saturation % 1_000)
  pure (sigma, sigma / (leverage % 1))

genMaxPledgeLeverage :: Gen NonNegativeInterval
genMaxPledgeLeverage = do
  l <- choose (1, 10_000)
  pure (l %! 1)

-- | No maximum pledge leverage, ie. the behavior of all the eras prior to Dijkstra.
noLeverageCap :: MaxPledgeLeverage
noLeverageCap = MaxPledgeLeverage SNothing

leverageCap :: NonNegativeInterval -> MaxPledgeLeverage
leverageCap = MaxPledgeLeverage . SJust

spec :: Spec
spec = do
  describe "expireBlsKeys" $ do
    prop "changes nothing but the BLS keys" $
      \(snapShot :: SnapShot) (pd :: PoolDistr) epochNo maxKeyAge ->
        let PoolDistr distr' totalActiveStake' = expireBlsKeys epochNo maxKeyAge snapShot pd
            scrub ips = ips {individualPoolStakeBls = SNothing}
         in totalActiveStake'
              === pdTotalActiveStake pd
              .&&. (scrub <$> distr')
              === (scrub <$> unPoolDistr pd)

    prop "expiry takes effect exactly maxKeyAgeEpochs after registration" $
      \(snapShot :: SnapShot) (ips :: IndividualPoolStake) (bks :: BlsKeyState) maxKeyAge ->
        let boundary = addEpochInterval (bksRegisteredIn bks) maxKeyAge
            withKey =
              snapShot
                { ssStakePoolsSnapShot =
                    VMap.map (\spss -> spss {spssBlsKey = SJust bks}) (ssStakePoolsSnapShot snapShot)
                }
            pd = PoolDistr (ips <$ VMap.toMap (ssStakePoolsSnapShot snapShot)) (ssTotalActiveStake snapShot)
            keysAt e = individualPoolStakeBls <$> Map.elems (unPoolDistr (expireBlsKeys e maxKeyAge withKey pd))
         in conjoin (map (=== SNothing) (keysAt boundary))
              .&&. ( boundary
                       > bksRegisteredIn bks ==> conjoin (map (=== SJust (bksKey bks)) (keysAt (bksRegisteredIn bks)))
                   )

  describe "maxPool'" $ do
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
