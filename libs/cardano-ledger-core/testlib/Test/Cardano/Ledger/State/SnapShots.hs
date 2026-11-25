{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.State.SnapShots (
  preDijkstraMaxPool,
  z0Of,
  genNOpt,
  genSigmaAndRelativePledge,
  genMaxPledgeLeverage,
  leverageCap,
  noLeverageCap,
) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  NonNegativeInterval,
  NonZero,
  StrictMaybe (..),
  knownNonZeroBounded,
  nonZeroOr,
  unNonZero,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (MaxPledgeLeverage (..))
import Data.Ratio ((%))
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
