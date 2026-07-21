{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the CIP-50 pledge-leverage parameter.
module Test.Cardano.Ledger.Dijkstra.RewardsSpec (spec) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  BoundedRational (..),
  NonNegativeInterval,
  NonZero,
  ProtVer,
  StrictMaybe (..),
  knownNonZeroBounded,
  nonZero,
  nonZeroOr,
  unNonZero,
 )
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError)
import Cardano.Ledger.Conway.PParams (ppuWellFormed)
import Cardano.Ledger.Core (
  emptyPParams,
  emptyPParamsUpdate,
  ppA0L,
  ppMaxLeverageFactorG,
  ppNOptL,
  ppProtocolVersionL,
 )
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (
  MaxLeverageFactor (..),
  cip50MaxLeverageFactor,
  cip50MinLeverageFactor,
  ppMaxLeverageFactorL,
  ppuMaxLeverageFactorL,
 )
import Cardano.Ledger.Plutus.ToPlutusData (fromPlutusData, toPlutusData)
import Cardano.Ledger.Shelley.Rewards (PoolRewardInfo (..), mkPoolRewardInfo)
import Cardano.Ledger.State (StakePoolSnapShot (..), maxPool, maxPool')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Word (Word16)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

-- | A frozen copy of the pre-CIP-50 Shelley reward-pot formula, used to ensure
-- via property testing that the new formula is exactly the same as the previous
-- one in the case where maxLeverageFactor is None.
preCip50MaxPool ::
  NonNegativeInterval ->
  NonZero Word16 ->
  Coin ->
  Rational ->
  Rational ->
  Coin
preCip50MaxPool a0 nOpt (Coin r) sigma pR = Coin $ floor (factor1 * factor2)
  where
    z0 = 1 % toInteger (unNonZero nOpt)
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = (r % 1) / (1 + unboundRational a0)
    factor2 = sigma' + p' * unboundRational a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | @z0 = 1/k@, the global saturation point.
z0Of :: NonZero Word16 -> Rational
z0Of nOpt = 1 % toInteger (unNonZero nOpt)

-- | A 'NonNegativeInterval' from a 'Rational'
nni :: Rational -> NonNegativeInterval
nni = fromMaybe (error "nni: out of range") . boundRational

genNOpt :: Gen (NonZero Word16)
genNOpt = do
  n <- choose (1, 2000) :: Gen Int
  pure (fromIntegral n `nonZeroOr` knownNonZeroBounded @1)

-- | A relative quantity in [0, 1] (stands in for σ or relativePledge).
genUnitRational :: Gen Rational
genUnitRational = do
  d <- choose (1, 1_000_000) :: Gen Integer
  n <- choose (0, d)
  pure (n % d)

-- | A leverage factor for the CIP-50 range [1, 10000].
genLeverage :: Gen NonNegativeInterval
genLeverage = do
  -- tenths, so we exercise fractional leverage too
  tenths <- choose (10, 100_000) :: Gen Integer
  pure (nni (tenths % 10))

genLeverageMaybe :: Gen (StrictMaybe NonNegativeInterval)
genLeverageMaybe = frequency [(1, pure SNothing), (4, SJust <$> genLeverage)]

spec :: Spec
spec = do
  describe "CIP-50 pledge-leverage cap in maxPool'" $ do
    prop "the no-cap (SNothing) path reproduces the frozen pre-CIP-50 formula exactly" $ do
      a0 <- arbitrary
      nOpt <- genNOpt
      r <- arbitrary
      sigma <- genUnitRational
      pR <- genUnitRational
      pure $ maxPool' a0 nOpt r sigma pR SNothing === preCip50MaxPool a0 nOpt r sigma pR

    prop "a leverage cap never increases the reward pot" $ do
      a0 <- arbitrary
      nOpt <- genNOpt
      r <- arbitrary
      sigma <- genUnitRational
      pR <- genUnitRational
      l <- genLeverage
      pure $ maxPool' a0 nOpt r sigma pR (SJust l) <= maxPool' a0 nOpt r sigma pR SNothing

    prop "the reward pot is monotonic in L (a larger cap never lowers it)" $ do
      a0 <- arbitrary
      nOpt <- genNOpt
      r <- arbitrary
      sigma <- genUnitRational
      pR <- genUnitRational
      l1 <- genLeverage
      l2 <- genLeverage
      pure $
        maxPool' a0 nOpt r sigma pR (SJust (min l1 l2))
          <= maxPool' a0 nOpt r sigma pR (SJust (max l1 l2))

    prop "a non-binding cap (L·relativePledge ≥ min σ z0) equals no cap" $ do
      a0 <- arbitrary
      nOpt <- genNOpt
      r <- arbitrary
      sigma <- genUnitRational
      pR <- genUnitRational
      l <- genLeverage
      let nonBinding = unboundRational l * pR >= min sigma (z0Of nOpt)
          capped = maxPool' a0 nOpt r sigma pR (SJust l)
          uncapped = maxPool' a0 nOpt r sigma pR SNothing
      pure $
        checkCoverage $
          cover 20 nonBinding "non-binding cap (equality asserted)" $
            if nonBinding then capped === uncapped else property True

    prop "a zero-pledge pool earns a zero reward pot when a cap is set" $ do
      a0 <- arbitrary
      nOpt <- genNOpt
      r <- arbitrary
      sigma <- genUnitRational
      l <- genLeverage
      pure $ maxPool' a0 nOpt r sigma 0 (SJust l) === Coin 0

    it "cap binds at exactly L·relativePledge (hand-computed pot)" $ do
      -- a nice example of the semantics of the cap.
      --
      -- With a0 = 0 the whole pot collapses to floor(r * σ'),
      -- so we can read the cap straight off:
      --   k = 10          => z0 = 1/10
      --   σ = z0          => saturated, so the k-cap alone would give σ' = 1/10
      --   relativePledge  = 1/1000
      --   L = 50          => cap = L·relativePledge = 50/1000 = 1/20, which binds below z0
      let a0 = nni 0
          nOpt = 10 `nonZeroOr` knownNonZeroBounded @1
          r = Coin 1000
          sigma = 1 % 10
          pR = 1 % 1000
          l = nni 50
      -- uncapped: floor(1000 * 1/10) = 100
      maxPool' a0 nOpt r sigma pR SNothing `shouldBe` Coin 100
      -- capped:   floor(1000 * 1/20) = 50
      maxPool' a0 nOpt r sigma pR (SJust l) `shouldBe` Coin 50

    it "caps an over-leveraged saturated pool below its no-cap pot" $ do
      -- A saturated pool (σ = z0) with tiny relative pledge: the leverage cap binds well
      -- below z0, so the capped pot is strictly smaller than the uncapped pot.
      let nOpt = 500 `nonZeroOr` knownNonZeroBounded @1
          z0 = z0Of nOpt
          a0 = nni (3 % 10)
          r = Coin 1_000_000_000
          sigma = z0
          pR = z0 / 1000 -- pledge is 0.1% of what L=100 would require at saturation
          l = nni (100 % 1)
          capped = maxPool' a0 nOpt r sigma pR (SJust l)
          uncapped = maxPool' a0 nOpt r sigma pR SNothing
      capped `shouldSatisfy` (< uncapped)

  describe "CIP-50 maxLeverageFactor PParams wiring" $ do
    it "defaults to no cap (SNothing) so Dijkstra replays Conway rewards unchanged" $
      (emptyPParams @DijkstraEra ^. ppMaxLeverageFactorG) `shouldBe` SNothing

    prop "ppMaxLeverageFactorG reflects the value set via ppMaxLeverageFactorL" $ do
      v <- genLeverageMaybe
      pure $
        ((emptyPParams @DijkstraEra & ppMaxLeverageFactorL .~ MaxLeverageFactor v) ^. ppMaxLeverageFactorG)
          === v

    prop "maxPool routes the pparam cap through to maxPool'" $ do
      v <- genLeverageMaybe
      r <- arbitrary
      sigma <- genUnitRational
      pR <- genUnitRational
      let pp = emptyPParams @DijkstraEra & ppMaxLeverageFactorL .~ MaxLeverageFactor v
          a0 = pp ^. ppA0L
          nOpt = (pp ^. ppNOptL) `nonZeroOr` knownNonZeroBounded @1
      pure $ maxPool pp r sigma pR === maxPool' a0 nOpt r sigma pR v

  describe "CIP-50 ppuWellFormed enforces the leverage-factor range" $ do
    let pv :: ProtVer
        pv = emptyPParams @DijkstraEra ^. ppProtocolVersionL
        withCap x = emptyPParamsUpdate @DijkstraEra & ppuMaxLeverageFactorL .~ SJust (MaxLeverageFactor x)
    it "accepts the lower bound L = cip50MinLeverageFactor" $
      ppuWellFormed pv (withCap (SJust (nni cip50MinLeverageFactor))) `shouldBe` True
    it "accepts the upper bound L = cip50MaxLeverageFactor" $
      ppuWellFormed pv (withCap (SJust (nni cip50MaxLeverageFactor))) `shouldBe` True
    it "accepts removing the cap (SNothing)" $
      ppuWellFormed pv (withCap SNothing) `shouldBe` True
    it "rejects L = 0" $
      ppuWellFormed pv (withCap (SJust (nni 0))) `shouldBe` False
    it "rejects a fractional L just below the lower bound" $
      ppuWellFormed pv (withCap (SJust (nni (cip50MinLeverageFactor - (1 % 2))))) `shouldBe` False
    it "rejects L just above the upper bound" $
      ppuWellFormed pv (withCap (SJust (nni (cip50MaxLeverageFactor + 1)))) `shouldBe` False

  describe "CIP-50 end-to-end reward pot via mkPoolRewardInfo" $
    prop "a binding cap lowers the pool pot the reward pipeline computes" $ do
      snap0 <- arbitrary
      poolId <- arbitrary
      let totalStake = Coin 1_000_000
          totalActiveStake = fromMaybe (error "totalActiveStake: zero") (nonZero (Coin 1_000_000))
          pledge = Coin 100
          -- σ = 50000/1e6 = 0.05, below z0 = 1/10 (k=10), so the k-cap does not bind.
          -- relativePledge = 100/1e6 = 1e-4, so L=100 gives a cap of 0.01 < 0.05: it binds.
          snap =
            snap0
              { spssStake = compactCoinOrError (Coin 50_000)
              , spssStakeRatio = 50_000 % 1_000_000
              , spssPledge = pledge
              , -- gate in mkPoolRewardInfo: the maxPool' branch is only taken when
                -- pledge <= self-delegated owner stake
                spssSelfDelegatedOwnersStake = pledge
              }
          blocks = BlocksMade (Map.singleton poolId 1)
          blocksTotal = 1
          r = Coin 1_000_000_000
          base = emptyPParams @DijkstraEra & ppNOptL .~ 10
          ppNoCap = base & ppMaxLeverageFactorL .~ MaxLeverageFactor SNothing
          ppCap = base & ppMaxLeverageFactorL .~ MaxLeverageFactor (SJust (nni 100))
          potOf pp =
            case mkPoolRewardInfo pp r blocks blocksTotal totalStake totalActiveStake poolId snap of
              Right info -> poolPot info
              Left _ -> error "expected the pool to have made a block"
      pure $ potOf ppCap `shouldSatisfy` (< potOf ppNoCap)

  describe "CIP-50 maxLeverageFactor ToPlutusData" $
    prop "round-trips the optional leverage value" $ do
      x <- MaxLeverageFactor <$> genLeverageMaybe
      pure $ fromPlutusData (toPlutusData x) === Just x
