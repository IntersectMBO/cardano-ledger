{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.SPORatifySpec (spec) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes ({-- EpochNo (..),--} StrictMaybe (..))

-- import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  -- GovAction (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifyState,
  Vote (..),
  -- pparamsUpdateThreshold,

  ensProtVerL,
  gasAction,
  rsEnactStateL,
  votingStakePoolThreshold,
 )
import Cardano.Ledger.Conway.Rules (
  spoAccepted,
  spoAcceptedRatio,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams, ppRewardAccount)

-- import Cardano.Ledger.Val ((<+>), (<->))
-- import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))

-- import qualified Data.Set as Set
-- import Data.Word (Word64)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()

-- import Test.Cardano.Ledger.Core.Rational ((%!))
import Lens.Micro ((^.))

spec :: Spec
spec = do
  describe "SPO Ratification" $ do
    -- correctThresholdsProp @Conway
    -- acceptedRatioProp @Conway
    noStakeProp @Conway
    allAbstainProp @Conway
    noVotesProp @Conway
    allYesProp @Conway
    noConfidenceProp @Conway

-- correctThresholdsProp ::
--   forall era.
--   ( ConwayEraPParams era
--   , Arbitrary (PParamsUpdate era)
--   ) =>
--   Spec
-- correctThresholdsProp = do
--   prop "PParamsUpdateThreshold always selects a threshold" $ \thresholds ppu -> do
--     let DRepVotingThresholds {..} = thresholds
--         allDRepThresholds =
--           Set.fromList
--             [ dvtPPNetworkGroup
--             , dvtPPEconomicGroup
--             , dvtPPTechnicalGroup
--             , dvtPPGovGroup
--             ]
--     when (ppu /= emptyPParamsUpdate) $
--       pparamsUpdateThreshold @era thresholds ppu `shouldSatisfy` (`Set.member` allDRepThresholds)
--     pparamsUpdateThreshold @era thresholds emptyPParamsUpdate `shouldBe` (0 %! 1)

-- acceptedRatioProp :: forall era. Era era => Spec
-- acceptedRatioProp = do
--   prop "SPO vote count for arbitrary vote ratios" $
--     forAll genRatios $ \ratios -> do
--       forAll (genTestData @era ratios) $
--         \(TestData {..}) -> do
--           let drepState =
--                 -- non-expired (active) dReps
--                 Map.fromList
--                   [(cred, DRepState (EpochNo 100) SNothing mempty mempty) | DRepCredential cred <- Map.keys distr]
--               ratifyEnv = (emptyRatifyEnv @era) {reDRepDistr = distr, reDRepState = drepState}
--               actual = dRepAcceptedRatio @era ratifyEnv votes InfoAction
--               -- Check the accepted min ratio is : yes/(total - abstain), or zero if everyone abstained
--               expected
--                 | totalStake == stakeAbstain <+> stakeAlwaysAbstain = 0
--                 | otherwise = unCoin stakeYes % unCoin (totalStake <-> stakeAbstain <-> stakeAlwaysAbstain)
--           actual `shouldBe` expected
--
--           -- This can be also expressed as: yes/(yes + no + not voted + noconfidence)
--           let expectedRephrased
--                 | stakeYes <+> stakeNo <+> stakeNotVoted <+> stakeNoConfidence == Coin 0 = 0
--                 | otherwise =
--                     unCoin stakeYes % unCoin (stakeYes <+> stakeNo <+> stakeNotVoted <+> stakeNoConfidence)
--           actual `shouldBe` expectedRephrased
--
--           let actualNoConfidence = dRepAcceptedRatio @era ratifyEnv votes (NoConfidence SNothing)
--               -- For NoConfidence action, we count the `NoConfidence` votes as Yes
--               expectedNoConfidence
--                 | totalStake == stakeAbstain <+> stakeAlwaysAbstain = 0
--                 | otherwise =
--                     unCoin (stakeYes <+> stakeNoConfidence)
--                       % unCoin (totalStake <-> stakeAbstain <-> stakeAlwaysAbstain)
--           actualNoConfidence `shouldBe` expectedNoConfidence
--
--           let allExpiredDreps =
--                 Map.fromList
--                   [(cred, DRepState (EpochNo 9) SNothing mempty mempty) | DRepCredential cred <- Map.keys distr]
--               actualAllExpired =
--                 dRepAcceptedRatio @era
--                   ( (emptyRatifyEnv @era)
--                       { reDRepDistr = distr
--                       , reDRepState = allExpiredDreps
--                       , reCurrentEpoch = EpochNo 10
--                       }
--                   )
--                   votes
--                   InfoAction
--           actualAllExpired `shouldBe` 0
--
--           -- Expire half of the DReps and check that the ratio is the same as if only the active DReps exist
--           let (activeDreps, expiredDreps) = splitAt (length distr `div` 2) (Map.keys distr)
--               activeDrepsState =
--                 Map.fromList
--                   [(cred, DRepState (EpochNo 10) SNothing mempty mempty) | DRepCredential cred <- activeDreps]
--               expiredDrepsState =
--                 Map.fromList
--                   [(cred, DRepState (EpochNo 3) SNothing mempty mempty) | DRepCredential cred <- expiredDreps]
--               someExpiredDrepsState = activeDrepsState `Map.union` expiredDrepsState
--
--               actualSomeExpired =
--                 dRepAcceptedRatio @era
--                   ( (emptyRatifyEnv @era)
--                       { reDRepDistr = distr
--                       , reDRepState = someExpiredDrepsState
--                       , reCurrentEpoch = EpochNo 5
--                       }
--                   )
--                   (votes `Map.union` Map.fromList [(cred, VoteYes) | DRepCredential cred <- expiredDreps])
--                   InfoAction
--
--           actualSomeExpired
--             `shouldBe` dRepAcceptedRatio @era
--               ( (emptyRatifyEnv @era)
--                   { reDRepDistr = distr
--                   , reDRepState = activeDrepsState
--                   , reCurrentEpoch = EpochNo 5
--                   }
--               )
--               votes
--               InfoAction

allAbstainProp ::
  forall era.
  ( Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (PParamsHKD Identity era)
  , ConwayEraPParams era
  ) =>
  Spec
allAbstainProp =
  prop @((RatifyEnv era, RatifyState era, GovActionState era) -> Property)
    "If all votes are abstain, accepted ratio is zero"
    $ \(re, rs, gas) -> forAll
      ( genTestData @era
          (Ratios {yes = 0, no = 0, abstain = 50 % 100, alwaysAbstain = 50 % 100, noConfidence = 0})
      )
      $ \TestData {..} ->
        spoAcceptedRatio
          @era
          re {reStakePoolDistr = distr, reDelegatees = delegatees, rePoolParams = poolParams}
          gas {gasStakePoolVotes = votes}
          (rs ^. rsEnactStateL . ensProtVerL)
          `shouldBe` 0

noConfidenceProp ::
  forall era.
  ( Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (PParamsHKD Identity era)
  , ConwayEraPParams era
  ) =>
  Spec
noConfidenceProp =
  prop @((RatifyEnv era, RatifyState era, GovActionState era) -> Property)
    "If all votes are no confidence, accepted ratio is zero"
    $ \(re, rs, gas) -> forAll
      ( genTestData @era
          (Ratios {yes = 0, no = 0, abstain = 0, alwaysAbstain = 0, noConfidence = 1 % 1})
      )
      $ \TestData {..} ->
        spoAcceptedRatio
          @era
          re {reStakePoolDistr = distr}
          gas {gasStakePoolVotes = votes}
          (rs ^. rsEnactStateL . ensProtVerL)
          `shouldBe` 0

noVotesProp ::
  forall era.
  ( Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (PParamsHKD Identity era)
  , ConwayEraPParams era
  ) =>
  Spec
noVotesProp =
  prop @((RatifyEnv era, RatifyState era, GovActionState era) -> Property)
    "If there are no votes, accepted ratio is zero"
    $ \(re, rs, gas) -> forAll
      ( genTestData @era
          (Ratios {yes = 0, no = 0, abstain = 0, alwaysAbstain = 0, noConfidence = 0})
      )
      $ \TestData {..} ->
        spoAcceptedRatio
          @era
          re {reStakePoolDistr = distr}
          gas {gasStakePoolVotes = votes}
          (rs ^. rsEnactStateL . ensProtVerL)
          `shouldBe` 0

allYesProp ::
  forall era.
  ( Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (PParamsHKD Identity era)
  , ConwayEraPParams era
  ) =>
  Spec
allYesProp =
  prop @((RatifyEnv era, RatifyState era, GovActionState era) -> Property)
    "If all vote yes, accepted ratio is 1 (unless there is no stake) "
    ( \(re, rs, gas) ->
        forAll
          ( genTestData @era
              (Ratios {yes = 100 % 100, no = 0, abstain = 0, alwaysAbstain = 0, noConfidence = 0})
          )
          ( \TestData {..} ->
              if fromCompact totalStake == Coin 0
                then
                  spoAcceptedRatio
                    @era
                    re {reStakePoolDistr = distr}
                    gas {gasStakePoolVotes = votes}
                    (rs ^. rsEnactStateL . ensProtVerL)
                    `shouldBe` 0
                else
                  spoAcceptedRatio
                    @era
                    re {reStakePoolDistr = distr}
                    gas {gasStakePoolVotes = votes}
                    (rs ^. rsEnactStateL . ensProtVerL)
                    `shouldBe` 1
          )
    )

noStakeProp ::
  forall era.
  ( Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (PParamsHKD Identity era)
  , ConwayEraPParams era
  ) =>
  Spec
noStakeProp =
  prop @((RatifyEnv era, RatifyState era, GovActionState era) -> IO ())
    "If there is no stake, accept iff threshold is zero"
    ( \(env, st, gas) ->
        spoAccepted
          @era
          env {reStakePoolDistr = PoolDistr Map.empty (fromJust . toCompact $ Coin 100)}
          st
          gas
          `shouldBe` votingStakePoolThreshold @era st (gasAction gas)
          == SJust minBound
    )

-- acceptedRatio :: forall era. TestData era -> Rational
-- acceptedRatio (TestData {..}) =
--   let activeDrepState =
--         -- non-expired dReps
--         Map.fromList
--           [(cred, DRepState (EpochNo 100) SNothing mempty mempty) | DRepCredential cred <- Map.keys distr]
--       ratifyEnv = (emptyRatifyEnv @era) {reDRepDistr = distr, reDRepState = activeDrepState}
--    in dRepAcceptedRatio @era ratifyEnv votes InfoAction

data TestData era = TestData
  { distr :: PoolDistr (EraCrypto era)
  , votes :: Map (KeyHash 'StakePool (EraCrypto era)) Vote
  , totalStake :: CompactForm Coin
  , stakeYes :: Coin
  , stakeNo :: Coin
  , stakeAbstain :: Coin
  , -- , stakeAlwaysAbstain :: Coin
    -- , stakeNoConfidence :: Coin
    stakeNotVoted :: Coin
  , delegatees :: Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era))
  , poolParams :: Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era))
  }
  deriving (Show)

data Ratios = Ratios
  { yes :: Rational
  , no :: Rational
  , abstain :: Rational
  , alwaysAbstain :: Rational
  , noConfidence :: Rational
  }
  deriving (Show)

-- Prepare the stake distribution and votes according to the given ratios.
genTestData ::
  forall era.
  Era era =>
  Ratios ->
  Gen (TestData era)
genTestData Ratios {yes, no, abstain, alwaysAbstain, noConfidence} = do
  let inPools = listOf1 (arbitrary @(KeyHash 'StakePool (EraCrypto era)))
  pools <- inPools
  let (poolsYes, poolsNo, poolsAbstain, poolsAlwaysAbstain, poolsNoConfidence, rest) = splitByPct yes no abstain alwaysAbstain noConfidence pools
  distr <- do
    vrf <- arbitrary
    let
      totalStake = length pools
      a = 1
      indivStake = IndividualPoolStake (toRational a / toRational totalStake) (CompactCoin a) vrf
    pure $
      PoolDistr
        ( Map.union
            (Map.fromList [(cred, indivStake) | cred <- poolsYes])
            $ Map.union
              (Map.fromList [(cred, indivStake) | cred <- poolsNo])
            $ Map.union
              (Map.fromList [(cred, indivStake) | cred <- poolsAbstain])
            $ Map.union
              (Map.fromList [(cred, indivStake) | cred <- poolsNoConfidence])
              (Map.fromList [(cred, indivStake) | cred <- poolsAlwaysAbstain])
        )
        (CompactCoin $ fromIntegral totalStake)

  poolParams <- do
    params <- arbitrary
    pure $ Map.fromList [(cred, params) | cred <- pools]
  let delegateesAA =
        Map.fromList
          . map (\(_, params) -> (raCredential $ ppRewardAccount params, DRepAlwaysAbstain))
          . Map.toList
          $ Map.filterWithKey (\k _ -> k `elem` poolsAlwaysAbstain) poolParams
      delegateesNC =
        Map.fromList
          . map (\(_, params) -> (raCredential $ ppRewardAccount params, DRepAlwaysNoConfidence))
          . Map.toList
          $ Map.filterWithKey (\k _ -> k `elem` poolsNoConfidence) poolParams

  let
    -- numPools = length pools
    -- alwaysAbstainPct :: Word64 = pct alwaysAbstain
    -- noConfidencePct :: Word64 = pct noConfidence
    -- distr =
    --   Map.alter
    --     (\case _ -> Just (CompactCoin noConfidencePct))
    --     DRepAlwaysNoConfidence
    --     . Map.alter
    --       (\case _ -> Just (CompactCoin alwaysAbstainPct))
    --       DRepAlwaysAbstain
    --     $ Map.fromList [(pool, CompactCoin 1) | pool <- pools]
    -- distr = Map.fromList [(pool, CompactCoin 1) | pool <- pools]
    notVotedStake = length rest
    votes =
      Map.union
        (Map.fromList [(cred, VoteYes) | cred <- poolsYes])
        $ Map.union
          (Map.fromList [(cred, VoteNo) | cred <- poolsNo])
          (Map.fromList [(cred, Abstain) | cred <- poolsAbstain])
  -- pct :: Integral a => Rational -> a
  -- pct r = ceiling (r * fromIntegral numPools)
  pure
    TestData
      { distr
      , votes
      , totalStake = pdTotalActiveStake distr
      , stakeYes = Coin (fromIntegral (length poolsYes))
      , stakeNo = Coin (fromIntegral (length poolsNo))
      , stakeAbstain = Coin (fromIntegral (length poolsAbstain))
      , -- , stakeAlwaysAbstain = Coin (fromIntegral alwaysAbstainPct)
        -- , stakeNoConfidence = Coin (fromIntegral noConfidencePct)
        stakeNotVoted = Coin (fromIntegral notVotedStake)
      , delegatees = Map.union delegateesAA delegateesNC
      , poolParams
      }
  where
    splitByPct ::
      Rational -> Rational -> Rational -> Rational -> Rational -> [a] -> ([a], [a], [a], [a], [a], [a])
    splitByPct r1 r2 r3 r4 r5 l =
      let
        size = fromIntegral $ length l
        (rs1, rest) = splitAt (ceiling (r1 * size)) l
        (rs2, rest') = splitAt (ceiling (r2 * size)) rest
        (rs3, rest'') = splitAt (ceiling (r3 * size)) rest'
        (rs4, rest''') = splitAt (ceiling (r4 * size)) rest''
        (rs5, rest'''') = splitAt (ceiling (r5 * size)) rest'''
       in
        (rs1, rs2, rs3, rs4, rs5, rest'''')

-- genRatios :: Gen Ratios
-- genRatios = do
--   (a, b, c, d, e, _) <- genPctsOf100
--   pure $ Ratios {yes = a, no = b, abstain = c, alwaysAbstain = d, noConfidence = e}
--
-- genPctsOf100 :: Gen (Rational, Rational, Rational, Rational, Rational, Rational)
-- genPctsOf100 = do
--   a <- choose (0, 100)
--   b <- choose (0, 100)
--   c <- choose (0, 100)
--   d <- choose (0, 100)
--   e <- choose (0, 100)
--   f <- choose (0, 100)
--   let s = a + b + c + d + e + f
--   pure (a % s, b % s, c % s, d % s, e % s, f % s)

-- emptyRatifyEnv :: forall era. RatifyEnv era
-- emptyRatifyEnv =
--   RatifyEnv
--     Map.empty
--     (PoolDistr Map.empty mempty)
--     Map.empty
--     Map.empty
--     (EpochNo 0)
--     (CommitteeState Map.empty)
--     Map.empty
--     Map.empty
