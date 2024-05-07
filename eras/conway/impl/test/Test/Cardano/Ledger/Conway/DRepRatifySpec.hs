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

module Test.Cardano.Ledger.Conway.DRepRatifySpec (spec) where

import Cardano.Ledger.BaseTypes (EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifyState,
  Vote (..),
  gasAction,
  pparamsUpdateThreshold,
  votingDRepThreshold,
 )
import Cardano.Ledger.Conway.Rules (
  dRepAccepted,
  dRepAcceptedRatio,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Val ((<+>), (<->))
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Word (Word64)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Rational ((%!))

spec :: Spec
spec = do
  describe "DRep Ratification" $ do
    correctThresholdsProp @Conway
    acceptedRatioProp @Conway
    noStakeProp @Conway
    allAbstainProp @Conway
    noVotesProp @Conway
    allYesProp @Conway
    noConfidenceProp @Conway

correctThresholdsProp ::
  forall era.
  ( ConwayEraPParams era
  , Arbitrary (PParamsUpdate era)
  ) =>
  Spec
correctThresholdsProp = do
  prop "PParamsUpdateThreshold always selects a threshold" $ \thresholds ppu -> do
    let DRepVotingThresholds {..} = thresholds
        allDRepThresholds =
          Set.fromList
            [ dvtPPNetworkGroup
            , dvtPPEconomicGroup
            , dvtPPTechnicalGroup
            , dvtPPGovGroup
            ]
    when (ppu /= emptyPParamsUpdate) $
      pparamsUpdateThreshold @era thresholds ppu `shouldSatisfy` (`Set.member` allDRepThresholds)
    pparamsUpdateThreshold @era thresholds emptyPParamsUpdate `shouldBe` (0 %! 1)

acceptedRatioProp :: forall era. Era era => Spec
acceptedRatioProp = do
  prop "DRep vote count for arbitrary vote ratios" $
    forAll genRatios $ \ratios -> do
      forAll (genTestData @era ratios) $
        \(TestData {..}) -> do
          let drepState =
                -- non-expired (active) dReps
                Map.fromList
                  [(cred, DRepState (EpochNo 100) SNothing mempty) | DRepCredential cred <- Map.keys distr]
              ratifyEnv = (emptyRatifyEnv @era) {reDRepDistr = distr, reDRepState = drepState}
              actual = dRepAcceptedRatio @era ratifyEnv votes InfoAction
              -- Check the accepted min ratio is : yes/(total - abstain), or zero if everyone abstained
              expected
                | totalStake == stakeAbstain <+> stakeAlwaysAbstain = 0
                | otherwise = unCoin stakeYes % unCoin (totalStake <-> stakeAbstain <-> stakeAlwaysAbstain)
          actual `shouldBe` expected

          -- This can be also expressed as: yes/(yes + no + not voted + noconfidence)
          let expectedRephrased
                | stakeYes <+> stakeNo <+> stakeNotVoted <+> stakeNoConfidence == Coin 0 = 0
                | otherwise =
                    unCoin stakeYes % unCoin (stakeYes <+> stakeNo <+> stakeNotVoted <+> stakeNoConfidence)
          actual `shouldBe` expectedRephrased

          let actualNoConfidence = dRepAcceptedRatio @era ratifyEnv votes (NoConfidence SNothing)
              -- For NoConfidence action, we count the `NoConfidence` votes as Yes
              expectedNoConfidence
                | totalStake == stakeAbstain <+> stakeAlwaysAbstain = 0
                | otherwise =
                    unCoin (stakeYes <+> stakeNoConfidence)
                      % unCoin (totalStake <-> stakeAbstain <-> stakeAlwaysAbstain)
          actualNoConfidence `shouldBe` expectedNoConfidence

          let allExpiredDreps =
                Map.fromList
                  [(cred, DRepState (EpochNo 9) SNothing mempty) | DRepCredential cred <- Map.keys distr]
              actualAllExpired =
                dRepAcceptedRatio @era
                  ( (emptyRatifyEnv @era)
                      { reDRepDistr = distr
                      , reDRepState = allExpiredDreps
                      , reCurrentEpoch = EpochNo 10
                      }
                  )
                  votes
                  InfoAction
          actualAllExpired `shouldBe` 0

          -- Expire half of the DReps and check that the ratio is the same as if only the active DReps exist
          let (activeDreps, expiredDreps) = splitAt (length distr `div` 2) (Map.keys distr)
              activeDrepsState =
                Map.fromList
                  [(cred, DRepState (EpochNo 10) SNothing mempty) | DRepCredential cred <- activeDreps]
              expiredDrepsState =
                Map.fromList
                  [(cred, DRepState (EpochNo 3) SNothing mempty) | DRepCredential cred <- expiredDreps]
              someExpiredDrepsState = activeDrepsState `Map.union` expiredDrepsState

              actualSomeExpired =
                dRepAcceptedRatio @era
                  ( (emptyRatifyEnv @era)
                      { reDRepDistr = distr
                      , reDRepState = someExpiredDrepsState
                      , reCurrentEpoch = EpochNo 5
                      }
                  )
                  (votes `Map.union` Map.fromList [(cred, VoteYes) | DRepCredential cred <- expiredDreps])
                  InfoAction

          actualSomeExpired
            `shouldBe` dRepAcceptedRatio @era
              ( (emptyRatifyEnv @era)
                  { reDRepDistr = distr
                  , reDRepState = activeDrepsState
                  , reCurrentEpoch = EpochNo 5
                  }
              )
              votes
              InfoAction

allAbstainProp :: forall era. Era era => Spec
allAbstainProp =
  prop "If all votes are abstain, accepted ratio is zero"
    $ forAll
      ( genTestData @era
          (Ratios {yes = 0, no = 0, abstain = 50 % 100, alwaysAbstain = 50 % 100, noConfidence = 0})
      )
    $ \drepTestData ->
      activeDRepAcceptedRatio drepTestData `shouldBe` 0

noConfidenceProp :: forall era. Era era => Spec
noConfidenceProp =
  prop "If all votes are no confidence, accepted ratio is zero"
    $ forAll
      ( genTestData @era
          (Ratios {yes = 0, no = 0, abstain = 0, alwaysAbstain = 0, noConfidence = 100 % 100})
      )
    $ \drepTestData ->
      activeDRepAcceptedRatio drepTestData `shouldBe` 0

noVotesProp :: forall era. Era era => Spec
noVotesProp =
  prop "If there are no votes, accepted ratio is zero"
    $ forAll
      (genTestData @era (Ratios {yes = 0, no = 0, abstain = 0, alwaysAbstain = 0, noConfidence = 0}))
    $ \drepTestData ->
      activeDRepAcceptedRatio drepTestData `shouldBe` 0

allYesProp :: forall era. Era era => Spec
allYesProp =
  prop "If all vote yes, accepted ratio is 1 (unless there is no stake) "
    $ forAll
      ( genTestData @era
          (Ratios {yes = 100 % 100, no = 0, abstain = 0, alwaysAbstain = 0, noConfidence = 0})
      )
    $ \drepTestData ->
      if totalStake drepTestData == Coin 0
        then activeDRepAcceptedRatio drepTestData `shouldBe` 0
        else activeDRepAcceptedRatio drepTestData `shouldBe` 1

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
        dRepAccepted
          @era
          env {reDRepDistr = Map.empty}
          st
          gas
          `shouldBe` votingDRepThreshold @era st (gasAction gas)
          == SJust minBound
    )

activeDRepAcceptedRatio :: forall era. TestData era -> Rational
activeDRepAcceptedRatio (TestData {..}) =
  let activeDrepState =
        -- non-expired dReps
        Map.fromList
          [(cred, DRepState (EpochNo 100) SNothing mempty) | DRepCredential cred <- Map.keys distr]
      ratifyEnv = (emptyRatifyEnv @era) {reDRepDistr = distr, reDRepState = activeDrepState}
   in dRepAcceptedRatio @era ratifyEnv votes InfoAction

data TestData era = TestData
  { distr :: Map (DRep (EraCrypto era)) (CompactForm Coin)
  , votes :: Map (Credential 'DRepRole (EraCrypto era)) Vote
  , totalStake :: Coin
  , stakeYes :: Coin
  , stakeNo :: Coin
  , stakeAbstain :: Coin
  , stakeAlwaysAbstain :: Coin
  , stakeNoConfidence :: Coin
  , stakeNotVoted :: Coin
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
  let inDreps = listOf (DRepCredential <$> (arbitrary @(Credential 'DRepRole (EraCrypto era))))
  dreps <- inDreps

  let drepSize = length dreps
      alwaysAbstainPct :: Word64 = pct alwaysAbstain
      noConfidencePct :: Word64 = pct noConfidence
      distr =
        Map.alter
          (\case _ -> Just (CompactCoin noConfidencePct))
          DRepAlwaysNoConfidence
          . Map.alter
            (\case _ -> Just (CompactCoin alwaysAbstainPct))
            DRepAlwaysAbstain
          $ Map.fromList [(drep, CompactCoin 1) | drep <- dreps]
      (drepsYes, drepsNo, drepsAbstain, rest) = splitByPct yes no abstain dreps
      notVotedStake = length rest
      votes =
        Map.union
          (Map.fromList [(cred, VoteYes) | DRepCredential cred <- drepsYes])
          $ Map.union
            (Map.fromList [(cred, VoteNo) | DRepCredential cred <- drepsNo])
            (Map.fromList [(cred, Abstain) | DRepCredential cred <- drepsAbstain])
      pct :: Integral a => Rational -> a
      pct r = ceiling (r * fromIntegral drepSize)
  pure
    TestData
      { distr = distr
      , votes = votes
      , totalStake = fromCompact (fold distr)
      , stakeYes = Coin (fromIntegral (length drepsYes))
      , stakeNo = Coin (fromIntegral (length drepsNo))
      , stakeAbstain = Coin (fromIntegral (length drepsAbstain))
      , stakeAlwaysAbstain = Coin (fromIntegral alwaysAbstainPct)
      , stakeNoConfidence = Coin (fromIntegral noConfidencePct)
      , stakeNotVoted = Coin (fromIntegral notVotedStake)
      }
  where
    splitByPct :: Rational -> Rational -> Rational -> [a] -> ([a], [a], [a], [a])
    splitByPct x y z l =
      let
        size = fromIntegral $ length l
        (xs, rest) = splitAt (ceiling (x * size)) l
        (ys, rest') = splitAt (ceiling (y * size)) rest
        (zs, rest'') = splitAt (ceiling (z * size)) rest'
       in
        (xs, ys, zs, rest'')

genRatios :: Gen Ratios
genRatios = do
  (a, b, c, d, e, _) <- genPctsOf100
  pure $ Ratios {yes = a, no = b, abstain = c, alwaysAbstain = d, noConfidence = e}

genPctsOf100 :: Gen (Rational, Rational, Rational, Rational, Rational, Rational)
genPctsOf100 = do
  a <- choose (0, 100)
  b <- choose (0, 100)
  c <- choose (0, 100)
  d <- choose (0, 100)
  e <- choose (0, 100)
  f <- choose (0, 100)
  let s = a + b + c + d + e + f
  pure (a % s, b % s, c % s, d % s, e % s, f % s)

emptyRatifyEnv :: forall era. RatifyEnv era
emptyRatifyEnv =
  RatifyEnv
    Map.empty
    (PoolDistr Map.empty mempty)
    Map.empty
    Map.empty
    (EpochNo 0)
    (CommitteeState Map.empty)
