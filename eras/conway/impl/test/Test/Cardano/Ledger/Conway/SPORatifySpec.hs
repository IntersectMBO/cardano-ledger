{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.SPORatifySpec (spec) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
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
  ensProtVerL,
  gasAction,
  gasActionL,
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
import Cardano.Ledger.Shelley.HardForks (bootstrapPhase)
import Cardano.Ledger.Val ((<+>), (<->))
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  describe "SPO Ratification" $ do
    acceptedRatioProp @Conway
    noStakeProp @Conway
    allAbstainProp @Conway
    noVotesProp @Conway
    allYesProp @Conway
    noConfidenceProp @Conway

acceptedRatioProp ::
  forall era.
  ( Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (PParamsHKD Identity era)
  , ConwayEraPParams era
  ) =>
  Spec
acceptedRatioProp = do
  prop @((RatifyEnv era, RatifyState era, GovActionState era) -> Property)
    "SPO vote count for arbitrary vote ratios"
    $ \(re, rs, gas) -> forAll genRatios $ \ratios ->
      forAll
        (genTestData @era ratios)
        ( \TestData {..} -> do
            let
              protVer = rs ^. rsEnactStateL . ensProtVerL
              actual =
                spoAcceptedRatio @era
                  re {reStakePoolDistr = distr, reDelegatees = delegatees, rePoolParams = poolParams}
                  gas {gasStakePoolVotes = votes}
                  protVer
              expected =
                if fromCompact totalStake == stakeAbstain <+> stakeAlwaysAbstain
                  then 0
                  else case gas ^. gasActionL of
                    HardForkInitiation _ _ -> unCoin stakeYes % unCoin (fromCompact totalStake <-> stakeAbstain)
                    action
                      | bootstrapPhase protVer ->
                          unCoin stakeYes
                            % unCoin (fromCompact totalStake <-> stakeAbstain <-> stakeAlwaysAbstain <-> stakeNoConfidence)
                      | NoConfidence {} <- action ->
                          unCoin (stakeYes <+> stakeNoConfidence)
                            % unCoin (fromCompact totalStake <-> stakeAbstain <-> stakeAlwaysAbstain)
                      | otherwise ->
                          unCoin stakeYes % unCoin (fromCompact totalStake <-> stakeAbstain <-> stakeAlwaysAbstain)
            actual `shouldBe` expected
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
    ( \(re, rs, gas) ->
        spoAccepted
          @era
          re {reStakePoolDistr = PoolDistr Map.empty (fromJust . toCompact $ Coin 100)}
          rs
          gas
          `shouldBe` votingStakePoolThreshold @era rs (gasAction gas)
          == SJust minBound
    )

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

data TestData era = TestData
  { distr :: PoolDistr (EraCrypto era)
  , votes :: Map (KeyHash 'StakePool (EraCrypto era)) Vote
  , totalStake :: CompactForm Coin
  , stakeYes :: Coin
  , stakeNo :: Coin
  , stakeAbstain :: Coin
  , stakeAlwaysAbstain :: Coin
  , stakeNoConfidence :: Coin
  , stakeNotVoted :: Coin
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

-- Prepare the pool distribution, votes, map of pool parameters and map of reward account delegatees
-- according to the given ratios.
genTestData ::
  forall era.
  Era era =>
  Ratios ->
  Gen (TestData era)
genTestData Ratios {yes, no, abstain, alwaysAbstain, noConfidence} = do
  let inPools = listOf (arbitrary @(KeyHash 'StakePool (EraCrypto era)))
  pools <- inPools
  let (poolsYes, poolsNo, poolsAbstain, poolsAlwaysAbstain, poolsNoConfidence, rest) =
        splitByPct yes no abstain alwaysAbstain noConfidence pools
      totalStake = length pools
  distr <- do
    vrf <- arbitrary
    let
      indivStake = IndividualPoolStake (1 / toRational totalStake) (CompactCoin 1) vrf
    pure $
      PoolDistr
        ( unionAllFromLists
            [ (poolsYes, indivStake)
            , (poolsNo, indivStake)
            , (poolsAbstain, indivStake)
            , (poolsAlwaysAbstain, indivStake)
            , (poolsNoConfidence, indivStake)
            ]
        )
        (CompactCoin $ fromIntegral totalStake)

  poolParamsAA <- genPoolParams poolsAlwaysAbstain
  poolParamsNC <- genPoolParams poolsNoConfidence
  poolParamsRest <- genPoolParams $ poolsYes <> poolsNo <> poolsAbstain
  let delegateesAA = mkDelegatees DRepAlwaysAbstain poolParamsAA
      delegateesNC = mkDelegatees DRepAlwaysNoConfidence poolParamsNC
      votes = unionAllFromLists [(poolsYes, VoteYes), (poolsNo, VoteNo), (poolsAbstain, Abstain)]

  pure
    TestData
      { distr
      , votes
      , totalStake = pdTotalActiveStake distr
      , stakeYes = Coin . fromIntegral $ length poolsYes
      , stakeNo = Coin . fromIntegral $ length poolsNo
      , stakeAbstain = Coin . fromIntegral $ length poolsAbstain
      , stakeAlwaysAbstain = Coin . fromIntegral $ length poolsAlwaysAbstain
      , stakeNoConfidence = Coin . fromIntegral $ length poolsNoConfidence
      , stakeNotVoted = Coin . fromIntegral $ length rest
      , delegatees = Map.union delegateesAA delegateesNC
      , poolParams = Map.union poolParamsRest $ Map.union poolParamsAA poolParamsNC
      }
  where
    splitByPct ::
      Rational ->
      Rational ->
      Rational ->
      Rational ->
      Rational ->
      [a] ->
      ([a], [a], [a], [a], [a], [a])
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

    genPoolParams p = do
      params <- arbitrary
      pure $ Map.fromList [(cred, params) | cred <- p]

    -- Given a delegatee and a map of stake pool params,
    -- create a map of reward account delegatees.
    mkDelegatees ::
      DRep (EraCrypto era) ->
      Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)) ->
      Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era))
    mkDelegatees drep =
      Map.fromList
        . map (\(_, params) -> (raCredential $ ppRewardAccount params, drep))
        . Map.toList

    -- Create a map from each pool with the given value, where the key is the pool credential
    -- and take the union of all these maps.
    unionAllFromLists ::
      [([KeyHash 'StakePool (EraCrypto era)], a)] ->
      Map (KeyHash 'StakePool (EraCrypto era)) a
    unionAllFromLists =
      foldr (Map.union . (\(l, v) -> Map.fromList [(cred, v) | cred <- l])) Map.empty

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
