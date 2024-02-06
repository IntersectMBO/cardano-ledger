{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.CommitteeRatifySpec (spec) where

import Cardano.Ledger.BaseTypes (EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionState (..),
  ProposalProcedure (..),
  RatifyEnv (..),
  RatifyState,
  Vote (..),
  ensCommitteeL,
  rsEnactStateL,
 )
import Cardano.Ledger.Conway.Rules (
  committeeAccepted,
  committeeAcceptedRatio,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Control.Monad (guard, join)
import Data.Functor.Identity (Identity)
import Data.List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  describe "Committee Ratification" $ do
    acceptedRatioProp @Conway
    acceptedProp @Conway
    allYesProp @Conway
    allNoProp @Conway
    allAbstainProp @Conway
    expiredAndResignedMembersProp @Conway

acceptedRatioProp :: forall era. Era era => Spec
acceptedRatioProp =
  prop "Committee vote count for arbitrary vote ratios" $
    forAll genRatios $ \ratios -> do
      forAll (genTestData ratios) $
        \TestData {members, votes, committeeState} -> do
          let acceptedRatio =
                committeeAcceptedRatio @era members (totalVotes votes) committeeState (EpochNo 0)
              Votes {..} = votes
              -- everyone is registered and noone is resigned,
              -- so we expect the accepted ratio to be yes / (yes + no + notVoted)
              expectedRatio =
                ratioOrZero
                  (length votedYes)
                  (length votedYes + length votedNo + length notVoted)

          acceptedRatio `shouldBe` expectedRatio

          -- we can also express this as : yes / (total - abstain)
          let expectedRatioAlt =
                ratioOrZero
                  (length votedYes)
                  (length members - length votedAbstain)

          acceptedRatio `shouldBe` expectedRatioAlt

acceptedProp ::
  forall era.
  ( ConwayEraPParams era
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Spec
acceptedProp =
  prop "Only NoConfidence or UpdateCommittee should pass without a committee" $
    forAll (arbitrary @(RatifyState era, RatifyEnv era, GovActionState era)) $ do
      \(rs, rEnv, gas) -> do
        committeeAccepted rEnv (rs & rsEnactStateL . ensCommitteeL .~ SNothing) gas
          `shouldBe` isNoConfidenceOrUpdateCommittee gas
  where
    isNoConfidenceOrUpdateCommittee GovActionState {gasProposalProcedure} =
      case pProcGovAction gasProposalProcedure of
        NoConfidence {} -> True
        UpdateCommittee {} -> True
        _ -> False

allYesProp :: forall era. Era era => Spec
allYesProp =
  prop "If all vote yes, ratio is 1" $
    forAll (genTestData (Ratios {yes = 1, no = 0, abstain = 0})) $
      \TestData {members, votes, committeeState} -> do
        let acceptedRatio =
              committeeAcceptedRatio @era members (totalVotes votes) committeeState (EpochNo 0)
        acceptedRatio `shouldBe` 1

allNoProp :: forall era. Era era => Spec
allNoProp =
  prop "If all vote no, ratio is 0" $
    forAll (genTestData (Ratios {yes = 0, no = 1, abstain = 0})) $
      \TestData {members, votes, committeeState} -> do
        let acceptedRatio =
              committeeAcceptedRatio @era members (totalVotes votes) committeeState (EpochNo 0)
        acceptedRatio `shouldBe` 0

allAbstainProp :: forall era. Era era => Spec
allAbstainProp =
  prop "If all abstain, ratio is 0" $
    forAll (genTestData (Ratios {yes = 0, no = 0, abstain = 1})) $
      \TestData {members, votes, committeeState} -> do
        let acceptedRatio =
              committeeAcceptedRatio @era members (totalVotes votes) committeeState (EpochNo 0)
        acceptedRatio `shouldBe` 0

expiredAndResignedMembersProp :: forall era. Era era => Spec
expiredAndResignedMembersProp =
  prop "Expired or resigned members are not counted" $
    forAll genRatios $ \ratios -> do
      forAll (genTestData @era ratios) $ \testData -> do
        forAll ((,) <$> genEpoch <*> genExpiredEpoch) $ \(epochNo, expiredEpochNo) -> do
          -- generate test data with some expired and/or resigned credentials corresponding
          -- to each category of votes
          forAll (genExpiredOrResignedForEachVoteType testData expiredEpochNo) $ do
            \(testData', remainingYes, remainingNo, remainingNotVoted) -> do
              let TestData {members, votes, committeeState} = testData'
                  acceptedRatio =
                    committeeAcceptedRatio @era members (totalVotes votes) committeeState epochNo
                  expectedRatio =
                    ratioOrZero
                      remainingYes
                      (remainingYes + remainingNo + remainingNotVoted)
              acceptedRatio `shouldBe` expectedRatio
  where
    genExpiredOrResignedForEachVoteType ::
      TestData era ->
      EpochNo ->
      Gen (TestData era, Int, Int, Int)
    genExpiredOrResignedForEachVoteType td epochNo = do
      let Votes {votedYes, votedNo, votedAbstain, notVoted} = votes td
      (td', remYes) <- genExpiredOrResigned td votedYes epochNo
      (td'', remNo) <- genExpiredOrResigned td' votedNo epochNo
      (td''', _) <- genExpiredOrResigned td'' votedAbstain epochNo
      (res, remNotVoted) <- genExpiredOrResigned td''' notVoted epochNo
      pure (res, remYes, remNo, remNotVoted)

    genExpiredOrResigned ::
      TestData era ->
      [Credential 'HotCommitteeRole (EraCrypto era)] ->
      EpochNo ->
      Gen (TestData era, Int)
    genExpiredOrResigned td votes epochNo = do
      pct <- arbitrary @Rational
      frequency
        [ (4, pure $ updatePctOfCommittee @era td pct votes (expireMembers epochNo))
        , (4, pure $ updatePctOfCommittee @era td pct votes resignMembers)
        , (2, pure $ updatePctOfCommittee @era td pct votes (expireAndResign epochNo))
        ]
    expireAndResign ::
      EpochNo ->
      Set.Set (Credential 'HotCommitteeRole (EraCrypto era)) ->
      TestData era ->
      TestData era
    expireAndResign epochNo hotCreds td =
      let td' = expireMembers epochNo hotCreds td
          td'' = resignMembers hotCreds td'
       in td''

-- Updates a percentage of the committee of the given test data.
-- The update is based on a function that given a set of hot credentials,
-- updates test data based on these.
-- We pass to this update function a percentage of the given list of credentials.
-- We also calculate and return the number of credentials that haven't been affected by the update.
-- The initial list contains duplicates (these are corresponding to votes).
-- We are passing a percentage of distinct credentials to the update functions,
-- but we want to calculate correctly the number of credentials that haven't been affected by the update
-- (including duplicates, excluding all the ones that are being updated).
updatePctOfCommittee ::
  TestData era ->
  Rational ->
  [Credential 'HotCommitteeRole (EraCrypto era)] ->
  -- | The update function, which updates test data based on a set of credentials.
  (Set.Set (Credential 'HotCommitteeRole (EraCrypto era)) -> TestData era -> TestData era) ->
  (TestData era, Int)
updatePctOfCommittee td pct hotCreds action =
  let
    hotCredsSet = Set.fromList hotCreds
    affectedSize = pctOfN pct (length hotCreds)
    affectedCreds = Set.take affectedSize hotCredsSet
    -- we want to count all the remaining credentials, including duplicates
    remaining = length $ filter (`Set.notMember` affectedCreds) hotCreds
    res = action affectedCreds td
   in
    (res, remaining)
  where
    pctOfN :: Rational -> Int -> Int
    pctOfN p n = floor (p * fromIntegral n)

data Ratios = Ratios
  { yes :: Rational
  , no :: Rational
  , abstain :: Rational
  }
  deriving (Show)

data TestData era = TestData
  { members :: Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo
  , votes :: Votes era
  , committeeState :: CommitteeState era
  }
  deriving (Show)

data Votes era = Votes
  { votedYes :: [Credential 'HotCommitteeRole (EraCrypto era)]
  , votedNo :: [Credential 'HotCommitteeRole (EraCrypto era)]
  , votedAbstain :: [Credential 'HotCommitteeRole (EraCrypto era)]
  , notVoted :: [Credential 'HotCommitteeRole (EraCrypto era)]
  }
  deriving (Show)

genTestData ::
  forall era.
  Era era =>
  Ratios ->
  Gen (TestData era)
genTestData ratios = do
  coldCreds <- genNonEmptyColdCreds @era
  committeeState@(CommitteeState {csCommitteeCreds}) <- genNonResignedCommitteeState @era coldCreds
  members <- genMembers @era coldCreds
  let hotCreds = catMaybes $ Map.elems csCommitteeCreds
      votes = distributeVotes @era ratios hotCreds
  pure $ TestData members votes committeeState

-- Updates the given test data by resigning the given hot credentials.
resignMembers ::
  Set.Set (Credential 'HotCommitteeRole (EraCrypto era)) ->
  TestData era ->
  TestData era
resignMembers hotCreds td@TestData {committeeState} =
  td
    { committeeState =
        CommitteeState
          ( Map.map
              (\mhk -> mhk >>= \hk -> hk <$ guard (hk `Set.notMember` hotCreds))
              (csCommitteeCreds committeeState)
          )
    }

expireMembers ::
  EpochNo ->
  Set.Set (Credential 'HotCommitteeRole (EraCrypto era)) ->
  TestData era ->
  TestData era
expireMembers newEpochNo hotCreds td@TestData {members, committeeState} =
  td
    { members =
        Map.mapWithKey (\ck epochNo -> if expire ck then newEpochNo else epochNo) members
    }
  where
    expire ck = hk ck `Set.isSubsetOf` hotCreds
    hk ck =
      maybe Set.empty Set.singleton $
        join $
          Map.lookup ck (csCommitteeCreds committeeState)

totalVotes :: Votes era -> Map (Credential 'HotCommitteeRole (EraCrypto era)) Vote
totalVotes Votes {votedYes, votedNo, votedAbstain} =
  Map.unions
    [ Map.fromSet (const VoteYes) (Set.fromList votedYes)
    , Map.fromSet (const VoteNo) (Set.fromList votedNo)
    , Map.fromSet (const Abstain) (Set.fromList votedAbstain)
    ]

genNonEmptyColdCreds :: Era era => Gen (Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)))
genNonEmptyColdCreds =
  Set.fromList <$> listOf1 arbitrary

genMembers ::
  Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  Gen (Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
genMembers coldCreds =
  Map.fromList . zip (Set.toList coldCreds)
    <$> vectorOf (length coldCreds) genNonExpiredEpoch

genEpoch :: Gen EpochNo
genEpoch = EpochNo <$> choose (100, 1000)

genNonExpiredEpoch :: Gen EpochNo
genNonExpiredEpoch = EpochNo <$> choose (1000, maxBound)

genExpiredEpoch :: Gen EpochNo
genExpiredEpoch = EpochNo <$> choose (0, 100)

genNonResignedCommitteeState ::
  forall era.
  Era era =>
  Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  Gen (CommitteeState era)
genNonResignedCommitteeState coldCreds = do
  hotCredsMap <- sequence $ Map.fromSet (\_ -> Just <$> arbitrary) coldCreds
  frequency
    [ (9, pure $ CommitteeState hotCredsMap)
    , (1, CommitteeState <$> overwriteWithDuplicate hotCredsMap)
    ]
  where
    overwriteWithDuplicate m
      | Map.size m < 2 = pure m
      | otherwise = do
          fromIx <- choose (0, Map.size m - 1)
          toIx <- choose (0, Map.size m - 1)
          let valueToDuplicate = snd $ Map.elemAt fromIx m
          pure $ Map.updateAt (\_ _ -> Just valueToDuplicate) toIx m

distributeVotes ::
  Ratios ->
  [Credential 'HotCommitteeRole (EraCrypto era)] ->
  Votes era
distributeVotes Ratios {yes, no, abstain} hotCreds = do
  let
    -- The list of hot credentials, which we split into the 4 voting categories, may contain duplicates.
    -- We want the duplicates to be in the same category (since this is what will happen in practice,
    -- where the votes is a Map from hot credential to vote).
    -- So we first remove the duplicates, then split the list into the 4 categories,
    -- and then add the duplicates back.
    hotCredsSet = Set.fromList hotCreds
    duplicates = Set.fromList $ hotCreds \\ Set.toList hotCredsSet
    (yesCreds, noCreds, abstainCreds, notVotedCreds) = splitByPct yes no abstain hotCredsSet
   in
    Votes
      { votedYes = addDuplicates yesCreds duplicates
      , votedNo = addDuplicates noCreds duplicates
      , votedAbstain = addDuplicates abstainCreds duplicates
      , notVoted = addDuplicates notVotedCreds duplicates
      }
  where
    splitByPct ::
      Rational ->
      Rational ->
      Rational ->
      Set.Set a ->
      (Set.Set a, Set.Set a, Set.Set a, Set.Set a)
    splitByPct x y z l =
      let
        size = fromIntegral $ length l
        (xs, rest) = Set.splitAt (round (x * size)) l
        (ys, rest') = Set.splitAt (round (y * size)) rest
        (zs, rest'') = Set.splitAt (round (z * size)) rest'
       in
        (xs, ys, zs, rest'')
    addDuplicates :: Ord a => Set.Set a -> Set.Set a -> [a]
    addDuplicates s dups =
      if dups `Set.isSubsetOf` s
        then Set.toList s ++ Set.toList dups
        else Set.toList s

genRatios :: Gen Ratios
genRatios = do
  (a, b, c, _) <- genPctsOf100
  pure $ Ratios {yes = a, no = b, abstain = c}

genPctsOf100 :: Gen (Rational, Rational, Rational, Rational)
genPctsOf100 = do
  a <- choose (0, 100)
  b <- choose (0, 100)
  c <- choose (0, 100)
  d <- choose (0, 100)
  let s = a + b + c + d
  pure (a % s, b % s, c % s, d % s)

ratioOrZero :: Integral a => a -> a -> Rational
ratioOrZero a b =
  if b == 0
    then 0
    else fromIntegral a % fromIntegral b
