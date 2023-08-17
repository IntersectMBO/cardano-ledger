{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.RatifySpec (spec) where

import Cardano.Ledger.BaseTypes (EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionState (..),
  Vote (..),
 )
import Cardano.Ledger.Conway.Rules (RatifyEnv (..), dRepAccepted, dRepAcceptedRatio)
import Cardano.Ledger.Core
import Cardano.Ledger.DRepDistr (DRepState (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  describe "Ratification" $ do
    drepsProp @Conway
    drepsPropNoStake @Conway

drepsProp :: forall era. Era era => Spec
drepsProp =
  prop "DRep vote counts" $
    forAll (arbitrary @(Map (DRep (EraCrypto era)) (CompactForm Coin))) $
      \dRepDistr -> do
        forAll (shuffle (Map.keys dRepDistr)) $ \dreps -> do
          let size = fromIntegral $ length dreps
              yes = ratio (30 :: Integer) size
              drepsYes = onlyDrepsCred $ take yes dreps
              votesYes = Map.fromList $ [(cred, VoteYes) | DRepCredential cred <- drepsYes]
              CompactCoin stakeYes = fold $ Map.restrictKeys dRepDistr (Set.fromList drepsYes)

              no = ratio 40 size
              drepsNo = onlyDrepsCred $ take no . drop yes $ dreps
              votesNo = Map.fromList $ [(cred, VoteNo) | DRepCredential cred <- drepsNo]
              CompactCoin stakeNo = fold $ Map.restrictKeys dRepDistr (Set.fromList drepsNo)

              abstain = ratio 10 size
              drepsAbstain = onlyDrepsCred $ take abstain . drop (yes + no) $ dreps
              votesAbstain = Map.fromList [(cred, Abstain) | DRepCredential cred <- drepsAbstain]
              CompactCoin stakeAbstain = fold $ Map.restrictKeys dRepDistr (Set.fromList drepsAbstain)

              CompactCoin stakeAlwaysAbstain = fromMaybe (CompactCoin 0) $ Map.lookup DRepAlwaysAbstain dRepDistr
              CompactCoin stakeAlwaysNoConfidence = fromMaybe (CompactCoin 0) $ Map.lookup DRepAlwaysNoConfidence dRepDistr
              CompactCoin notVotedStake =
                fold $
                  Map.withoutKeys
                    dRepDistr
                    (Set.fromList (drepsYes ++ drepsNo ++ drepsAbstain ++ [DRepAlwaysAbstain, DRepAlwaysNoConfidence]))

              votes = Map.union votesYes $ Map.union votesNo votesAbstain

              CompactCoin totalStake = fold dRepDistr
              ratifyEnv =
                RatifyEnv
                  { reStakeDistr = Map.empty
                  , reStakePoolDistr = PoolDistr Map.empty
                  , reDRepDistr = dRepDistr
                  , reDRepState =
                      Map.fromList
                        [(cred, DRepState (EpochNo 100) SNothing mempty) | DRepCredential cred <- Map.keys dRepDistr]
                  , reCurrentEpoch = EpochNo 0
                  }

              actual = dRepAcceptedRatio @era ratifyEnv votes InfoAction
              expected
                | totalStake == stakeAbstain + stakeAlwaysAbstain = 0
                | otherwise = toInteger stakeYes % toInteger (totalStake - stakeAbstain - stakeAlwaysAbstain)

          actual `shouldBe` expected

          let expectedRephrased
                | stakeYes + stakeNo + notVotedStake + stakeAlwaysNoConfidence == 0 = 0
                | otherwise = toInteger stakeYes % toInteger (stakeYes + stakeNo + notVotedStake + stakeAlwaysNoConfidence)
          actual `shouldBe` expectedRephrased

          let actualNoConfidence = dRepAcceptedRatio @era ratifyEnv votes (NoConfidence SNothing)
              expectedNoConfidence
                | totalStake == stakeAbstain + stakeAlwaysAbstain = 0
                | otherwise = toInteger (stakeYes + stakeAlwaysNoConfidence) % toInteger (totalStake - stakeAbstain - stakeAlwaysAbstain)
          actualNoConfidence `shouldBe` expectedNoConfidence
  where
    ratio pct tot = ceiling $ (pct * tot) % 100
    onlyDrepsCred l =
      filter
        ( \case
            DRepCredential _ -> True
            _ -> False
        )
        l

drepsPropNoStake ::
  forall era.
  ( EraPParams era
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (PParamsHKD Identity era)
  ) =>
  Spec
drepsPropNoStake =
  prop "If there is no stake, accept only if the threshold is zero" $
    forAll
      ((,) <$> arbitrary @(RatifyEnv era) <*> arbitrary @(GovActionState era))
      ( \(env, gas) -> do
          dRepAccepted @era env {reDRepDistr = Map.empty} gas 0
            `shouldBe` True
          dRepAccepted @era env {reDRepDistr = Map.empty} gas (1 % 2)
            `shouldBe` False
      )
