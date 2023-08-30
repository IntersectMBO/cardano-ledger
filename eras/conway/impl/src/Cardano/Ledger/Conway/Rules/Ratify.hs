{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ratify (
  RatifyState (..),
  RatifyEnv (..),
  RatifySignal (..),
  dRepAccepted,
  dRepAcceptedRatio,
) where

import Cardano.Ledger.BaseTypes (BoundedRational (..), ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.CertState (DRepState (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayENACT, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionState (..),
  PrevGovActionIds (..),
  RatifyState (..),
  Vote (..),
  thresholdDRep,
  thresholdSPO,
 )
import Cardano.Ledger.Conway.PParams (ConwayEraPParams)
import Cardano.Ledger.Conway.Rules.Enact (EnactSignal (..), EnactState (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..), individualPoolStake)
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.Val (Val (..), (<+>))
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Data.Word (Word64)

data RatifyEnv era = RatifyEnv
  { reStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , reStakePoolDistr :: !(PoolDistr (EraCrypto era))
  , reDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
  , reDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
  , reCurrentEpoch :: !EpochNo
  }

deriving instance Show (RatifyEnv era)

newtype RatifySignal era = RatifySignal (StrictSeq (GovActionState era))

instance
  ( ConwayEraPParams era
  , Embed (EraRule "ENACT" era) (ConwayRATIFY era)
  , State (EraRule "ENACT" era) ~ EnactState era
  , Environment (EraRule "ENACT" era) ~ ()
  , Signal (EraRule "ENACT" era) ~ EnactSignal era
  ) =>
  STS (ConwayRATIFY era)
  where
  type Environment (ConwayRATIFY era) = RatifyEnv era
  type PredicateFailure (ConwayRATIFY era) = Void
  type Signal (ConwayRATIFY era) = RatifySignal era
  type State (ConwayRATIFY era) = RatifyState era
  type BaseM (ConwayRATIFY era) = ShelleyBase

  initialRules = []
  transitionRules = [ratifyTransition]

--- Constants

-- ccThreshold :: Int
-- ccThreshold = 0

spoAccepted ::
  ConwayEraPParams era =>
  RatifyState era ->
  RatifyEnv era ->
  GovActionState era ->
  Bool
spoAccepted rs RatifyEnv {reStakePoolDistr = PoolDistr poolDistr} gas =
  case thresholdSPO rs gasAction of
    -- Short circuit on zero threshold in order to avoid redundant computation.
    SJust r -> r == minBound || totalAcceptedStakePoolsRatio >= unboundRational r
    SNothing -> False
  where
    GovActionState {gasStakePoolVotes, gasAction} = gas
    -- Final ratio for `totalAcceptedStakePoolsRatio` we want is: t = y / (s - a)
    -- Where:
    --  * `y` - total delegated stake that voted Yes
    --  * `a` - total delegated stake that voted Abstain
    --  * `s` - total delegated stake
    --
    -- However, computing the total stake again would be wasteful, since we already have
    -- distributions per pool computed. So, values that we have available are not exactly
    -- what we need, because we have:
    --  * `y/s` - stake that votes yes over the total stake
    --  * `a/s` - stake that voted abstain over the total stake
    --
    -- We divide both numerator and denominator by `s` and we'll get a formula that we can
    -- use:
    --
    -- t = y / (s - a) = (y / s) / (1 - a / s)
    totalAcceptedStakePoolsRatio
      | abstainVotesRatio == 1 = 0 -- guard against the degenerate case when all abstain.
      | otherwise = yesVotesRatio / (1 - abstainVotesRatio)
      where
        (Sum yesVotesRatio, Sum abstainVotesRatio) =
          Map.foldMapWithKey lookupStakePoolDistrForVotes gasStakePoolVotes

        lookupStakePoolDistrForVotes poolId vote = fromMaybe (mempty, mempty) $ do
          distr <- Sum . individualPoolStake <$> Map.lookup poolId poolDistr
          case vote of
            VoteNo -> Nothing
            VoteYes -> Just (distr, mempty)
            Abstain -> Just (mempty, distr)

dRepAccepted :: forall era. ConwayEraPParams era => RatifyEnv era -> RatifyState era -> GovActionState era -> Bool
dRepAccepted re rs GovActionState {gasDRepVotes, gasAction} =
  case thresholdDRep rs gasAction of
    SJust r ->
      -- Short circuit on zero threshold in order to avoid redundant computation.
      r == minBound
        || dRepAcceptedRatio re gasDRepVotes gasAction >= unboundRational r
    SNothing -> False

-- Compute the dRep ratio yes/(yes + no), where
-- yes: is the total stake of
--    - registered dReps that voted 'yes', plus
--    - the AlwaysNoConfidence dRep, in case the action is NoConfidence
-- no: is the total stake of
--    - registered dReps that voted 'no', plus
--    - registered dReps that did not vote for this action, plus
--    - the AlwaysNoConfidence dRep
-- In other words, the denominator `yes + no` is the total stake of all registered dReps, minus the abstain votes stake
-- (both credential DReps and AlwaysAbstain)
--
-- We iterate over the dRep distribution, and incrementally construct the numerator and denominator.
dRepAcceptedRatio ::
  forall era.
  RatifyEnv era ->
  Map (Credential 'DRepRole (EraCrypto era)) Vote ->
  GovAction era ->
  Rational
dRepAcceptedRatio RatifyEnv {reDRepDistr, reDRepState, reCurrentEpoch} gasDRepVotes gasAction
  | totalExcludingAbstainStake == 0 = 0
  | otherwise = toInteger yesStake % toInteger totalExcludingAbstainStake
  where
    accumStake :: (Word64, Word64) -> DRep (EraCrypto era) -> CompactForm Coin -> (Word64, Word64)
    accumStake (!yes, !tot) drep (CompactCoin stake) =
      case drep of
        DRepCredential cred ->
          case Map.lookup cred reDRepState of
            Nothing -> (yes, tot) -- drep is not registered, so we don't consider it
            Just (DRepState expiry _ _)
              | reCurrentEpoch > expiry -> (yes, tot) -- drep is expired, so we don't consider it
              | otherwise ->
                  case Map.lookup cred gasDRepVotes of
                    Nothing -> (yes, tot + stake) -- drep hasn't voted for this action, so we don't count
                    -- the vote but we consider it in the denominator
                    Just VoteYes -> (yes + stake, tot + stake)
                    Just Abstain -> (yes, tot)
                    Just VoteNo -> (yes, tot + stake)
        DRepAlwaysNoConfidence ->
          case gasAction of
            NoConfidence _ -> (yes + stake, tot + stake)
            _ -> (yes, tot + stake)
        DRepAlwaysAbstain -> (yes, tot)

    (yesStake, totalExcludingAbstainStake) = Map.foldlWithKey' accumStake (0, 0) reDRepDistr

delayingAction :: GovAction era -> Bool
delayingAction NoConfidence {} = True
delayingAction HardForkInitiation {} = True
delayingAction NewCommittee {} = True
delayingAction NewConstitution {} = True
delayingAction TreasuryWithdrawals {} = False
delayingAction ParameterChange {} = False
delayingAction InfoAction {} = False

ratifyTransition ::
  forall era.
  ( Embed (EraRule "ENACT" era) (ConwayRATIFY era)
  , State (EraRule "ENACT" era) ~ EnactState era
  , Environment (EraRule "ENACT" era) ~ ()
  , Signal (EraRule "ENACT" era) ~ EnactSignal era
  , ConwayEraPParams era
  ) =>
  TransitionRule (ConwayRATIFY era)
ratifyTransition = do
  TRC
    ( env@RatifyEnv {reCurrentEpoch}
      , st@RatifyState {..}
      , RatifySignal rsig
      ) <-
    judgmentContext
  case rsig of
    ast :<| sigs -> do
      let gas@GovActionState {gasId, gasAction, gasExpiresAfter} = ast
          withdrawalCanWithdraw (TreasuryWithdrawals m) =
            Map.foldr' (<+>) zero m <= ensTreasury rsEnactState
          withdrawalCanWithdraw _ = True
          notDelayed = not rsDelayed
      if prevActionAsExpected
        gasAction
        (ensPrevGovActionIds rsEnactState)
        && notDelayed
        && withdrawalCanWithdraw gasAction
        && spoAccepted st env ast
        && dRepAccepted env st gas
        then do
          -- Update ENACT state with the governance action that was ratified
          es <-
            trans @(EraRule "ENACT" era) $
              TRC ((), rsEnactState, EnactSignal gasId gasAction)
          let st' =
                st
                  { rsEnactState = es
                  , rsRemoved = Set.insert gasId rsRemoved
                  , rsDelayed = delayingAction gasAction
                  }
          trans @(ConwayRATIFY era) $ TRC (env, st', RatifySignal sigs)
        else do
          -- This action hasn't been ratified yet. Process the remaining actions.
          st' <- trans @(ConwayRATIFY era) $ TRC (env, st, RatifySignal sigs)
          -- Finally, filter out actions that are not processed.
          if gasExpiresAfter < reCurrentEpoch
            then pure st' {rsRemoved = Set.insert gasId rsRemoved} -- Action expires after current Epoch. Remove it.
            else pure st'
    Empty -> pure st

-- | Check that the previous governance action id specified in the proposal
--   does match the last one of the same purpose that was enacted.
prevActionAsExpected :: forall era. GovAction era -> PrevGovActionIds era -> Bool
prevActionAsExpected (ParameterChange prev _) (PrevGovActionIds {pgaPParamUpdate}) =
  prev == pgaPParamUpdate
prevActionAsExpected (HardForkInitiation prev _) (PrevGovActionIds {pgaHardFork}) =
  prev == pgaHardFork
prevActionAsExpected (NoConfidence prev) (PrevGovActionIds {pgaCommittee}) =
  prev == pgaCommittee
prevActionAsExpected (NewCommittee prev _ _) (PrevGovActionIds {pgaCommittee}) =
  prev == pgaCommittee
prevActionAsExpected (NewConstitution prev _) (PrevGovActionIds {pgaConstitution}) =
  prev == pgaConstitution
prevActionAsExpected _ _ = True -- for the other actions, the last action is not relevant

instance EraGov era => Embed (ConwayENACT era) (ConwayRATIFY era) where
  wrapFailed = absurd
  wrapEvent = absurd
