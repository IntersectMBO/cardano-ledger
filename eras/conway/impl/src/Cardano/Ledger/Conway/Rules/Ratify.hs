{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ratify (
  RatifyState (..),
  committeeAccepted,
  committeeAcceptedRatio,
  spoAccepted,
  spoAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  -- Testing
  prevActionAsExpected,
  validCommitteeTerm,
  withdrawalCanWithdraw,
) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  ShelleyBase,
  StrictMaybe (..),
  addEpochInterval,
 )
import Cardano.Ledger.CertState (CommitteeAuthorization (..), CommitteeState (csCommitteeCreds))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway.Core (
  Era (EraCrypto),
  EraGov,
  EraRule,
  PParams,
 )
import Cardano.Ledger.Conway.Era (ConwayENACT, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  GovAction (..),
  GovActionState (..),
  GovRelation,
  ProposalProcedure (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  Vote (..),
  ensCommitteeL,
  ensTreasuryL,
  gasAction,
  rsDelayedL,
  rsEnactStateL,
  rsEnactedL,
  rsExpiredL,
  votingCommitteeThreshold,
  votingDRepThreshold,
  votingStakePoolThreshold,
  withGovActionParent,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  ppCommitteeMaxTermLengthL,
 )
import Cardano.Ledger.Conway.Rules.Enact (EnactSignal (..), EnactState (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..), individualTotalPoolStake)
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
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Data.Word (Word64)
import Lens.Micro

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

-- Compute the ratio yes/(yes + no), where
-- yes:
--      - the number of registered, unexpired, unresigned committee members that voted yes
-- no:
--      - the number of registered, unexpired, unresigned committee members that voted no, plus
--      - the number of registered, unexpired, unresigned committee members that did not vote for this action
--
-- We iterate over the committee, and incrementally construct the numerator and denominator,
-- based on the votes and the committee state.
committeeAccepted ::
  ConwayEraPParams era =>
  RatifyEnv era ->
  RatifyState era ->
  GovActionState era ->
  Bool
committeeAccepted RatifyEnv {reCommitteeState, reCurrentEpoch} rs gas =
  case votingCommitteeThreshold reCurrentEpoch rs reCommitteeState (gasAction gas) of
    SNothing -> False -- this happens if we have no committee, or if the committee is too small,
    -- in which case the committee vote is `no`
    SJust r ->
      -- short circuit on zero threshold, in which case the committee vote is `yes`
      r == minBound || acceptedRatio >= unboundRational r
  where
    acceptedRatio =
      committeeAcceptedRatio members (gasCommitteeVotes gas) reCommitteeState reCurrentEpoch
    members = foldMap' committeeMembers (rs ^. rsEnactStateL . ensCommitteeL)

committeeAcceptedRatio ::
  forall era.
  Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo ->
  Map (Credential 'HotCommitteeRole (EraCrypto era)) Vote ->
  CommitteeState era ->
  EpochNo ->
  Rational
committeeAcceptedRatio members votes committeeState currentEpoch
  | totalExcludingAbstain == 0 = 0
  | otherwise = yesVotes % totalExcludingAbstain
  where
    accumVotes ::
      (Integer, Integer) ->
      Credential 'ColdCommitteeRole (EraCrypto era) ->
      EpochNo ->
      (Integer, Integer)
    accumVotes (!yes, !tot) member expiry
      | currentEpoch > expiry = (yes, tot) -- member is expired, vote "abstain" (don't count it)
      | otherwise =
          case Map.lookup member (csCommitteeCreds committeeState) of
            Nothing -> (yes, tot) -- member is not registered, vote "abstain"
            Just (CommitteeMemberResigned _) -> (yes, tot) -- member has resigned, vote "abstain"
            Just (CommitteeHotCredential hotKey) ->
              case Map.lookup hotKey votes of
                Nothing -> (yes, tot + 1) -- member hasn't voted, vote "no"
                Just Abstain -> (yes, tot) -- member voted "abstain"
                Just VoteNo -> (yes, tot + 1) -- member voted "no"
                Just VoteYes -> (yes + 1, tot + 1) -- member voted "yes"
    (yesVotes, totalExcludingAbstain) = Map.foldlWithKey' accumVotes (0, 0) members

spoAccepted ::
  ConwayEraPParams era => RatifyEnv era -> RatifyState era -> GovActionState era -> Bool
spoAccepted re rs gas =
  case votingStakePoolThreshold rs (gasAction gas) of
    -- Short circuit on zero threshold in order to avoid redundant computation.
    SJust r -> r == minBound || spoAcceptedRatio re gas >= unboundRational r
    SNothing -> False

-- | Final ratio for `totalAcceptedStakePoolsRatio` we want is: t = y \/ (s - a)
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
-- t = y \/ (s - a) = (y \/ s) / (1 - a \/ s)
--
-- For `HardForkInitiation` all SPOs that didn't vote are considered as
-- `No` votes. Whereas, for all other `GovAction`s, SPOs that didn't
-- vote are considered as `Abstain` votes.
spoAcceptedRatio :: forall era. RatifyEnv era -> GovActionState era -> Rational
spoAcceptedRatio
  RatifyEnv {reStakePoolDistr = PoolDistr individualPoolStake (CompactCoin totalActiveStake)}
  GovActionState
    { gasStakePoolVotes
    , gasProposalProcedure = ProposalProcedure {pProcGovAction}
    }
    | abstainVotesRatio == 1 = 0 -- guard against the degenerate case when all abstain.
    | otherwise = yesVotesRatio / (1 - abstainVotesRatio)
    where
      yesVotesRatio = toInteger yesVotes % toInteger nonZeroTotalActiveStake
      abstainVotesRatio = toInteger abstainVotes % toInteger nonZeroTotalActiveStake
      nonZeroTotalActiveStake = if 0 == totalActiveStake then 1 else totalActiveStake
      (CompactCoin yesVotes, CompactCoin abstainVotes) =
        Map.foldlWithKey' getVotesStakePerStakePoolDistr (mempty, mempty) individualPoolStake
      getVotesStakePerStakePoolDistr (!yess, !abstains) poolId distr =
        let d = individualTotalPoolStake distr
            vote = Map.lookup poolId gasStakePoolVotes
         in case vote of
              Nothing
                | HardForkInitiation {} <- pProcGovAction -> (yess, abstains)
                | otherwise -> (yess, abstains <> d)
              Just Abstain -> (yess, abstains <> d)
              Just VoteNo -> (yess, abstains)
              Just VoteYes -> (yess <> d, abstains)

dRepAccepted ::
  ConwayEraPParams era => RatifyEnv era -> RatifyState era -> GovActionState era -> Bool
dRepAccepted re rs GovActionState {gasDRepVotes, gasProposalProcedure} =
  case votingDRepThreshold rs govAction of
    SJust r ->
      -- Short circuit on zero threshold in order to avoid redundant computation.
      r == minBound
        || dRepAcceptedRatio re gasDRepVotes govAction >= unboundRational r
    SNothing -> False
  where
    govAction = pProcGovAction gasProposalProcedure

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
dRepAcceptedRatio RatifyEnv {reDRepDistr, reDRepState, reCurrentEpoch} gasDRepVotes govAction
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
                    -- drep hasn't voted for this action, so we don't count
                    -- the vote but we consider it in the denominator:
                    Nothing -> (yes, tot + stake)
                    Just VoteYes -> (yes + stake, tot + stake)
                    Just Abstain -> (yes, tot)
                    Just VoteNo -> (yes, tot + stake)
        DRepAlwaysNoConfidence ->
          case govAction of
            NoConfidence _ -> (yes + stake, tot + stake)
            _ -> (yes, tot + stake)
        DRepAlwaysAbstain -> (yes, tot)

    (yesStake, totalExcludingAbstainStake) = Map.foldlWithKey' accumStake (0, 0) reDRepDistr

delayingAction :: GovAction era -> Bool
delayingAction NoConfidence {} = True
delayingAction HardForkInitiation {} = True
delayingAction UpdateCommittee {} = True
delayingAction NewConstitution {} = True
delayingAction TreasuryWithdrawals {} = False
delayingAction ParameterChange {} = False
delayingAction InfoAction {} = False

withdrawalCanWithdraw :: GovAction era -> Coin -> Bool
withdrawalCanWithdraw (TreasuryWithdrawals m _) treasury =
  Map.foldr' (<+>) zero m <= treasury
withdrawalCanWithdraw _ _ = True

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
      , st@( RatifyState
              rsEnactState@EnactState
                { ensCurPParams
                , ensTreasury
                , ensPrevGovActionIds
                }
              _rsEnacted
              _rsExpired
              rsDelayed
            )
      , RatifySignal rsig
      ) <-
    judgmentContext
  case rsig of
    gas@GovActionState {gasId, gasExpiresAfter} SSeq.:<| sigs -> do
      let govAction = gasAction gas
      if prevActionAsExpected gas ensPrevGovActionIds
        && validCommitteeTerm govAction ensCurPParams reCurrentEpoch
        && not rsDelayed
        && withdrawalCanWithdraw govAction ensTreasury
        && committeeAccepted env st gas
        && spoAccepted env st gas
        && dRepAccepted env st gas
        then do
          newEnactState <-
            trans @(EraRule "ENACT" era) $
              TRC ((), rsEnactState, EnactSignal gasId govAction)
          let
            st' =
              st
                & rsEnactStateL .~ newEnactState
                & rsDelayedL .~ delayingAction govAction
                & rsEnactedL %~ (Seq.:|> gas)
          trans @(ConwayRATIFY era) $ TRC (env, st', RatifySignal sigs)
        else do
          -- This action hasn't been ratified yet. Process the remaining actions.
          st' <- trans @(ConwayRATIFY era) $ TRC (env, st, RatifySignal sigs)
          -- Finally, filter out actions that have expired.
          if gasExpiresAfter < reCurrentEpoch
            then pure $ st' & rsExpiredL %~ Set.insert gasId
            else pure st'
    SSeq.Empty -> pure $ st & rsEnactStateL . ensTreasuryL .~ Coin 0

-- | Check that the previous governance action id specified in the proposal
-- does match the last one of the same purpose that was enacted.
prevActionAsExpected :: GovActionState era -> GovRelation StrictMaybe era -> Bool
prevActionAsExpected gas prevGovActionIds =
  withGovActionParent gas True $ \govRelationL parent _ ->
    parent == prevGovActionIds ^. govRelationL

validCommitteeTerm ::
  ConwayEraPParams era =>
  GovAction era ->
  PParams era ->
  EpochNo ->
  Bool
validCommitteeTerm govAction pp currentEpoch =
  case govAction of
    UpdateCommittee _ _ newMembers _ -> withinMaxTermLength newMembers
    _ -> True
  where
    committeeMaxTermLength = pp ^. ppCommitteeMaxTermLengthL
    withinMaxTermLength = all (<= addEpochInterval currentEpoch committeeMaxTermLength)

instance EraGov era => Embed (ConwayENACT era) (ConwayRATIFY era) where
  wrapFailed = absurd
  wrapEvent = absurd
