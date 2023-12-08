{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Gov (
  ConwayGOV,
  GovEnv (..),
  ConwayGovEvent (..),
  GovRuleState (..),
  ConwayGovPredFailure (..),
) where

import Cardano.Ledger.Address (RewardAcnt, getRwdNetwork)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Network,
  ProtVer,
  ShelleyBase,
  StrictMaybe (SJust),
  addEpochInterval,
  networkId,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayGOV)
import Cardano.Ledger.Conway.Governance (
  GovActionId (..),
  GovActionState (..),
  GovProcedures (..),
  PrevGovActionId (..),
  PrevGovActionIds (..),
  PrevGovActionIdsChildren,
  ProposalProcedure (..),
  Proposals,
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  gasExpiresAfterL,
  indexedGovProps,
  isCommitteeVotingAllowed,
  isDRepVotingAllowed,
  isStakePoolVotingAllowed,
  proposalsActions,
  proposalsAddProposal,
  proposalsAddVote,
  proposalsGovActionStates,
  proposalsLookupId,
 )
import Cardano.Ledger.Conway.Governance.Procedures (GovAction (..))
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  ppGovActionDepositL,
  ppGovActionLifetimeL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import Cardano.Ledger.TxIn (TxId (..))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  judgmentContext,
  liftSTS,
  tellEvent,
  (?!),
 )
import qualified Data.Foldable as Fold (toList)
import qualified Data.Map.Merge.Strict as Map (dropMissing, merge, zipWithMaybeMatched)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.OSet.Strict as OSet
import Data.Pulse (foldlM')
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

-- ===========================================

data GovEnv era = GovEnv
  { geTxId :: !(TxId (EraCrypto era))
  , geEpoch :: !EpochNo
  , gePParams :: !(PParams era)
  , gePrevGovActionIds :: !(PrevGovActionIds era)
  }

data GovRuleState era = GovRuleState
  { grsProposals :: !(Proposals era)
  , grsPrevGovActionIdsChildren :: !(PrevGovActionIdsChildren era)
  }

data ConwayGovPredFailure era
  = GovActionsDoNotExist (Set.Set (GovActionId (EraCrypto era)))
  | MalformedProposal (GovAction era)
  | ProposalProcedureNetworkIdMismatch (RewardAcnt (EraCrypto era)) Network
  | TreasuryWithdrawalsNetworkIdMismatch (Set.Set (RewardAcnt (EraCrypto era))) Network
  | ProposalDepositIncorrect
      -- | Submitted deposit
      Coin
      -- | Expected deposit taken from `PParams`
      Coin
  | -- | Some governance actions are not allowed to be voted on by certain types of
    -- Voters. This failure lists all governance action ids with their respective voters
    -- that are not allowed to vote on those governance actions.
    DisallowedVoters !(Map.Map (GovActionId (EraCrypto era)) (Voter (EraCrypto era)))
  | ConflictingCommitteeUpdate
      -- | Credentials that are mentioned as members to be both removed and added
      (Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)))
  | ExpirationEpochTooSmall
      -- | Members for which the expiration epoch has already been reached
      (Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
  | InvalidPrevGovActionId (ProposalProcedure era)
  | VotingOnExpiredGovAction (Map.Map (GovActionId (EraCrypto era)) (Voter (EraCrypto era)))
  | ProposalsCantFollow
      ( Map.Map
          -- \| The Id of the GovAction being Proposed
          (GovActionId (EraCrypto era))
          -- \| Its protocol version
          ( ProtVer
          , -- \| The ProtVer of the Previous GovAction pointed to by the one proposed
            ProtVer
          )
      )
  deriving (Eq, Show, Generic)

instance EraPParams era => NFData (ConwayGovPredFailure era)

instance EraPParams era => NoThunks (ConwayGovPredFailure era)

instance EraPParams era => DecCBOR (ConwayGovPredFailure era) where
  decCBOR = decode $ Summands "ConwayGovPredFailure" $ \case
    0 -> SumD GovActionsDoNotExist <! From
    1 -> SumD MalformedProposal <! From
    2 -> SumD ProposalProcedureNetworkIdMismatch <! From <! From
    3 -> SumD TreasuryWithdrawalsNetworkIdMismatch <! From <! From
    4 -> SumD ProposalDepositIncorrect <! From <! From
    5 -> SumD DisallowedVoters <! From
    6 -> SumD ConflictingCommitteeUpdate <! From
    7 -> SumD ExpirationEpochTooSmall <! From
    8 -> SumD InvalidPrevGovActionId <! From
    9 -> SumD VotingOnExpiredGovAction <! From
    10 -> SumD ProposalsCantFollow <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (ConwayGovPredFailure era) where
  encCBOR =
    encode . \case
      GovActionsDoNotExist gid -> Sum GovActionsDoNotExist 0 !> To gid
      MalformedProposal ga -> Sum MalformedProposal 1 !> To ga
      ProposalProcedureNetworkIdMismatch acnt nid ->
        Sum ProposalProcedureNetworkIdMismatch 2 !> To acnt !> To nid
      TreasuryWithdrawalsNetworkIdMismatch acnts nid ->
        Sum TreasuryWithdrawalsNetworkIdMismatch 3 !> To acnts !> To nid
      ProposalDepositIncorrect submitted expected ->
        Sum ProposalDepositIncorrect 4 !> To submitted !> To expected
      DisallowedVoters votes ->
        Sum DisallowedVoters 5 !> To votes
      ConflictingCommitteeUpdate members ->
        Sum ConflictingCommitteeUpdate 6 !> To members
      ExpirationEpochTooSmall members ->
        Sum ExpirationEpochTooSmall 7 !> To members
      InvalidPrevGovActionId proposal ->
        Sum InvalidPrevGovActionId 8 !> To proposal
      VotingOnExpiredGovAction ga ->
        Sum VotingOnExpiredGovAction 9 !> To ga
      ProposalsCantFollow themap ->
        Sum ProposalsCantFollow 10
          !> To themap

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

data ConwayGovEvent era
  = GovNewProposals !(TxId (EraCrypto era)) !(Proposals era)

instance ConwayEraPParams era => STS (ConwayGOV era) where
  type State (ConwayGOV era) = GovRuleState era
  type Signal (ConwayGOV era) = GovProcedures era
  type Environment (ConwayGOV era) = GovEnv era
  type BaseM (ConwayGOV era) = ShelleyBase
  type PredicateFailure (ConwayGOV era) = ConwayGovPredFailure era
  type Event (ConwayGOV era) = ConwayGovEvent era

  initialRules = []

  transitionRules = [govTransition]

addVoterVote ::
  forall era.
  Voter (EraCrypto era) ->
  Proposals era ->
  GovActionId (EraCrypto era) ->
  VotingProcedure era ->
  Proposals era
addVoterVote voter as govActionId VotingProcedure {vProcVote} =
  proposalsAddVote voter vProcVote govActionId as

checkVotesAreForValidActions ::
  EpochNo ->
  Proposals era ->
  Map.Map (GovActionId (EraCrypto era)) (Voter (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
checkVotesAreForValidActions curEpoch proposals gaIds =
  let curGovActionIds = proposalsGovActionStates proposals
      expiredActions = Map.filter ((curEpoch >) . (^. gasExpiresAfterL)) curGovActionIds
      unknownGovActionIds = gaIds `Map.difference` curGovActionIds
      votesOnExpiredActions = gaIds `Map.intersection` expiredActions
   in failureUnless
        (Map.null unknownGovActionIds)
        (GovActionsDoNotExist (Map.keysSet unknownGovActionIds))
        *> failureUnless
          (Map.null votesOnExpiredActions)
          (VotingOnExpiredGovAction votesOnExpiredActions)

checkVotersAreValid ::
  forall era.
  ConwayEraPParams era =>
  Proposals era ->
  Map.Map (GovActionId (EraCrypto era)) (Voter (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
checkVotersAreValid proposals voters =
  let curGovActionIds = proposalsGovActionStates proposals
      disallowedVoters =
        Map.merge Map.dropMissing Map.dropMissing keepDisallowedVoters curGovActionIds voters
      keepDisallowedVoters = Map.zipWithMaybeMatched $ \_ GovActionState {gasAction} voter -> do
        guard $ not $ case voter of
          CommitteeVoter {} -> isCommitteeVotingAllowed gasAction
          DRepVoter {} -> isDRepVotingAllowed gasAction
          StakePoolVoter {} -> isStakePoolVotingAllowed gasAction
        Just voter
   in failureUnless (Map.null disallowedVoters) $ DisallowedVoters disallowedVoters

actionWellFormed :: ConwayEraPParams era => GovAction era -> Test (ConwayGovPredFailure era)
actionWellFormed ga = failureUnless isWellFormed $ MalformedProposal ga
  where
    isWellFormed = case ga of
      ParameterChange _ ppd -> ppuWellFormed ppd
      _ -> True

mkGovActionState ::
  GovActionId (EraCrypto era) ->
  -- | The deposit
  Coin ->
  -- | The return address
  RewardAcnt (EraCrypto era) ->
  GovAction era ->
  -- | The number of epochs to expiry from protocol parameters
  EpochInterval ->
  -- | The current epoch
  EpochNo ->
  GovActionState era
mkGovActionState actionId deposit returnAddress action expiryInterval curEpoch =
  GovActionState
    { gasId = actionId
    , gasCommitteeVotes = mempty
    , gasDRepVotes = mempty
    , gasStakePoolVotes = mempty
    , gasDeposit = deposit
    , gasReturnAddr = returnAddress
    , gasAction = action
    , gasProposedIn = curEpoch
    , gasExpiresAfter = addEpochInterval curEpoch expiryInterval
    , gasChildren = mempty
    }

govTransition ::
  forall era.
  ConwayEraPParams era =>
  TransitionRule (ConwayGOV era)
govTransition = do
  TRC
    ( GovEnv txid currentEpoch pp prevGovActionIds
      , GovRuleState st prevChildren
      , gp
      ) <-
    judgmentContext

  expectedNetworkId <- liftSTS $ asks networkId

  runTest $ checkProposalsHaveBadProtVer pp prevGovActionIds st

  let processProposal grs@GovRuleState {..} (idx, proposal@ProposalProcedure {..}) = do
        -- PParamsUpdate well-formedness check
        runTest $ actionWellFormed pProcGovAction

        -- Deposit check
        let expectedDep = pp ^. ppGovActionDepositL
         in pProcDeposit
              == expectedDep
                ?! ProposalDepositIncorrect pProcDeposit expectedDep

        -- Return address network id check
        getRwdNetwork pProcReturnAddr
          == expectedNetworkId
            ?! ProposalProcedureNetworkIdMismatch pProcReturnAddr expectedNetworkId

        -- Treasury withdrawal return address and committee well-formedness checks
        case pProcGovAction of
          TreasuryWithdrawals wdrls ->
            let mismatchedAccounts =
                  Set.filter ((/= expectedNetworkId) . getRwdNetwork) $ Map.keysSet wdrls
             in Set.null mismatchedAccounts
                  ?! TreasuryWithdrawalsNetworkIdMismatch mismatchedAccounts expectedNetworkId
          UpdateCommittee _mPrevGovActionId membersToRemove membersToAdd _qrm -> do
            checkConflictingUpdate
            checkExpirationEpoch
            where
              checkConflictingUpdate =
                let conflicting =
                      Set.intersection
                        (Map.keysSet membersToAdd)
                        membersToRemove
                 in Set.null conflicting ?! ConflictingCommitteeUpdate conflicting
              checkExpirationEpoch =
                let invalidMembers = Map.filter (<= currentEpoch) membersToAdd
                 in Map.null invalidMembers ?! ExpirationEpochTooSmall invalidMembers
          _ -> pure ()

        -- Ancestry checks and accept proposal
        let expiry = pp ^. ppGovActionLifetimeL
            actionState =
              mkGovActionState
                (GovActionId txid idx)
                pProcDeposit
                pProcReturnAddr
                pProcGovAction
                expiry
                currentEpoch
            updatedProposalsState =
              proposalsAddProposal
                actionState
                prevGovActionIds
                grsPrevGovActionIdsChildren
                grsProposals
         in case updatedProposalsState of
              Just (updatedChildren, updatedProposals) ->
                pure $ GovRuleState updatedProposals updatedChildren
              Nothing ->
                grs <$ failBecause (InvalidPrevGovActionId proposal)

  GovRuleState proposals prevChildren' <-
    foldlM'
      processProposal
      (GovRuleState st prevChildren)
      (indexedGovProps $ SSeq.fromStrict $ OSet.toStrictSeq $ gpProposalProcedures gp)

  -- Voting
  let VotingProcedures votingProcedures = gpVotingProcedures gp
      -- Inversion of the keys in VotingProcedures, where we can find the voter for every
      -- govActionId
      govActionIdVotes =
        Map.foldlWithKey'
          (\acc voter gaIds -> Map.union (voter <$ gaIds) acc)
          Map.empty
          votingProcedures
  runTest $ checkVotesAreForValidActions currentEpoch proposals govActionIdVotes
  runTest $ checkVotersAreValid proposals govActionIdVotes

  let applyVoterVotes curState voter =
        Map.foldlWithKey' (addVoterVote voter) curState
      updatedProposalStates =
        Map.foldlWithKey' applyVoterVotes proposals votingProcedures

  -- Report the event
  tellEvent $ GovNewProposals txid updatedProposalStates

  pure $ GovRuleState updatedProposalStates prevChildren'

instance Inject (ConwayGovPredFailure era) (ConwayGovPredFailure era) where
  inject = id

-- ================================================

-- | If the GovAction is a HardFork, then return 2 things (if they exist)
--   1) The proposed ProtVer
--   2) The ProtVer of the preceeding HardFork
--   If it is not a HardFork, or the previous govActionId points to something other
--   than  HardFork, return Nothing. It will be verified with another predicate check
preceedingHardFork ::
  EraPParams era =>
  GovAction era ->
  PParams era ->
  PrevGovActionIds era ->
  Proposals era ->
  Maybe (ProtVer, ProtVer)
preceedingHardFork (HardForkInitiation mPrev newProtVer) pp pgaids ps
  | mPrev == pgaHardFork pgaids = Just (newProtVer, pp ^. ppProtocolVersionL)
  | otherwise = do
      SJust (PrevGovActionId prevGovActionId) <- Just mPrev
      HardForkInitiation _ prevProtVer <- gasAction <$> proposalsLookupId prevGovActionId ps
      Just (newProtVer, prevProtVer)
preceedingHardFork _ _ _ _ = Nothing

-- | If the GovAction part of the GovActionState is a HardFork, test that
--   it's ProtVer is legal. If it is not then return the data to make one
--   entry in the Map inside PredicateFailure 'ProposalsCantFollow'
legalProtVer ::
  EraPParams era =>
  PParams era ->
  PrevGovActionIds era ->
  Proposals era ->
  GovActionState era ->
  Maybe (GovActionId (EraCrypto era), (ProtVer, ProtVer))
legalProtVer pp pgaids ps gas = do
  (newProtVer, prevProtVer) <- preceedingHardFork (gasAction gas) pp pgaids ps
  if pvCanFollow prevProtVer newProtVer
    then Nothing
    else Just (gasId gas, (newProtVer, prevProtVer))

-- | Raise a Predicate failure if any of the proposals is a Hardfork,
--   and it does not have a legal ProtVer
checkProposalsHaveBadProtVer ::
  EraPParams era =>
  PParams era ->
  PrevGovActionIds era ->
  Proposals era ->
  Test (ConwayGovPredFailure era)
checkProposalsHaveBadProtVer pp pgaids ps =
  let invalidProposals =
        mapMaybe (legalProtVer pp pgaids ps) $ Fold.toList (proposalsActions ps)
   in failureUnless (null invalidProposals) $
        ProposalsCantFollow (Map.fromList invalidProposals)
