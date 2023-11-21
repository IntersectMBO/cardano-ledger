{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  ConwayGovPredFailure (..),
) where

import Cardano.Ledger.Address (RewardAcnt, getRwdNetwork)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Network,
  ShelleyBase,
  StrictMaybe (..),
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
  proposalsAddVote,
  proposalsInsertGovAction,
  proposalsLookupId,
 )
import Cardano.Ledger.Conway.Governance.Procedures (
  GovAction (..),
  govProceduresProposalsL,
  pProcGovActionL,
 )
import Cardano.Ledger.Conway.Governance.Proposals (proposalsGovActionStates)
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  ppGovActionDepositL,
  ppGovActionLifetimeL,
 )
import Cardano.Ledger.Conway.Rules.Ratify (prevActionAsExpected)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.Shelley.Tx (TxId (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  tellEvent,
  (?!),
 )
import qualified Data.Map.Merge.Strict as Map (dropMissing, merge, zipWithMaybeMatched)
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data GovEnv era = GovEnv
  { geTxId :: !(TxId (EraCrypto era))
  , geEpoch :: !EpochNo
  , gePParams :: !(PParams era)
  , gePrevGovActionIds :: !(PrevGovActionIds era)
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
  | InvalidPrevGovActionIdsInProposals (Seq.Seq (ProposalProcedure era))
  | VotingOnExpiredGovAction (Map.Map (GovActionId (EraCrypto era)) (Voter (EraCrypto era)))
  deriving (Eq, Show, Generic)

instance EraPParams era => ToExpr (ConwayGovPredFailure era)

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
    8 -> SumD InvalidPrevGovActionIdsInProposals <! From
    9 -> SumD VotingOnExpiredGovAction <! From
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
      InvalidPrevGovActionIdsInProposals proposals ->
        Sum InvalidPrevGovActionIdsInProposals 8 !> To proposals
      VotingOnExpiredGovAction ga ->
        Sum VotingOnExpiredGovAction 9 !> To ga

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

data ConwayGovEvent era
  = GovNewProposals !(TxId (EraCrypto era)) !(Proposals era)

instance ConwayEraPParams era => STS (ConwayGOV era) where
  type State (ConwayGOV era) = Proposals era
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

addAction ::
  EpochNo ->
  EpochInterval ->
  GovActionId (EraCrypto era) ->
  Coin ->
  RewardAcnt (EraCrypto era) ->
  GovAction era ->
  Proposals era ->
  Proposals era
addAction epoch gaExpiry gaid c addr act =
  proposalsInsertGovAction gai'
  where
    gai' =
      GovActionState
        { gasId = gaid
        , gasCommitteeVotes = mempty
        , gasDRepVotes = mempty
        , gasStakePoolVotes = mempty
        , gasDeposit = c
        , gasReturnAddr = addr
        , gasAction = act
        , gasProposedIn = epoch
        , gasExpiresAfter = addEpochInterval epoch gaExpiry
        }

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

checkProposalsHaveAValidPrevious ::
  forall era.
  PrevGovActionIds era ->
  Proposals era ->
  GovProcedures era ->
  Test (ConwayGovPredFailure era)
checkProposalsHaveAValidPrevious prevGovActionIds proposalss procedures =
  let isValidPrevGovActionId ::
        StrictMaybe (PrevGovActionId p (EraCrypto era)) ->
        (GovAction era -> Bool) ->
        Bool
      isValidPrevGovActionId prevGovActionId proposalsCond =
        case prevGovActionId of
          -- The case of having an SNothing as valid, for the very first proposal ever, is handled in `prevActionAsExpected`
          SNothing -> False
          SJust (PrevGovActionId govActionId) ->
            case proposalsLookupId govActionId proposalss of
              Nothing -> False
              -- lookup has to succeed _and_ purpose of looked-up action has to match condition
              Just found -> proposalsCond $ gasAction found
      isValid proposal =
        prevActionAsExpected (proposal ^. pProcGovActionL) prevGovActionIds
          || case proposal ^. pProcGovActionL of
            ParameterChange prev _ ->
              isValidPrevGovActionId prev $ \case ParameterChange {} -> True; _ -> False
            HardForkInitiation prev _ ->
              isValidPrevGovActionId prev $ \case HardForkInitiation {} -> True; _ -> False
            TreasuryWithdrawals _ -> True
            NoConfidence prev ->
              isValidPrevGovActionId prev $ \case NoConfidence {} -> True; UpdateCommittee {} -> True; _ -> False
            UpdateCommittee prev _ _ _ ->
              isValidPrevGovActionId prev $ \case NoConfidence {} -> True; UpdateCommittee {} -> True; _ -> False
            NewConstitution prev _ ->
              isValidPrevGovActionId prev $ \case NewConstitution {} -> True; _ -> False
            InfoAction -> True
      invalidProposals = Seq.filter (not . isValid) $ SSeq.fromStrict $ OSet.toStrictSeq $ procedures ^. govProceduresProposalsL
   in failureUnless (Seq.null invalidProposals) $ InvalidPrevGovActionIdsInProposals invalidProposals

govTransition ::
  forall era.
  ConwayEraPParams era =>
  TransitionRule (ConwayGOV era)
govTransition = do
  TRC (GovEnv txid currentEpoch pp prevGovActionIds, st, gp) <- judgmentContext
  expectedNetworkId <- liftSTS $ asks networkId

  let applyProps st' Seq.Empty = pure st'
      applyProps st' ((idx, ProposalProcedure {..}) Seq.:<| ps) = do
        let expectedDeposit = pp ^. ppGovActionDepositL
         in pProcDeposit
              == expectedDeposit
                ?! ProposalDepositIncorrect pProcDeposit expectedDeposit

        runTest $ actionWellFormed pProcGovAction

        getRwdNetwork pProcReturnAddr
          == expectedNetworkId
            ?! ProposalProcedureNetworkIdMismatch pProcReturnAddr expectedNetworkId

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

        let st'' =
              addAction
                currentEpoch
                (pp ^. ppGovActionLifetimeL)
                (GovActionId txid idx)
                pProcDeposit
                pProcReturnAddr
                pProcGovAction
                st'
        applyProps st'' ps
  stProps <- applyProps st $ indexedGovProps $ SSeq.fromStrict $ OSet.toStrictSeq $ gpProposalProcedures gp

  let VotingProcedures votingProcedures = gpVotingProcedures gp
      -- Inversion of the keys in VotingProcedures, where we can find the voter for every
      -- govActionId
      govActionIdVotes =
        Map.foldlWithKey'
          (\acc voter gaIds -> Map.union (voter <$ gaIds) acc)
          Map.empty
          votingProcedures
  runTest $ checkVotesAreForValidActions currentEpoch st govActionIdVotes
  runTest $ checkVotersAreValid st govActionIdVotes
  runTest $ checkProposalsHaveAValidPrevious prevGovActionIds st gp

  let applyVoterVotes curState voter =
        Map.foldlWithKey' (addVoterVote voter) curState
      updatedProposalStates =
        Map.foldlWithKey' applyVoterVotes stProps votingProcedures
  tellEvent $ GovNewProposals txid updatedProposalStates
  pure updatedProposalStates

instance Inject (ConwayGovPredFailure era) (ConwayGovPredFailure era) where
  inject = id
