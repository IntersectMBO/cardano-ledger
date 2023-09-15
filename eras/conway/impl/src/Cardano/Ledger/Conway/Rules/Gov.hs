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
  ConwayGovPredFailure (..),
) where

import Cardano.Ledger.Address (RewardAcnt, getRwdNetwork)
import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Network,
  ShelleyBase,
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
  GovSnapshots (..),
  ProposalProcedure (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  curGovSnapshotsL,
  indexedGovProps,
  isCommitteeVotingAllowed,
  isDRepVotingAllowed,
  isStakePoolVotingAllowed,
  snapshotAddVote,
  snapshotInsertGovAction,
 )
import Cardano.Ledger.Conway.Governance.Procedures (
  GovAction (..),
 )
import Cardano.Ledger.Conway.Governance.Snapshots (snapshotGovActionStates)
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  ppGovActionDepositL,
  ppGovActionExpirationL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.Shelley.Tx (TxId (..))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  (?!),
 )
import qualified Data.Map.Merge.Strict as Map (dropMissing, merge, zipWithMaybeMatched)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (^.))
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data GovEnv era = GovEnv
  { geTxId :: !(TxId (EraCrypto era))
  , geEpoch :: !EpochNo
  , gePParams :: !(PParams era)
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

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

instance ConwayEraPParams era => STS (ConwayGOV era) where
  type State (ConwayGOV era) = GovSnapshots era
  type Signal (ConwayGOV era) = GovProcedures era
  type Environment (ConwayGOV era) = GovEnv era
  type BaseM (ConwayGOV era) = ShelleyBase
  type PredicateFailure (ConwayGOV era) = ConwayGovPredFailure era
  type Event (ConwayGOV era) = ()

  initialRules = []

  transitionRules = [govTransition]

addVoterVote ::
  forall era.
  Voter (EraCrypto era) ->
  GovSnapshots era ->
  GovActionId (EraCrypto era) ->
  VotingProcedure era ->
  GovSnapshots era
addVoterVote voter as govActionId VotingProcedure {vProcVote} =
  as & curGovSnapshotsL %~ snapshotAddVote voter vProcVote govActionId

addAction ::
  EpochNo ->
  EpochNo ->
  GovActionId (EraCrypto era) ->
  Coin ->
  RewardAcnt (EraCrypto era) ->
  GovAction era ->
  GovSnapshots era ->
  GovSnapshots era
addAction epoch gaExpiry gaid c addr act as =
  as & curGovSnapshotsL %~ snapshotInsertGovAction gai'
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
        , gasExpiresAfter = epoch + gaExpiry
        }

noSuchGovActions ::
  GovSnapshots era ->
  Map.Map (GovActionId (EraCrypto era)) v ->
  Test (ConwayGovPredFailure era)
noSuchGovActions govSnapshots gaIds =
  let curGovActionIds = snapshotGovActionStates $ curGovSnapshots govSnapshots
      unknownGovActionIds = gaIds `Map.difference` curGovActionIds
   in failureUnless (Map.null unknownGovActionIds) $
        GovActionsDoNotExist (Map.keysSet unknownGovActionIds)

checkVotesAreValid ::
  forall era.
  ConwayEraPParams era =>
  GovSnapshots era ->
  Map.Map (GovActionId (EraCrypto era)) (Voter (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
checkVotesAreValid govSnapshots voters =
  let curGovActionIds = snapshotGovActionStates $ curGovSnapshots govSnapshots
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

govTransition ::
  forall era.
  ConwayEraPParams era =>
  TransitionRule (ConwayGOV era)
govTransition = do
  TRC (GovEnv txid currentEpoch pp, st, gp) <- judgmentContext
  expectedNetworkId <- liftSTS $ asks networkId

  let applyProps st' Empty = pure st'
      applyProps st' ((idx, ProposalProcedure {..}) :<| ps) = do
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
                (pp ^. ppGovActionExpirationL)
                (GovActionId txid idx)
                pProcDeposit
                pProcReturnAddr
                pProcGovAction
                st'
        applyProps st'' ps
  stProps <- applyProps st $ indexedGovProps $ gpProposalProcedures gp

  let VotingProcedures votingProcedures = gpVotingProcedures gp
      -- Inversion of the keys in VotingProcedures, where we can find the voter for every
      -- govActionId
      govActionIdVotes =
        Map.foldlWithKey'
          (\acc voter gaIds -> Map.union (voter <$ gaIds) acc)
          Map.empty
          votingProcedures
  runTest $ noSuchGovActions st govActionIdVotes
  runTest $ checkVotesAreValid st govActionIdVotes

  let applyVoterVotes curState voter =
        Map.foldlWithKey' (addVoterVote voter) curState
  pure $ Map.foldlWithKey' applyVoterVotes stProps votingProcedures

instance Inject (ConwayGovPredFailure era) (ConwayGovPredFailure era) where
  inject = id
