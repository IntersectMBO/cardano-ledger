{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Gov (
  ConwayGOV,
  GovEnv (..),
  GovSignal (..),
  ConwayGovEvent (..),
  ConwayGovPredFailure (..),
) where

import Cardano.Ledger.Address (RewardAccount, raNetwork)
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
import Cardano.Ledger.CertState (
  CertState (..),
  CommitteeState (..),
  PState (..),
  VState (..),
  authorizedHotCommitteeCredentials,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayGOV)
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionId (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovPurposeId (..),
  GovRelation (..),
  ProposalProcedure (..),
  Proposals,
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  foldlVotingProcedures,
  foldrVotingProcedures,
  gasAction,
  gasDRepVotesL,
  grHardForkL,
  indexedGovProps,
  isCommitteeVotingAllowed,
  isDRepVotingAllowed,
  isStakePoolVotingAllowed,
  pRootsL,
  proposalsActionsMap,
  proposalsAddAction,
  proposalsAddVote,
  proposalsLookupId,
  toPrevGovActionIds,
 )
import Cardano.Ledger.Conway.Governance.Proposals (mapProposals)
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  ppGovActionDepositL,
  ppGovActionLifetimeL,
 )
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import Cardano.Ledger.TxIn (TxId (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  failOnJust,
  failOnNonEmpty,
  failureOnNonEmpty,
  judgmentContext,
  liftSTS,
  tellEvent,
  (?!),
 )
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Pulse (foldlM')
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import qualified Lens.Micro as L
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data GovEnv era = GovEnv
  { geTxId :: !(TxId (EraCrypto era))
  , geEpoch :: !EpochNo
  , gePParams :: !(PParams era)
  , gePPolicy :: !(StrictMaybe (ScriptHash (EraCrypto era)))
  , geCertState :: !(CertState era)
  }
  deriving (Generic)

instance (NFData (PParams era), Era era) => NFData (GovEnv era)
deriving instance (Show (PParams era), Era era) => Show (GovEnv era)
deriving instance Eq (PParams era) => Eq (GovEnv era)

data ConwayGovPredFailure era
  = GovActionsDoNotExist (NonEmpty (GovActionId (EraCrypto era)))
  | MalformedProposal (GovAction era)
  | ProposalProcedureNetworkIdMismatch (RewardAccount (EraCrypto era)) Network
  | TreasuryWithdrawalsNetworkIdMismatch (Set.Set (RewardAccount (EraCrypto era))) Network
  | ProposalDepositIncorrect
      -- | Submitted deposit
      Coin
      -- | Expected deposit taken from `PParams`
      Coin
  | -- | Some governance actions are not allowed to be voted on by certain types of
    -- Voters. This failure lists all governance action ids with their respective voters
    -- that are not allowed to vote on those governance actions.
    DisallowedVoters !(NonEmpty (Voter (EraCrypto era), GovActionId (EraCrypto era)))
  | ConflictingCommitteeUpdate
      -- | Credentials that are mentioned as members to be both removed and added
      (Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)))
  | ExpirationEpochTooSmall
      -- | Members for which the expiration epoch has already been reached
      (Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
  | InvalidPrevGovActionId (ProposalProcedure era)
  | VotingOnExpiredGovAction (NonEmpty (Voter (EraCrypto era), GovActionId (EraCrypto era)))
  | ProposalCantFollow
      -- | The PrevGovActionId of the HardForkInitiation that fails
      (StrictMaybe (GovPurposeId 'HardForkPurpose era))
      -- | Its protocol version
      ProtVer
      -- | The ProtVer of the Previous GovAction pointed to by the one proposed
      ProtVer
  | InvalidPolicyHash
      -- | The policy script hash in the proposal
      (StrictMaybe (ScriptHash (EraCrypto era)))
      -- | The policy script hash of the current constitution
      (StrictMaybe (ScriptHash (EraCrypto era)))
  | DisallowedProposalDuringBootstrap (ProposalProcedure era)
  | DisallowedVotesDuringBootstrap
      (NonEmpty (Voter (EraCrypto era), GovActionId (EraCrypto era)))
  | -- | Predicate failure for votes by entities that are not present in the ledger state
    VotersDoNotExist (NonEmpty (Voter (EraCrypto era)))
  deriving (Eq, Show, Generic)

type instance EraRuleFailure "GOV" (ConwayEra c) = ConwayGovPredFailure (ConwayEra c)

type instance EraRuleEvent "GOV" (ConwayEra c) = ConwayGovEvent (ConwayEra c)

instance InjectRuleFailure "GOV" ConwayGovPredFailure (ConwayEra c)

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
    10 -> SumD ProposalCantFollow <! From <! From <! From
    11 -> SumD InvalidPolicyHash <! From <! From
    12 -> SumD DisallowedProposalDuringBootstrap <! From
    13 -> SumD DisallowedVotesDuringBootstrap <! From
    14 -> SumD VotersDoNotExist <! From
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
      ProposalCantFollow prevgaid pv1 pv2 ->
        Sum ProposalCantFollow 10
          !> To prevgaid
          !> To pv1
          !> To pv2
      InvalidPolicyHash got expected ->
        Sum InvalidPolicyHash 11
          !> To got
          !> To expected
      DisallowedProposalDuringBootstrap proposal ->
        Sum DisallowedProposalDuringBootstrap 12 !> To proposal
      DisallowedVotesDuringBootstrap votes ->
        Sum DisallowedVotesDuringBootstrap 13 !> To votes
      VotersDoNotExist voters ->
        Sum VotersDoNotExist 14 !> To voters

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

data ConwayGovEvent era
  = GovNewProposals !(TxId (EraCrypto era)) !(Proposals era)
  deriving (Generic, Eq)

instance EraPParams era => NFData (ConwayGovEvent era)

data GovSignal era = GovSignal
  { gsVotingProcedures :: !(VotingProcedures era)
  , gsProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
  , gsCertificates :: !(SSeq.StrictSeq (TxCert era))
  }
  deriving (Generic)

deriving instance (EraPParams era, Eq (TxCert era)) => Eq (GovSignal era)
deriving instance (EraPParams era, Show (TxCert era)) => Show (GovSignal era)

instance (EraPParams era, NFData (TxCert era)) => NFData (GovSignal era)

instance
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , EraRule "GOV" era ~ ConwayGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  ) =>
  STS (ConwayGOV era)
  where
  type State (ConwayGOV era) = Proposals era
  type Signal (ConwayGOV era) = GovSignal era
  type Environment (ConwayGOV era) = GovEnv era
  type BaseM (ConwayGOV era) = ShelleyBase
  type PredicateFailure (ConwayGOV era) = ConwayGovPredFailure era
  type Event (ConwayGOV era) = ConwayGovEvent era

  initialRules = []

  transitionRules = [govTransition @era]

checkVotesAreNotForExpiredActions ::
  EpochNo ->
  [(Voter (EraCrypto era), GovActionState era)] ->
  Test (ConwayGovPredFailure era)
checkVotesAreNotForExpiredActions curEpoch votes =
  checkDisallowedVotes votes VotingOnExpiredGovAction $ \GovActionState {gasExpiresAfter} _ ->
    curEpoch <= gasExpiresAfter

checkVotersAreValid ::
  forall era.
  ConwayEraPParams era =>
  EpochNo ->
  CommitteeState era ->
  [(Voter (EraCrypto era), GovActionState era)] ->
  Test (ConwayGovPredFailure era)
checkVotersAreValid currentEpoch committeeState votes =
  checkDisallowedVotes votes DisallowedVoters $ \gas ->
    \case
      CommitteeVoter {} -> isCommitteeVotingAllowed currentEpoch committeeState (gasAction gas)
      DRepVoter {} -> isDRepVotingAllowed (gasAction gas)
      StakePoolVoter {} -> isStakePoolVotingAllowed (gasAction gas)

checkBootstrapVotes ::
  forall era.
  EraPParams era =>
  PParams era ->
  [(Voter (EraCrypto era), GovActionState era)] ->
  Test (ConwayGovPredFailure era)
checkBootstrapVotes pp votes
  | HF.bootstrapPhase (pp ^. ppProtocolVersionL) =
      checkDisallowedVotes votes DisallowedVotesDuringBootstrap $ \gas ->
        \case
          DRepVoter {} | gasAction gas == InfoAction -> True
          DRepVoter {} -> False
          _ -> isBootstrapAction $ gasAction gas
  | otherwise = pure ()

actionWellFormed :: ConwayEraPParams era => GovAction era -> Test (ConwayGovPredFailure era)
actionWellFormed ga = failureUnless isWellFormed $ MalformedProposal ga
  where
    isWellFormed = case ga of
      ParameterChange _ ppd _ -> ppuWellFormed ppd
      _ -> True

mkGovActionState ::
  GovActionId (EraCrypto era) ->
  ProposalProcedure era ->
  -- | The number of epochs to expiry from protocol parameters
  EpochInterval ->
  -- | The current epoch
  EpochNo ->
  GovActionState era
mkGovActionState actionId proposal expiryInterval curEpoch =
  GovActionState
    { gasId = actionId
    , gasCommitteeVotes = mempty
    , gasDRepVotes = mempty
    , gasStakePoolVotes = mempty
    , gasProposalProcedure = proposal
    , gasProposedIn = curEpoch
    , gasExpiresAfter = addEpochInterval curEpoch expiryInterval
    }

checkPolicy ::
  StrictMaybe (ScriptHash (EraCrypto era)) ->
  StrictMaybe (ScriptHash (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
checkPolicy expectedPolicyHash actualPolicyHash =
  failureUnless (actualPolicyHash == expectedPolicyHash) $
    InvalidPolicyHash actualPolicyHash expectedPolicyHash

checkBootstrapProposal ::
  EraPParams era =>
  PParams era ->
  ProposalProcedure era ->
  Test (ConwayGovPredFailure era)
checkBootstrapProposal pp proposal@ProposalProcedure {pProcGovAction}
  | HF.bootstrapPhase (pp ^. ppProtocolVersionL) =
      failureUnless (isBootstrapAction pProcGovAction) $ DisallowedProposalDuringBootstrap proposal
  | otherwise = pure ()

govTransition ::
  forall era.
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , STS (EraRule "GOV" era)
  , Event (EraRule "GOV" era) ~ ConwayGovEvent era
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , PredicateFailure (EraRule "GOV" era) ~ ConwayGovPredFailure era
  , BaseM (EraRule "GOV" era) ~ ShelleyBase
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , State (EraRule "GOV" era) ~ Proposals era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  ) =>
  TransitionRule (EraRule "GOV" era)
govTransition = do
  TRC
    ( GovEnv txid currentEpoch pp constitutionPolicy CertState {certPState, certVState}
      , st
      , GovSignal {gsVotingProcedures, gsProposalProcedures, gsCertificates}
      ) <-
    judgmentContext
  let prevGovActionIds = st ^. pRootsL . L.to toPrevGovActionIds
      committeeState = vsCommitteeState certVState
      knownDReps = vsDReps certVState
      knownStakePools = psStakePoolParams certPState
      knownCommitteeMembers = authorizedHotCommitteeCredentials committeeState

  expectedNetworkId <- liftSTS $ asks networkId

  let processProposal ps (idx, proposal@ProposalProcedure {..}) = do
        runTest $ checkBootstrapProposal pp proposal

        let newGaid = GovActionId txid idx

        -- In a HardFork, check that the ProtVer can follow
        let badHardFork = do
              (prevGaid, newProtVer, prevProtVer) <-
                preceedingHardFork @era pProcGovAction pp prevGovActionIds st
              if pvCanFollow prevProtVer newProtVer
                then Nothing
                else Just $ ProposalCantFollow @era prevGaid newProtVer prevProtVer
        failOnJust badHardFork id

        -- PParamsUpdate well-formedness check
        runTest $ actionWellFormed pProcGovAction

        -- Deposit check
        let expectedDep = pp ^. ppGovActionDepositL
         in pProcDeposit
              == expectedDep
                ?! ProposalDepositIncorrect pProcDeposit expectedDep

        -- Return address network id check
        raNetwork pProcReturnAddr
          == expectedNetworkId
            ?! ProposalProcedureNetworkIdMismatch pProcReturnAddr expectedNetworkId

        -- Treasury withdrawal return address and committee well-formedness checks
        case pProcGovAction of
          TreasuryWithdrawals wdrls proposalPolicy ->
            let mismatchedAccounts =
                  Set.filter ((/= expectedNetworkId) . raNetwork) $ Map.keysSet wdrls
             in do
                  Set.null mismatchedAccounts
                    ?! TreasuryWithdrawalsNetworkIdMismatch mismatchedAccounts expectedNetworkId

                  -- Policy check
                  runTest $ checkPolicy @era constitutionPolicy proposalPolicy
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
          ParameterChange _ _ proposalPolicy ->
            runTest $ checkPolicy @era constitutionPolicy proposalPolicy
          _ -> pure ()

        -- Ancestry checks and accept proposal
        let expiry = pp ^. ppGovActionLifetimeL
            actionState =
              mkGovActionState
                newGaid
                proposal
                expiry
                currentEpoch
         in case proposalsAddAction actionState ps of
              Just updatedPs -> pure updatedPs
              Nothing -> ps <$ failBecause (InvalidPrevGovActionId proposal)

  proposals <-
    foldlM' processProposal st $
      indexedGovProps $
        SSeq.fromStrict $
          OSet.toStrictSeq $
            gsProposalProcedures

  -- Inversion of the keys in VotingProcedures, where we can find the voters for every
  -- govActionId
  let (unknownGovActionIds, knownVotes) =
        foldrVotingProcedures
          -- strictness is not needed for `unknown`
          ( \voter gaId _ (unknown, !known) ->
              case Map.lookup gaId curGovActionIds of
                Just gas -> (unknown, (voter, gas) : known)
                Nothing -> (gaId : unknown, known)
          )
          ([], [])
          gsVotingProcedures
      curGovActionIds = proposalsActionsMap proposals
      isVoterKnown = \case
        CommitteeVoter hotCred -> hotCred `Set.member` knownCommitteeMembers
        DRepVoter cred -> cred `Map.member` knownDReps
        StakePoolVoter poolId -> poolId `Map.member` knownStakePools
      unknownVoters =
        Map.keys $
          Map.filterWithKey (\voter _ -> not (isVoterKnown voter)) (unVotingProcedures gsVotingProcedures)

  failOnNonEmpty unknownVoters VotersDoNotExist
  failOnNonEmpty unknownGovActionIds GovActionsDoNotExist
  runTest $ checkBootstrapVotes pp knownVotes
  runTest $ checkVotesAreNotForExpiredActions currentEpoch knownVotes
  runTest $ checkVotersAreValid currentEpoch committeeState knownVotes

  let
    addVoterVote ps voter govActionId VotingProcedure {vProcVote} =
      proposalsAddVote voter vProcVote govActionId ps
    updatedProposalStates =
      cleanupProposalVotes $
        foldlVotingProcedures addVoterVote proposals gsVotingProcedures
    unregisteredDReps =
      -- , removedAuthorizations =
      let collectRemovals drepCreds = \case
            UnRegDRepTxCert drepCred _ -> Set.insert drepCred drepCreds
            _ -> drepCreds
       in F.foldl' collectRemovals mempty gsCertificates
    -- AuthCommitteeHotKeyTxCert
    -- ResignCommitteeColdTxCert
    cleanupProposalVotes =
      let cleanupVoters gas =
            gas & gasDRepVotesL %~ (`Map.withoutKeys` unregisteredDReps)
       in mapProposals cleanupVoters

  -- Report the event
  tellEvent $ GovNewProposals txid $ updatedProposalStates

  pure updatedProposalStates

isBootstrapAction :: GovAction era -> Bool
isBootstrapAction =
  \case
    ParameterChange {} -> True
    HardForkInitiation {} -> True
    InfoAction -> True
    _ -> False

checkDisallowedVotes ::
  [(Voter (EraCrypto era), GovActionState era)] ->
  (NonEmpty (Voter (EraCrypto era), GovActionId (EraCrypto era)) -> ConwayGovPredFailure era) ->
  (GovActionState era -> Voter (EraCrypto era) -> Bool) ->
  Test (ConwayGovPredFailure era)
checkDisallowedVotes votes failure canBeVotedOnBy =
  failureOnNonEmpty disallowedVotes failure
  where
    disallowedVotes =
      [(voter, gasId gas) | (voter, gas) <- votes, not (gas `canBeVotedOnBy` voter)]

-- | If the GovAction is a HardFork, then return 3 things (if they exist)
-- 1) The (StrictMaybe GovPurposeId), pointed to by the HardFork proposal
-- 2) The proposed ProtVer
-- 3) The ProtVer of the preceeding HardFork
-- If it is not a HardFork, or the previous govActionId points to something other
-- than  HardFork, return Nothing. It will be verified with another predicate check.
preceedingHardFork ::
  EraPParams era =>
  GovAction era ->
  PParams era ->
  GovRelation StrictMaybe era ->
  Proposals era ->
  Maybe (StrictMaybe (GovPurposeId 'HardForkPurpose era), ProtVer, ProtVer)
preceedingHardFork (HardForkInitiation mPrev newProtVer) pp pgaids ps
  | mPrev == pgaids ^. grHardForkL = Just (mPrev, newProtVer, pp ^. ppProtocolVersionL)
  | otherwise = do
      SJust (GovPurposeId prevGovActionId) <- Just mPrev
      HardForkInitiation _ prevProtVer <- gasAction <$> proposalsLookupId prevGovActionId ps
      Just (mPrev, newProtVer, prevProtVer)
preceedingHardFork _ _ _ _ = Nothing
