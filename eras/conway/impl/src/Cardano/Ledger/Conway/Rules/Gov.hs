{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Gov (
  ConwayGOV,
  GovEnv (..),
  GovSignal (..),
  ConwayGovEvent (..),
  ConwayGovPredFailure (..),
  unelectedCommitteeVoters,
) where

import Cardano.Ledger.Address (RewardAccount, raCredential, raNetwork)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Mismatch (..),
  Network,
  ProtVer,
  Relation (..),
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
  internMap,
  internSet,
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
import Cardano.Ledger.Conway.Core (ppGovActionDepositL, ppGovActionLifetimeL)
import Cardano.Ledger.Conway.Era (
  ConwayEra,
  ConwayGOV,
  hardforkConwayBootstrapPhase,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
 )
import Cardano.Ledger.Conway.Governance (
  Committee,
  ConwayEraGov,
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
  authorizedElectedHotCommitteeCredentials,
  foldrVotingProcedures,
  gasAction,
  gasDRepVotesL,
  grHardForkL,
  indexedGovProps,
  isCommitteeVotingAllowed,
  isDRepVotingAllowed,
  isStakePoolVotingAllowed,
  pProcGovActionL,
  pProcReturnAddrL,
  pRootsL,
  proposalsActionsMap,
  proposalsAddAction,
  proposalsAddVote,
  proposalsLookupId,
  toPrevGovActionIds,
 )
import Cardano.Ledger.Conway.Governance.Proposals (mapProposals)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import Cardano.Ledger.TxIn (TxId (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
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
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Pulse (foldlM')
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import qualified Lens.Micro as L
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data GovEnv era = GovEnv
  { geTxId :: TxId
  , geEpoch :: EpochNo
  , gePParams :: PParams era
  , gePPolicy :: StrictMaybe ScriptHash
  , geCertState :: CertState era
  , geCommittee :: StrictMaybe (Committee era)
  }
  deriving (Generic)

instance (EraGov era, EraPParams era, EraCertState era) => EncCBOR (GovEnv era) where
  encCBOR x@(GovEnv _ _ _ _ _ _) =
    let GovEnv {..} = x
     in encode $
          Rec GovEnv
            !> To geTxId
            !> To geEpoch
            !> To gePParams
            !> To gePPolicy
            !> To geCertState
            !> To geCommittee

instance (NFData (PParams era), Era era, EraCertState era) => NFData (GovEnv era)

deriving instance (Show (PParams era), Era era, EraCertState era) => Show (GovEnv era)

deriving instance (Eq (PParams era), EraCertState era) => Eq (GovEnv era)

data ConwayGovPredFailure era
  = GovActionsDoNotExist (NonEmpty GovActionId)
  | MalformedProposal (GovAction era)
  | ProposalProcedureNetworkIdMismatch RewardAccount Network
  | TreasuryWithdrawalsNetworkIdMismatch (Set.Set RewardAccount) Network
  | ProposalDepositIncorrect (Mismatch RelEQ Coin)
  | -- | Some governance actions are not allowed to be voted on by certain types of
    -- Voters. This failure lists all governance action ids with their respective voters
    -- that are not allowed to vote on those governance actions.
    DisallowedVoters (NonEmpty (Voter, GovActionId))
  | ConflictingCommitteeUpdate
      -- | Credentials that are mentioned as members to be both removed and added
      (Set.Set (Credential ColdCommitteeRole))
  | ExpirationEpochTooSmall
      -- | Members for which the expiration epoch has already been reached
      (Map.Map (Credential ColdCommitteeRole) EpochNo)
  | InvalidPrevGovActionId (ProposalProcedure era)
  | VotingOnExpiredGovAction (NonEmpty (Voter, GovActionId))
  | ProposalCantFollow
      -- | The PrevGovActionId of the HardForkInitiation that fails
      (StrictMaybe (GovPurposeId 'HardForkPurpose))
      -- | Its protocol version and the protocal version of the previous gov-action pointed to by the proposal
      (Mismatch RelGT ProtVer)
  | InvalidPolicyHash
      -- | The policy script hash in the proposal
      (StrictMaybe ScriptHash)
      -- | The policy script hash of the current constitution
      (StrictMaybe ScriptHash)
  | DisallowedProposalDuringBootstrap (ProposalProcedure era)
  | DisallowedVotesDuringBootstrap
      (NonEmpty (Voter, GovActionId))
  | -- | Predicate failure for votes by entities that are not present in the ledger state
    VotersDoNotExist (NonEmpty Voter)
  | -- | Treasury withdrawals that sum up to zero are not allowed
    ZeroTreasuryWithdrawals (GovAction era)
  | -- | Proposals that have an invalid reward account for returns of the deposit
    ProposalReturnAccountDoesNotExist RewardAccount
  | -- | Treasury withdrawal proposals to an invalid reward account
    TreasuryWithdrawalReturnAccountsDoNotExist (NonEmpty RewardAccount)
  | -- | Disallow votes by unelected committee members
    UnelectedCommitteeVoters (NonEmpty (Credential HotCommitteeRole))
  deriving (Eq, Show, Generic)

type instance EraRuleFailure "GOV" ConwayEra = ConwayGovPredFailure ConwayEra

type instance EraRuleEvent "GOV" ConwayEra = ConwayGovEvent ConwayEra

instance InjectRuleFailure "GOV" ConwayGovPredFailure ConwayEra

instance EraPParams era => NFData (ConwayGovPredFailure era)

instance EraPParams era => NoThunks (ConwayGovPredFailure era)

instance EraPParams era => DecCBOR (ConwayGovPredFailure era) where
  decCBOR = decode $ Summands "ConwayGovPredFailure" $ \case
    0 -> SumD GovActionsDoNotExist <! From
    1 -> SumD MalformedProposal <! From
    2 -> SumD ProposalProcedureNetworkIdMismatch <! From <! From
    3 -> SumD TreasuryWithdrawalsNetworkIdMismatch <! From <! From
    4 -> SumD ProposalDepositIncorrect <! FromGroup
    5 -> SumD DisallowedVoters <! From
    6 -> SumD ConflictingCommitteeUpdate <! From
    7 -> SumD ExpirationEpochTooSmall <! From
    8 -> SumD InvalidPrevGovActionId <! From
    9 -> SumD VotingOnExpiredGovAction <! From
    10 -> SumD ProposalCantFollow <! From <! FromGroup
    11 -> SumD InvalidPolicyHash <! From <! From
    12 -> SumD DisallowedProposalDuringBootstrap <! From
    13 -> SumD DisallowedVotesDuringBootstrap <! From
    14 -> SumD VotersDoNotExist <! From
    15 -> SumD ZeroTreasuryWithdrawals <! From
    16 -> SumD ProposalReturnAccountDoesNotExist <! From
    17 -> SumD TreasuryWithdrawalReturnAccountsDoNotExist <! From
    18 -> SumD UnelectedCommitteeVoters <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (ConwayGovPredFailure era) where
  encCBOR =
    encode . \case
      GovActionsDoNotExist gid ->
        Sum GovActionsDoNotExist 0 !> To gid
      MalformedProposal ga ->
        Sum MalformedProposal 1 !> To ga
      ProposalProcedureNetworkIdMismatch acnt nid ->
        Sum ProposalProcedureNetworkIdMismatch 2 !> To acnt !> To nid
      TreasuryWithdrawalsNetworkIdMismatch acnts nid ->
        Sum TreasuryWithdrawalsNetworkIdMismatch 3 !> To acnts !> To nid
      ProposalDepositIncorrect mm ->
        Sum ProposalDepositIncorrect 4 !> ToGroup mm
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
      ProposalCantFollow prevgaid mm ->
        Sum ProposalCantFollow 10 !> To prevgaid !> ToGroup mm
      InvalidPolicyHash got expected ->
        Sum InvalidPolicyHash 11 !> To got !> To expected
      DisallowedProposalDuringBootstrap proposal ->
        Sum DisallowedProposalDuringBootstrap 12 !> To proposal
      DisallowedVotesDuringBootstrap votes ->
        Sum DisallowedVotesDuringBootstrap 13 !> To votes
      VotersDoNotExist voters ->
        Sum VotersDoNotExist 14 !> To voters
      ZeroTreasuryWithdrawals ga ->
        Sum ZeroTreasuryWithdrawals 15 !> To ga
      ProposalReturnAccountDoesNotExist returnAccount ->
        Sum ProposalReturnAccountDoesNotExist 16 !> To returnAccount
      TreasuryWithdrawalReturnAccountsDoNotExist accounts ->
        Sum TreasuryWithdrawalReturnAccountsDoNotExist 17 !> To accounts
      UnelectedCommitteeVoters committee ->
        Sum UnelectedCommitteeVoters 18 !> To committee

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

data ConwayGovEvent era
  = GovNewProposals !TxId !(Proposals era)
  | GovRemovedVotes
      !TxId
      -- | Votes that were replaced in this tx.
      !(Set (Voter, GovActionId))
      -- | Any votes from these DReps in this or in previous txs are removed
      !(Set (Credential DRepRole))
  deriving (Generic, Eq)

instance EraPParams era => NFData (ConwayGovEvent era)

data GovSignal era = GovSignal
  { gsVotingProcedures :: !(VotingProcedures era)
  , gsProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
  , gsCertificates :: !(SSeq.StrictSeq (TxCert era))
  }
  deriving (Generic)

instance (EraPParams era, EraTxCert era) => EncCBOR (GovSignal era) where
  encCBOR x@(GovSignal _ _ _) =
    let GovSignal {..} = x
     in encode $
          Rec GovSignal
            !> To gsVotingProcedures
            !> To gsProposalProcedures
            !> To gsCertificates

deriving instance (EraPParams era, Eq (TxCert era)) => Eq (GovSignal era)

deriving instance (EraPParams era, Show (TxCert era)) => Show (GovSignal era)

instance (EraPParams era, NFData (TxCert era)) => NFData (GovSignal era)

instance
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , ConwayEraGov era
  , EraRule "GOV" era ~ ConwayGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  , EraCertState era
  , ConwayEraCertState era
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
  [(Voter, GovActionState era)] ->
  Test (ConwayGovPredFailure era)
checkVotesAreNotForExpiredActions curEpoch votes =
  checkDisallowedVotes votes VotingOnExpiredGovAction $ \GovActionState {gasExpiresAfter} _ ->
    curEpoch <= gasExpiresAfter

checkVotersAreValid ::
  forall era.
  ConwayEraPParams era =>
  EpochNo ->
  CommitteeState era ->
  [(Voter, GovActionState era)] ->
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
  [(Voter, GovActionState era)] ->
  Test (ConwayGovPredFailure era)
checkBootstrapVotes pp votes
  | hardforkConwayBootstrapPhase (pp ^. ppProtocolVersionL) =
      checkDisallowedVotes votes DisallowedVotesDuringBootstrap $ \gas ->
        \case
          DRepVoter {} | gasAction gas == InfoAction -> True
          DRepVoter {} -> False
          _ -> isBootstrapAction $ gasAction gas
  | otherwise = pure ()

actionWellFormed ::
  ConwayEraPParams era => ProtVer -> GovAction era -> Test (ConwayGovPredFailure era)
actionWellFormed pv ga = failureUnless isWellFormed $ MalformedProposal ga
  where
    isWellFormed = case ga of
      ParameterChange _ ppd _ -> ppuWellFormed pv ppd
      _ -> True

mkGovActionState ::
  GovActionId ->
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
  StrictMaybe ScriptHash ->
  StrictMaybe ScriptHash ->
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
  | hardforkConwayBootstrapPhase (pp ^. ppProtocolVersionL) =
      failureUnless (isBootstrapAction pProcGovAction) $ DisallowedProposalDuringBootstrap proposal
  | otherwise = pure ()

govTransition ::
  forall era.
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , ConwayEraGov era
  , STS (EraRule "GOV" era)
  , Event (EraRule "GOV" era) ~ ConwayGovEvent era
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , PredicateFailure (EraRule "GOV" era) ~ ConwayGovPredFailure era
  , BaseM (EraRule "GOV" era) ~ ShelleyBase
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , State (EraRule "GOV" era) ~ Proposals era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  , ConwayEraCertState era
  ) =>
  TransitionRule (EraRule "GOV" era)
govTransition = do
  TRC
    ( GovEnv txid currentEpoch pp constitutionPolicy certState committee
      , st
      , GovSignal {gsVotingProcedures, gsProposalProcedures, gsCertificates}
      ) <-
    judgmentContext
  let prevGovActionIds = st ^. pRootsL . L.to toPrevGovActionIds
      certVState = certState ^. certVStateL
      certPState = certState ^. certPStateL
      certDState = certState ^. certDStateL
      committeeState = vsCommitteeState certVState
      knownDReps = vsDReps certVState
      knownStakePools = psStakePools certPState
      knownCommitteeMembers = authorizedHotCommitteeCredentials committeeState

  expectedNetworkId <- liftSTS $ asks networkId

  when (hardforkConwayDisallowUnelectedCommitteeFromVoting $ pp ^. ppProtocolVersionL) $
    failOnNonEmpty
      (unelectedCommitteeVoters committee committeeState gsVotingProcedures)
      UnelectedCommitteeVoters

  let processProposal ps (idx, proposal@ProposalProcedure {..}) = do
        runTest $ checkBootstrapProposal pp proposal

        let newGaid = GovActionId txid idx

        -- In a HardFork, check that the ProtVer can follow
        let badHardFork = do
              (prevGaid, newProtVer, prevProtVer) <-
                preceedingHardFork @era pProcGovAction pp prevGovActionIds st
              if pvCanFollow prevProtVer newProtVer
                then Nothing
                else
                  Just $
                    ProposalCantFollow @era prevGaid $
                      Mismatch
                        { mismatchSupplied = newProtVer
                        , mismatchExpected = prevProtVer
                        }
        failOnJust badHardFork id

        -- PParamsUpdate well-formedness check
        runTest $ actionWellFormed (pp ^. ppProtocolVersionL) pProcGovAction

        unless (hardforkConwayBootstrapPhase $ pp ^. ppProtocolVersionL) $ do
          let refundAddress = proposal ^. pProcReturnAddrL
              govAction = proposal ^. pProcGovActionL
          isAccountRegistered (raCredential refundAddress) (certDState ^. accountsL)
            ?! ProposalReturnAccountDoesNotExist refundAddress
          case govAction of
            TreasuryWithdrawals withdrawals _ -> do
              let nonRegisteredAccounts =
                    flip Map.filterWithKey withdrawals $ \withdrawalAddress _ ->
                      not $ isAccountRegistered (raCredential withdrawalAddress) (certDState ^. accountsL)
              failOnNonEmpty (Map.keys nonRegisteredAccounts) TreasuryWithdrawalReturnAccountsDoNotExist
            _ -> pure ()

        -- Deposit check
        let expectedDeposit = pp ^. ppGovActionDepositL
         in pProcDeposit
              == expectedDeposit
                ?! ProposalDepositIncorrect
                  Mismatch
                    { mismatchSupplied = pProcDeposit
                    , mismatchExpected = expectedDeposit
                    }

        -- Return address network id check
        raNetwork pProcReturnAddr
          == expectedNetworkId
            ?! ProposalProcedureNetworkIdMismatch pProcReturnAddr expectedNetworkId

        -- Treasury withdrawal return address and committee well-formedness checks
        case pProcGovAction of
          TreasuryWithdrawals wdrls proposalPolicy -> do
            let mismatchedAccounts =
                  Set.filter ((/= expectedNetworkId) . raNetwork) $ Map.keysSet wdrls
            Set.null mismatchedAccounts
              ?! TreasuryWithdrawalsNetworkIdMismatch mismatchedAccounts expectedNetworkId

            -- Policy check
            runTest $ checkPolicy @era constitutionPolicy proposalPolicy

            unless (hardforkConwayBootstrapPhase $ pp ^. ppProtocolVersionL) $
              -- The sum of all withdrawals must be positive
              F.fold wdrls /= mempty ?! ZeroTreasuryWithdrawals pProcGovAction
          UpdateCommittee _mPrevGovActionId membersToRemove membersToAdd _qrm -> do
            let conflicting = Set.intersection (Map.keysSet membersToAdd) membersToRemove
             in Set.null conflicting ?! ConflictingCommitteeUpdate conflicting

            let invalidMembers = Map.filter (<= currentEpoch) membersToAdd
             in Map.null invalidMembers ?! ExpirationEpochTooSmall invalidMembers
          ParameterChange _ _ proposalPolicy ->
            runTest $ checkPolicy @era constitutionPolicy proposalPolicy
          _ -> pure ()

        -- Ancestry checks and accept proposal
        let expiry = pp ^. ppGovActionLifetimeL
            actionState = mkGovActionState newGaid proposal expiry currentEpoch
         in case proposalsAddAction actionState ps of
              Just updatedPs -> pure updatedPs
              Nothing -> ps <$ failBecause (InvalidPrevGovActionId proposal)

  proposals <-
    foldlM' processProposal st $
      indexedGovProps (SSeq.fromStrict (OSet.toStrictSeq gsProposalProcedures))

  let knownVotes = [(voter, gas) | (voter, _vote, gas) <- knownVotesWithCast]
      (unknownGovActionIds, !knownVotesWithCast, replacedVotes) =
        foldrVotingProcedures
          -- strictness is not needed for `unknown` or `replaced`
          ( \voter gaId vp (unknown, !known, replaced) ->
              case Map.lookup gaId curGovActionIds of
                Just gas ->
                  let isVoteReplaced =
                        case voter of
                          CommitteeVoter hotCred -> hotCred `Map.member` gasCommitteeVotes gas
                          DRepVoter cred -> cred `Map.member` gasDRepVotes gas
                          StakePoolVoter poolId -> poolId `Map.member` gasStakePoolVotes gas
                      replaced'
                        | isVoteReplaced = Set.insert (voter, gaId) replaced
                        | otherwise = replaced
                   in (unknown, (voter, vProcVote vp, gas) : known, replaced')
                Nothing -> (gaId : unknown, known, replaced)
          )
          ([], [], Set.empty)
          (VotingProcedures knownVoters)
      curGovActionIds = proposalsActionsMap proposals
      internVoter = \case
        CommitteeVoter hotCred -> CommitteeVoter <$> internSet hotCred knownCommitteeMembers
        DRepVoter cred -> DRepVoter <$> internMap cred knownDReps
        StakePoolVoter poolId -> StakePoolVoter <$> internMap poolId knownStakePools
      (unknownVoters, knownVoters) =
        bimap Set.fromList Map.fromList $
          partitionEithers
            [ maybe (Left voter) (\v -> Right (v, votes)) (internVoter voter)
            | (voter, votes) <- Map.toList (unVotingProcedures gsVotingProcedures)
            ]

  failOnNonEmpty unknownVoters VotersDoNotExist
  failOnNonEmpty unknownGovActionIds GovActionsDoNotExist
  runTest $ checkBootstrapVotes pp knownVotes
  runTest $ checkVotesAreNotForExpiredActions currentEpoch knownVotes
  runTest $ checkVotersAreValid currentEpoch committeeState knownVotes

  let
    !updatedProposalStates =
      let addVoterVote ps (voter, vote, gas) = proposalsAddVote voter vote (gasId gas) ps
       in cleanupProposalVotes $ F.foldl' addVoterVote proposals knownVotesWithCast
    unregisteredDReps =
      let collectRemovals drepCreds = \case
            UnRegDRepTxCert drepCred _ -> Set.insert drepCred drepCreds
            _ -> drepCreds
       in F.foldl' collectRemovals mempty gsCertificates
    cleanupProposalVotes
      -- optimization: avoid iterating over proposals when there is nothing to cleanup
      | Set.null unregisteredDReps = id
      | otherwise =
          let cleanupVoters gas =
                gas & gasDRepVotesL %~ (`Map.withoutKeys` unregisteredDReps)
           in mapProposals cleanupVoters

  -- Report the event
  tellEvent $ GovNewProposals txid updatedProposalStates
  tellEvent $ GovRemovedVotes txid replacedVotes unregisteredDReps

  pure updatedProposalStates

isBootstrapAction :: GovAction era -> Bool
isBootstrapAction =
  \case
    ParameterChange {} -> True
    HardForkInitiation {} -> True
    InfoAction -> True
    _ -> False

checkDisallowedVotes ::
  [(Voter, GovActionState era)] ->
  (NonEmpty (Voter, GovActionId) -> ConwayGovPredFailure era) ->
  (GovActionState era -> Voter -> Bool) ->
  Test (ConwayGovPredFailure era)
checkDisallowedVotes votes failure canBeVotedOnBy =
  failureOnNonEmpty disallowedVotes failure
  where
    disallowedVotes =
      [(voter, gasId gas) | (voter, gas) <- votes, not (gas `canBeVotedOnBy` voter)]

unelectedCommitteeVoters ::
  StrictMaybe (Committee era) ->
  CommitteeState era ->
  VotingProcedures era ->
  Set (Credential HotCommitteeRole)
unelectedCommitteeVoters committee committeeState =
  let authorizedElectedCommittee = authorizedElectedHotCommitteeCredentials committee committeeState
      collectUnelectedCommitteeVotes !unelectedHotCreds voter _ =
        case voter of
          CommitteeVoter hotCred
            | hotCred `Set.notMember` authorizedElectedCommittee ->
                Set.insert hotCred unelectedHotCreds
          _ -> unelectedHotCreds
   in Map.foldlWithKey' collectUnelectedCommitteeVotes Set.empty . unVotingProcedures

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
  GovRelation StrictMaybe ->
  Proposals era ->
  Maybe (StrictMaybe (GovPurposeId 'HardForkPurpose), ProtVer, ProtVer)
preceedingHardFork (HardForkInitiation mPrev newProtVer) pp pgaids ps
  | mPrev == pgaids ^. grHardForkL = Just (mPrev, newProtVer, pp ^. ppProtocolVersionL)
  | otherwise = do
      SJust (GovPurposeId prevGovActionId) <- Just mPrev
      HardForkInitiation _ prevProtVer <- gasAction <$> proposalsLookupId prevGovActionId ps
      Just (mPrev, newProtVer, prevProtVer)
preceedingHardFork _ _ _ _ = Nothing
