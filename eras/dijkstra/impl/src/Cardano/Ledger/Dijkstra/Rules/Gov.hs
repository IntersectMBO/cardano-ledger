{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Gov (
  DijkstraGOV,
  DijkstraGovPredFailure (..),
  pattern InvalidPolicyHash,
  conwayToDijkstraGovPredFailure,
) where

import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Mismatch (..),
  Network,
  ProtVer,
  Relation (..),
  ShelleyBase,
  StrictMaybe,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord,
  invalidKey,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  GovAction (..),
  GovActionId (..),
  GovActionPurpose (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  Proposals,
  Voter (..),
 )
import Cardano.Ledger.Conway.Rules (
  ConwayGovEvent,
  ConwayGovPredFailure,
  GovEnv,
  GovSignal,
  conwayGovTransition,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraGOV)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  STS (..),
 )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Set.NonEmpty (NonEmptySet)
import GHC.Generics (Generic)

data DijkstraGovPredFailure era
  = GovActionsDoNotExist (NonEmpty GovActionId)
  | MalformedProposal (GovAction era)
  | ProposalProcedureNetworkIdMismatch AccountAddress Network
  | TreasuryWithdrawalsNetworkIdMismatch (NonEmptySet AccountAddress) Network
  | ProposalDepositIncorrect (Mismatch RelEQ Coin)
  | -- | Some governance actions are not allowed to be voted on by certain types of
    -- Voters. This failure lists all governance action ids with their respective voters
    -- that are not allowed to vote on those governance actions.
    DisallowedVoters (NonEmpty (Voter, GovActionId))
  | ConflictingCommitteeUpdate
      -- | Credentials that are mentioned as members to be both removed and added
      (NonEmptySet (Credential ColdCommitteeRole))
  | ExpirationEpochTooSmall
      -- | Members for which the expiration epoch has already been reached
      (NonEmptyMap (Credential ColdCommitteeRole) EpochNo)
  | InvalidPrevGovActionId (ProposalProcedure era)
  | VotingOnExpiredGovAction (NonEmpty (Voter, GovActionId))
  | ProposalCantFollow
      -- | The PrevGovActionId of the HardForkInitiation that fails
      (StrictMaybe (GovPurposeId 'HardForkPurpose))
      -- | Its protocol version and the protocal version of the previous gov-action pointed to by the proposal
      (Mismatch RelGT ProtVer)
  | InvalidGuardrailsScriptHash
      -- | The guardrails script hash in the proposal
      (StrictMaybe ScriptHash)
      -- | The guardrails script hash of the current constitution
      (StrictMaybe ScriptHash)
  | DisallowedProposalDuringBootstrap (ProposalProcedure era)
  | DisallowedVotesDuringBootstrap
      (NonEmpty (Voter, GovActionId))
  | -- | Predicate failure for votes by entities that are not present in the ledger state
    VotersDoNotExist (NonEmpty Voter)
  | -- | Treasury withdrawals that sum up to zero are not allowed
    ZeroTreasuryWithdrawals (GovAction era)
  | -- | Proposals that have an invalid account address for returns of the deposit
    ProposalReturnAccountDoesNotExist AccountAddress
  | -- | Treasury withdrawal proposals to an invalid account address
    TreasuryWithdrawalReturnAccountsDoNotExist (NonEmpty AccountAddress)
  | -- | Disallow votes by unelected committee members
    UnelectedCommitteeVoters (NonEmpty (Credential HotCommitteeRole))
  deriving (Eq, Show, Generic)

{-# DEPRECATED InvalidPolicyHash "In favor of `InvalidGuardrailsScriptHash`" #-}
pattern InvalidPolicyHash ::
  StrictMaybe ScriptHash -> StrictMaybe ScriptHash -> DijkstraGovPredFailure era
pattern InvalidPolicyHash got expected = InvalidGuardrailsScriptHash got expected

type instance EraRuleFailure "GOV" DijkstraEra = DijkstraGovPredFailure DijkstraEra

type instance EraRuleEvent "GOV" DijkstraEra = ConwayGovEvent DijkstraEra

instance InjectRuleFailure "GOV" DijkstraGovPredFailure DijkstraEra

instance InjectRuleFailure "GOV" ConwayGovPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraGovPredFailure

instance InjectRuleEvent "GOV" ConwayGovEvent DijkstraEra

instance EraPParams era => NFData (DijkstraGovPredFailure era)

instance EraPParams era => DecCBOR (DijkstraGovPredFailure era) where
  decCBOR = decodeRecordSum "DijkstraGovPredFailure" $ \case
    0 -> fmap (2,) $ GovActionsDoNotExist <$> decCBOR
    1 -> fmap (2,) $ MalformedProposal <$> decCBOR
    2 -> fmap (3,) $ ProposalProcedureNetworkIdMismatch <$> decCBOR <*> decCBOR
    3 -> fmap (3,) $ TreasuryWithdrawalsNetworkIdMismatch <$> decCBOR <*> decCBOR
    4 -> fmap (2,) $ ProposalDepositIncorrect <$> decCBOR
    5 -> fmap (2,) $ DisallowedVoters <$> decCBOR
    6 -> fmap (2,) $ ConflictingCommitteeUpdate <$> decCBOR
    7 -> fmap (2,) $ ExpirationEpochTooSmall <$> decCBOR
    8 -> fmap (2,) $ InvalidPrevGovActionId <$> decCBOR
    9 -> fmap (2,) $ VotingOnExpiredGovAction <$> decCBOR
    10 -> fmap (3,) $ ProposalCantFollow <$> decCBOR <*> decCBOR
    11 -> fmap (3,) $ InvalidGuardrailsScriptHash <$> decCBOR <*> decCBOR
    12 -> fmap (2,) $ DisallowedProposalDuringBootstrap <$> decCBOR
    13 -> fmap (2,) $ DisallowedVotesDuringBootstrap <$> decCBOR
    14 -> fmap (2,) $ VotersDoNotExist <$> decCBOR
    15 -> fmap (2,) $ ZeroTreasuryWithdrawals <$> decCBOR
    16 -> fmap (2,) $ ProposalReturnAccountDoesNotExist <$> decCBOR
    17 -> fmap (2,) $ TreasuryWithdrawalReturnAccountsDoNotExist <$> decCBOR
    18 -> fmap (2,) $ UnelectedCommitteeVoters <$> decCBOR
    k -> invalidKey k

instance EraPParams era => EncCBOR (DijkstraGovPredFailure era) where
  encCBOR =
    \case
      GovActionsDoNotExist gid ->
        encodeListLen 2 <> encodeWord 0 <> encCBOR gid
      MalformedProposal ga ->
        encodeListLen 2 <> encodeWord 1 <> encCBOR ga
      ProposalProcedureNetworkIdMismatch acnt nid ->
        encodeListLen 3 <> encodeWord 2 <> encCBOR acnt <> encCBOR nid
      TreasuryWithdrawalsNetworkIdMismatch acnts nid ->
        encodeListLen 3 <> encodeWord 3 <> encCBOR acnts <> encCBOR nid
      ProposalDepositIncorrect mm ->
        encodeListLen 2 <> encodeWord 4 <> encCBOR mm
      DisallowedVoters votes ->
        encodeListLen 2 <> encodeWord 5 <> encCBOR votes
      ConflictingCommitteeUpdate members ->
        encodeListLen 2 <> encodeWord 6 <> encCBOR members
      ExpirationEpochTooSmall members ->
        encodeListLen 2 <> encodeWord 7 <> encCBOR members
      InvalidPrevGovActionId proposal ->
        encodeListLen 2 <> encodeWord 8 <> encCBOR proposal
      VotingOnExpiredGovAction ga ->
        encodeListLen 2 <> encodeWord 9 <> encCBOR ga
      ProposalCantFollow prevgaid mm ->
        encodeListLen 3 <> encodeWord 10 <> encCBOR prevgaid <> encCBOR mm
      InvalidGuardrailsScriptHash got expected ->
        encodeListLen 3 <> encodeWord 11 <> encCBOR got <> encCBOR expected
      DisallowedProposalDuringBootstrap proposal ->
        encodeListLen 2 <> encodeWord 12 <> encCBOR proposal
      DisallowedVotesDuringBootstrap votes ->
        encodeListLen 2 <> encodeWord 13 <> encCBOR votes
      VotersDoNotExist voters ->
        encodeListLen 2 <> encodeWord 14 <> encCBOR voters
      ZeroTreasuryWithdrawals ga ->
        encodeListLen 2 <> encodeWord 15 <> encCBOR ga
      ProposalReturnAccountDoesNotExist returnAccount ->
        encodeListLen 2 <> encodeWord 16 <> encCBOR returnAccount
      TreasuryWithdrawalReturnAccountsDoNotExist accounts ->
        encodeListLen 2 <> encodeWord 17 <> encCBOR accounts
      UnelectedCommitteeVoters committee ->
        encodeListLen 2 <> encodeWord 18 <> encCBOR committee

instance
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , ConwayEraGov era
  , EraRule "GOV" era ~ DijkstraGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  , InjectRuleEvent "GOV" ConwayGovEvent era
  , EraCertState era
  , ConwayEraCertState era
  ) =>
  STS (DijkstraGOV era)
  where
  type State (DijkstraGOV era) = Proposals era
  type Signal (DijkstraGOV era) = GovSignal era
  type Environment (DijkstraGOV era) = GovEnv era
  type BaseM (DijkstraGOV era) = ShelleyBase
  type PredicateFailure (DijkstraGOV era) = DijkstraGovPredFailure era
  type Event (DijkstraGOV era) = ConwayGovEvent era

  initialRules = []

  transitionRules = [conwayGovTransition]

conwayToDijkstraGovPredFailure :: forall era. ConwayGovPredFailure era -> DijkstraGovPredFailure era
conwayToDijkstraGovPredFailure = \case
  Conway.GovActionsDoNotExist gaId -> GovActionsDoNotExist gaId
  Conway.MalformedProposal ga -> MalformedProposal ga
  Conway.ProposalProcedureNetworkIdMismatch ras n -> ProposalProcedureNetworkIdMismatch ras n
  Conway.TreasuryWithdrawalsNetworkIdMismatch ras n -> TreasuryWithdrawalsNetworkIdMismatch ras n
  Conway.ProposalDepositIncorrect mm -> ProposalDepositIncorrect mm
  Conway.DisallowedVoters vs -> DisallowedVoters vs
  Conway.ConflictingCommitteeUpdate ccrs -> ConflictingCommitteeUpdate ccrs
  Conway.ExpirationEpochTooSmall ccrs -> ExpirationEpochTooSmall ccrs
  Conway.InvalidPrevGovActionId pp -> InvalidPrevGovActionId pp
  Conway.VotingOnExpiredGovAction ga -> VotingOnExpiredGovAction ga
  Conway.ProposalCantFollow gpId mm -> ProposalCantFollow gpId mm
  Conway.InvalidGuardrailsScriptHash sh1 sh2 -> InvalidGuardrailsScriptHash sh1 sh2
  Conway.DisallowedProposalDuringBootstrap pp -> DisallowedProposalDuringBootstrap pp
  Conway.DisallowedVotesDuringBootstrap vs -> DisallowedVotesDuringBootstrap vs
  Conway.VotersDoNotExist vs -> VotersDoNotExist vs
  Conway.ZeroTreasuryWithdrawals ga -> ZeroTreasuryWithdrawals ga
  Conway.ProposalReturnAccountDoesNotExist ra -> ProposalReturnAccountDoesNotExist ra
  Conway.TreasuryWithdrawalReturnAccountsDoNotExist ra -> TreasuryWithdrawalReturnAccountsDoNotExist ra
  Conway.UnelectedCommitteeVoters vs -> UnelectedCommitteeVoters vs
