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

import Cardano.Ledger.Address (RewardAccount)
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
import NoThunks.Class (NoThunks (..))

data DijkstraGovPredFailure era
  = GovActionsDoNotExist (NonEmpty GovActionId)
  | MalformedProposal (GovAction era)
  | ProposalProcedureNetworkIdMismatch RewardAccount Network
  | TreasuryWithdrawalsNetworkIdMismatch (NonEmptySet RewardAccount) Network
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
  | -- | Proposals that have an invalid reward account for returns of the deposit
    ProposalReturnAccountDoesNotExist RewardAccount
  | -- | Treasury withdrawal proposals to an invalid reward account
    TreasuryWithdrawalReturnAccountsDoNotExist (NonEmpty RewardAccount)
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

instance EraPParams era => NFData (DijkstraGovPredFailure era)

instance EraPParams era => NoThunks (DijkstraGovPredFailure era)

instance EraPParams era => DecCBOR (DijkstraGovPredFailure era) where
  decCBOR = decode $ Summands "DijkstraGovPredFailure" $ \case
    0 -> SumD GovActionsDoNotExist <! From
    1 -> SumD MalformedProposal <! From
    2 -> SumD ProposalProcedureNetworkIdMismatch <! From <! From
    3 -> SumD TreasuryWithdrawalsNetworkIdMismatch <! From <! From
    4 -> SumD ProposalDepositIncorrect <! From
    5 -> SumD DisallowedVoters <! From
    6 -> SumD ConflictingCommitteeUpdate <! From
    7 -> SumD ExpirationEpochTooSmall <! From
    8 -> SumD InvalidPrevGovActionId <! From
    9 -> SumD VotingOnExpiredGovAction <! From
    10 -> SumD ProposalCantFollow <! From <! From
    11 -> SumD InvalidGuardrailsScriptHash <! From <! From
    12 -> SumD DisallowedProposalDuringBootstrap <! From
    13 -> SumD DisallowedVotesDuringBootstrap <! From
    14 -> SumD VotersDoNotExist <! From
    15 -> SumD ZeroTreasuryWithdrawals <! From
    16 -> SumD ProposalReturnAccountDoesNotExist <! From
    17 -> SumD TreasuryWithdrawalReturnAccountsDoNotExist <! From
    18 -> SumD UnelectedCommitteeVoters <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (DijkstraGovPredFailure era) where
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
        Sum ProposalDepositIncorrect 4 !> To mm
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
        Sum ProposalCantFollow 10 !> To prevgaid !> To mm
      InvalidGuardrailsScriptHash got expected ->
        Sum InvalidGuardrailsScriptHash 11 !> To got !> To expected
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

instance EraPParams era => ToCBOR (DijkstraGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (DijkstraGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

instance
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , ConwayEraGov era
  , EraRule "GOV" era ~ DijkstraGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
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

  transitionRules = [conwayGovTransition @era]

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
