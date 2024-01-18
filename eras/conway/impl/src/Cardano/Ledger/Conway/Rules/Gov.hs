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
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayGOV)
import Cardano.Ledger.Conway.Governance (
  GovActionId (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovProcedures (..),
  GovPurposeId (..),
  GovRelation (..),
  ProposalProcedure (..),
  Proposals,
  Voter (..),
  VotingProcedure (..),
  foldlVotingProcedures,
  grHardForkL,
  indexedGovProps,
  isCommitteeVotingAllowed,
  isDRepVotingAllowed,
  isStakePoolVotingAllowed,
  proposalsActionsMap,
  proposalsAddAction,
  proposalsAddVote,
  proposalsLookupId,
 )
import Cardano.Ledger.Conway.Governance.Procedures (GovAction (..), foldrVotingProcedures)
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
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
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
  , gePrevGovActionIds :: !(GovRelation StrictMaybe era)
  , gePPolicy :: !(StrictMaybe (ScriptHash (EraCrypto era)))
  }
  deriving (Generic)

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
    10 -> SumD ProposalCantFollow <! From <! From <! From
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

checkVotesAreNotForExpiredActions ::
  EpochNo ->
  [(Voter (EraCrypto era), GovActionState era)] ->
  Test (ConwayGovPredFailure era)
checkVotesAreNotForExpiredActions curEpoch voters =
  let votesOnExpiredActions = filter (\(_, GovActionState {gasExpiresAfter}) -> curEpoch > gasExpiresAfter) voters
   in failureOnNonEmpty votesOnExpiredActions $
        VotingOnExpiredGovAction . fmap (second gasId)

checkVotersAreValid ::
  forall era.
  ConwayEraPParams era =>
  [(Voter (EraCrypto era), GovActionState era)] ->
  Test (ConwayGovPredFailure era)
checkVotersAreValid voters =
  let canVoteOn voter gasAction = case voter of
        CommitteeVoter {} -> isCommitteeVotingAllowed gasAction
        DRepVoter {} -> isDRepVotingAllowed gasAction
        StakePoolVoter {} -> isStakePoolVotingAllowed gasAction
      disallowedVoters =
        filter
          (\(voter, action) -> not $ voter `canVoteOn` gasAction action)
          voters
   in failureOnNonEmpty disallowedVoters $
        DisallowedVoters . fmap (second gasId)

actionWellFormed :: ConwayEraPParams era => GovAction era -> Test (ConwayGovPredFailure era)
actionWellFormed ga = failureUnless isWellFormed $ MalformedProposal ga
  where
    isWellFormed = case ga of
      ParameterChange _ ppd _ -> ppuWellFormed ppd
      _ -> True

mkGovActionState ::
  GovActionId (EraCrypto era) ->
  -- | The deposit
  Coin ->
  -- | The return address
  RewardAccount (EraCrypto era) ->
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
    }

checkPolicy ::
  StrictMaybe (ScriptHash (EraCrypto era)) ->
  StrictMaybe (ScriptHash (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
checkPolicy expectedPolicyHash actualPolicyHash =
  failureUnless (actualPolicyHash == expectedPolicyHash) $
    InvalidPolicyHash actualPolicyHash expectedPolicyHash

govTransition ::
  forall era.
  ConwayEraPParams era =>
  TransitionRule (ConwayGOV era)
govTransition = do
  TRC
    ( GovEnv txid currentEpoch pp prevGovActionIds constitutionPolicy
      , st
      , gp
      ) <-
    judgmentContext

  expectedNetworkId <- liftSTS $ asks networkId

  let processProposal ps (idx, proposal@ProposalProcedure {..}) = do
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
                pProcDeposit
                pProcReturnAddr
                pProcGovAction
                expiry
                currentEpoch
         in case proposalsAddAction actionState ps of
              Just updatedPs -> pure updatedPs
              Nothing -> ps <$ failBecause (InvalidPrevGovActionId proposal)

  proposals <-
    foldlM'
      processProposal
      st
      (indexedGovProps $ SSeq.fromStrict $ OSet.toStrictSeq $ gpProposalProcedures gp)

  -- Voting
  let votingProcedures = gpVotingProcedures gp
      -- Inversion of the keys in VotingProcedures, where we can find
      -- the voters for every govActionId
      (unknownGovActionIds, knownVotes) =
        foldrVotingProcedures
          -- strictness is not needed for `unknown`
          ( \voter gaId _ (unknown, !known) ->
              case Map.lookup gaId curGovActionIds of
                Just gas -> (unknown, (voter, gas) : known)
                Nothing -> (gaId : unknown, known)
          )
          ([], [])
          votingProcedures
      curGovActionIds = proposalsActionsMap proposals
  failOnNonEmpty unknownGovActionIds GovActionsDoNotExist
  runTest $ checkVotesAreNotForExpiredActions currentEpoch knownVotes
  runTest $ checkVotersAreValid knownVotes

  let
    addVoterVote ps voter govActionId VotingProcedure {vProcVote} =
      proposalsAddVote voter vProcVote govActionId ps
    updatedProposalStates = foldlVotingProcedures addVoterVote proposals votingProcedures

  -- Report the event
  tellEvent $ GovNewProposals txid updatedProposalStates

  pure updatedProposalStates

instance Inject (ConwayGovPredFailure era) (ConwayGovPredFailure era) where
  inject = id

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
