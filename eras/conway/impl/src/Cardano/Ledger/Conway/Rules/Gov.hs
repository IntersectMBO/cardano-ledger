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
import Cardano.Ledger.Conway.Core (
  ConwayEraPParams (..),
  ppGovActionDepositL,
  ppGovActionExpirationL,
  ppMinCommitteeSizeL,
 )
import Cardano.Ledger.Conway.Era (ConwayGOV)
import Cardano.Ledger.Conway.Governance (
  GovActionId (..),
  GovActionState (..),
  GovActionsState (..),
  GovProcedures (..),
  ProposalProcedure (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  curGovActionsStateL,
  indexedGovProps,
 )
import Cardano.Ledger.Conway.Governance.Procedures (
  GovAction (..),
  committeeMembersL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.Shelley.Tx (TxId (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  (?!),
 )
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
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
  | NewCommitteeSizeTooSmall
      -- | Size of `Committee` in `ProposalProcedure`
      Natural
      -- | `ppMinCommitteeSize` from `PParams`
      Natural
  | ProposalDepositIncorrect
      -- | Submitted deposit
      Coin
      -- | Expected deposit taken from `PParams`
      Coin
  deriving (Eq, Show, Generic)

instance EraPParams era => NFData (ConwayGovPredFailure era)

instance EraPParams era => NoThunks (ConwayGovPredFailure era)

instance EraPParams era => DecCBOR (ConwayGovPredFailure era) where
  decCBOR = decode $ Summands "ConwayGovPredFailure" $ \case
    0 -> SumD GovActionsDoNotExist <! From
    1 -> SumD MalformedProposal <! From
    2 -> SumD ProposalProcedureNetworkIdMismatch <! From <! From
    3 -> SumD TreasuryWithdrawalsNetworkIdMismatch <! From <! From
    4 -> SumD NewCommitteeSizeTooSmall <! From <! From
    5 -> SumD ProposalDepositIncorrect <! From <! From
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
      NewCommitteeSizeTooSmall submitted expected ->
        Sum NewCommitteeSizeTooSmall 4 !> To submitted !> To expected
      ProposalDepositIncorrect submitted expected ->
        Sum ProposalDepositIncorrect 5 !> To submitted !> To expected

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

instance ConwayEraPParams era => STS (ConwayGOV era) where
  type State (ConwayGOV era) = GovActionsState era
  type Signal (ConwayGOV era) = GovProcedures era
  type Environment (ConwayGOV era) = GovEnv era
  type BaseM (ConwayGOV era) = ShelleyBase
  type PredicateFailure (ConwayGOV era) = ConwayGovPredFailure era
  type Event (ConwayGOV era) = ()

  initialRules = []

  transitionRules = [govTransition]

addVoterVote ::
  Voter (EraCrypto era) ->
  GovActionsState era ->
  GovActionId (EraCrypto era) ->
  VotingProcedure era ->
  GovActionsState era
addVoterVote voter as govActionId VotingProcedure {vProcVote} =
  as & curGovActionsStateL %~ Map.update (Just . updateVote) govActionId
  where
    updateVote GovActionState {..} =
      case voter of
        CommitteeVoter cred ->
          GovActionState
            { gasCommitteeVotes = Map.insert cred vProcVote gasCommitteeVotes
            , ..
            }
        DRepVoter cred ->
          GovActionState
            { gasDRepVotes = Map.insert cred vProcVote gasDRepVotes
            , ..
            }
        StakePoolVoter poolId ->
          GovActionState
            { gasStakePoolVotes = Map.insert poolId vProcVote gasStakePoolVotes
            , ..
            }

addAction ::
  EpochNo ->
  EpochNo ->
  GovActionId (EraCrypto era) ->
  Coin ->
  RewardAcnt (EraCrypto era) ->
  GovAction era ->
  GovActionsState era ->
  GovActionsState era
addAction epoch gaExpiry gaid c addr act as =
  as & curGovActionsStateL %~ Map.insert gaid gai'
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
  GovActionsState era ->
  Set.Set (GovActionId (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
noSuchGovActions gas gaIds =
  let unknownGovActionIds = Set.filter (`Map.notMember` curGovActionsState gas) gaIds
   in failureUnless (Set.null unknownGovActionIds) $
        GovActionsDoNotExist unknownGovActionIds

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
  TRC (GovEnv txid epoch pp, st, gp) <- judgmentContext

  let applyProps st' Empty = pure st'
      applyProps st' ((idx, ProposalProcedure {..}) :<| ps) = do
        let expectedDeposit = pp ^. ppGovActionDepositL
         in pProcDeposit
              == expectedDeposit
                ?! ProposalDepositIncorrect pProcDeposit expectedDeposit

        runTest $ actionWellFormed pProcGovAction

        expectedNetworkId <- liftSTS $ asks networkId
        getRwdNetwork pProcReturnAddr
          == expectedNetworkId
            ?! ProposalProcedureNetworkIdMismatch pProcReturnAddr expectedNetworkId

        case pProcGovAction of
          TreasuryWithdrawals wdrls ->
            let mismatchedAccounts =
                  Set.filter ((/= expectedNetworkId) . getRwdNetwork) $ Map.keysSet wdrls
             in Set.null mismatchedAccounts
                  ?! TreasuryWithdrawalsNetworkIdMismatch mismatchedAccounts expectedNetworkId
          NewCommittee _mPrevGovActionId _oldColdCreds newCommittee ->
            let minCommitteeSize = pp ^. ppMinCommitteeSizeL
                newCommitteeSize = fromIntegral . Map.size $ newCommittee ^. committeeMembersL
             in newCommitteeSize
                  >= minCommitteeSize
                    ?! NewCommitteeSizeTooSmall newCommitteeSize minCommitteeSize
          _ -> pure ()

        let st'' =
              addAction
                epoch
                (pp ^. ppGovActionExpirationL)
                (GovActionId txid idx)
                pProcDeposit
                pProcReturnAddr
                pProcGovAction
                st'
        applyProps st'' ps
  stProps <- applyProps st $ indexedGovProps $ gpProposalProcedures gp

  let VotingProcedures votingProcedures = gpVotingProcedures gp

  runTest $ noSuchGovActions st $ foldMap Map.keysSet votingProcedures

  let applyVoterVotes curState voter =
        Map.foldlWithKey' (addVoterVote voter) curState
  pure $ Map.foldlWithKey' applyVoterVotes stProps votingProcedures

instance Inject (ConwayGovPredFailure era) (ConwayGovPredFailure era) where
  inject = id
