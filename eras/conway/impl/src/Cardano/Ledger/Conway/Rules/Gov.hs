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

import Cardano.Ledger.Address (RewardAcnt)
import Cardano.Ledger.BaseTypes (EpochNo (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Era (ConwayGOV)
import Cardano.Ledger.Conway.Governance (
  ConwayGovState (..),
  GovernanceAction,
  GovernanceActionId (..),
  GovernanceActionState (..),
  GovernanceProcedures (..),
  ProposalProcedure (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  indexedGovProps,
 )
import Cardano.Ledger.Conway.Governance.Procedures (GovernanceAction (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.Shelley.Tx (TxId (..))
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data GovEnv era = GovEnv
  { teTxId :: !(TxId (EraCrypto era))
  , teEpoch :: !EpochNo
  }

data ConwayGovPredFailure era
  = GovernanceActionsDoNotExist (Set.Set (GovernanceActionId (EraCrypto era)))
  | MalformedProposal (GovernanceAction era)
  deriving (Eq, Show, Generic)

instance EraPParams era => NFData (ConwayGovPredFailure era)

instance EraPParams era => NoThunks (ConwayGovPredFailure era)

instance EraPParams era => DecCBOR (ConwayGovPredFailure era) where
  decCBOR = decode $ Summands "ConwayGovPredFailure" $ \case
    0 -> SumD GovernanceActionsDoNotExist <! From
    1 -> SumD MalformedProposal <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (ConwayGovPredFailure era) where
  encCBOR =
    encode . \case
      GovernanceActionsDoNotExist gid -> Sum GovernanceActionsDoNotExist 0 !> To gid
      MalformedProposal ga -> Sum MalformedProposal 1 !> To ga

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

instance ConwayEraPParams era => STS (ConwayGOV era) where
  type State (ConwayGOV era) = ConwayGovState era
  type Signal (ConwayGOV era) = GovernanceProcedures era
  type Environment (ConwayGOV era) = GovEnv era
  type BaseM (ConwayGOV era) = ShelleyBase
  type PredicateFailure (ConwayGOV era) = ConwayGovPredFailure era
  type Event (ConwayGOV era) = ()

  initialRules = []

  transitionRules = [govTransition]

addVoterVote ::
  Voter (EraCrypto era) ->
  ConwayGovState era ->
  GovernanceActionId (EraCrypto era) ->
  VotingProcedure era ->
  ConwayGovState era
addVoterVote voter (ConwayGovState st) govActionId VotingProcedure {vProcVote} =
  ConwayGovState $ Map.update (Just . updateVote) govActionId st
  where
    updateVote GovernanceActionState {..} =
      case voter of
        CommitteeVoter cred ->
          GovernanceActionState
            { gasCommitteeVotes = Map.insert cred vProcVote gasCommitteeVotes
            , ..
            }
        DRepVoter cred ->
          GovernanceActionState
            { gasDRepVotes = Map.insert cred vProcVote gasDRepVotes
            , ..
            }
        StakePoolVoter poolId ->
          GovernanceActionState
            { gasStakePoolVotes = Map.insert poolId vProcVote gasStakePoolVotes
            , ..
            }

addAction ::
  EpochNo ->
  GovernanceActionId (EraCrypto era) ->
  Coin ->
  RewardAcnt (EraCrypto era) ->
  GovernanceAction era ->
  ConwayGovState era ->
  ConwayGovState era
addAction epoch gaid c addr act (ConwayGovState st) =
  ConwayGovState $
    Map.insert gaid gai' st
  where
    gai' =
      GovernanceActionState
        { gasCommitteeVotes = mempty
        , gasDRepVotes = mempty
        , gasStakePoolVotes = mempty
        , gasDeposit = c
        , gasProposedIn = epoch
        , gasAction = act
        , gasReturnAddr = addr
        }

noSuchGovernanceActions ::
  ConwayGovState era ->
  Set.Set (GovernanceActionId (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
noSuchGovernanceActions (ConwayGovState st) gaIds =
  let unknownGovActionIds = Set.filter (`Map.notMember` st) gaIds
   in failureUnless (Set.null unknownGovActionIds) $
        GovernanceActionsDoNotExist unknownGovActionIds

actionWellFormed :: ConwayEraPParams era => GovernanceAction era -> Test (ConwayGovPredFailure era)
actionWellFormed ga = failureUnless isWellFormed $ MalformedProposal ga
  where
    isWellFormed = case ga of
      ParameterChange ppd -> ppuWellFormed ppd
      _ -> True

govTransition :: forall era. ConwayEraPParams era => TransitionRule (ConwayGOV era)
govTransition = do
  -- TODO Check the signatures
  TRC (GovEnv txid epoch, st, gp) <- judgmentContext

  let applyProps st' Empty = pure st'
      applyProps st' ((idx, ProposalProcedure {..}) :<| ps) = do
        runTest $ actionWellFormed pProcGovernanceAction
        let st'' =
              addAction
                epoch
                (GovernanceActionId txid idx)
                pProcDeposit
                pProcReturnAddr
                pProcGovernanceAction
                st'
        applyProps st'' ps
  stProps <- applyProps st $ indexedGovProps $ gpProposalProcedures gp

  let VotingProcedures votingProcedures = gpVotingProcedures gp

  runTest $ noSuchGovernanceActions st $ foldMap Map.keysSet votingProcedures

  let applyVoterVotes curState voter =
        Map.foldlWithKey' (addVoterVote voter) curState
  pure $ Map.foldlWithKey' applyVoterVotes stProps votingProcedures

instance Inject (ConwayGovPredFailure era) (ConwayGovPredFailure era) where
  inject = id
