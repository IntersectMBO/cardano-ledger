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
import Cardano.Ledger.Conway.Gov (
  GovAction,
  GovActionId (..),
  GovActionState (..),
  GovActionsState (..),
  GovProcedures (..),
  ProposalProcedure (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  indexedGovProps,
 )
import Cardano.Ledger.Conway.Gov.Procedures (GovAction (..))
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
  = GovActionsDoNotExist (Set.Set (GovActionId (EraCrypto era)))
  | MalformedProposal (GovAction era)
  deriving (Eq, Show, Generic)

instance EraPParams era => NFData (ConwayGovPredFailure era)

instance EraPParams era => NoThunks (ConwayGovPredFailure era)

instance EraPParams era => DecCBOR (ConwayGovPredFailure era) where
  decCBOR = decode $ Summands "ConwayGovPredFailure" $ \case
    0 -> SumD GovActionsDoNotExist <! From
    1 -> SumD MalformedProposal <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (ConwayGovPredFailure era) where
  encCBOR =
    encode . \case
      GovActionsDoNotExist gid -> Sum GovActionsDoNotExist 0 !> To gid
      MalformedProposal ga -> Sum MalformedProposal 1 !> To ga

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
addVoterVote voter (GovActionsState st) govActionId VotingProcedure {vProcVote} =
  GovActionsState $ Map.update (Just . updateVote) govActionId st
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
  GovActionId (EraCrypto era) ->
  Coin ->
  RewardAcnt (EraCrypto era) ->
  GovAction era ->
  GovActionsState era ->
  GovActionsState era
addAction epoch gaid c addr act (GovActionsState st) =
  GovActionsState $
    Map.insert gaid gai' st
  where
    gai' =
      GovActionState
        { gasCommitteeVotes = mempty
        , gasDRepVotes = mempty
        , gasStakePoolVotes = mempty
        , gasDeposit = c
        , gasProposedIn = epoch
        , gasAction = act
        , gasReturnAddr = addr
        }

noSuchGovActions ::
  GovActionsState era ->
  Set.Set (GovActionId (EraCrypto era)) ->
  Test (ConwayGovPredFailure era)
noSuchGovActions (GovActionsState st) gaIds =
  let unknownGovActionIds = Set.filter (`Map.notMember` st) gaIds
   in failureUnless (Set.null unknownGovActionIds) $
        GovActionsDoNotExist unknownGovActionIds

actionWellFormed :: ConwayEraPParams era => GovAction era -> Test (ConwayGovPredFailure era)
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
        runTest $ actionWellFormed pProcGovAction
        let st'' =
              addAction
                epoch
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
