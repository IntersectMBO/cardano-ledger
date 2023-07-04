{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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

module Cardano.Ledger.Conway.Rules.Tally (
  ConwayTALLY,
  TallyEnv (..),
  ConwayTallyPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayTALLY)
import Cardano.Ledger.Conway.Governance (
  ConwayTallyState (..),
  GovernanceAction,
  GovernanceActionId (..),
  GovernanceActionState (..),
  GovernanceProcedures (..),
  ProposalProcedure (..),
  Voter (..),
  VotingProcedure (..),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data TallyEnv era = TallyEnv
  { teTxId :: !(TxId (EraCrypto era))
  , teEpoch :: !EpochNo
  }

newtype ConwayTallyPredFailure era
  = GovernanceActionDoesNotExist (GovernanceActionId (EraCrypto era))
  deriving (Eq, Show, Generic)

instance Era era => NFData (ConwayTallyPredFailure era)

instance Era era => NoThunks (ConwayTallyPredFailure era)

instance EraPParams era => DecCBOR (ConwayTallyPredFailure era) where
  decCBOR = decode $ Summands "ConwayTallyPredFailure" $ \case
    0 -> SumD GovernanceActionDoesNotExist <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (ConwayTallyPredFailure era) where
  encCBOR =
    encode . \case
      GovernanceActionDoesNotExist gid -> Sum (GovernanceActionDoesNotExist @era) 0 !> To gid

instance EraPParams era => ToCBOR (ConwayTallyPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayTallyPredFailure era) where
  fromCBOR = fromEraCBOR @era

instance Era era => STS (ConwayTALLY era) where
  type State (ConwayTALLY era) = ConwayTallyState era
  type Signal (ConwayTALLY era) = GovernanceProcedures era
  type Environment (ConwayTALLY era) = TallyEnv era
  type BaseM (ConwayTALLY era) = ShelleyBase
  type PredicateFailure (ConwayTALLY era) = ConwayTallyPredFailure era
  type Event (ConwayTALLY era) = ()

  initialRules = []

  transitionRules = [tallyTransition]

addVote ::
  VotingProcedure era ->
  ConwayTallyState era ->
  ConwayTallyState era
addVote VotingProcedure {vProcGovActionId, vProcVoter, vProcVote} (ConwayTallyState st) =
  ConwayTallyState $
    Map.update (Just . updateVote) vProcGovActionId st
  where
    updateVote GovernanceActionState {..} =
      case vProcVoter of
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
  KeyHash 'Staking (EraCrypto era) ->
  GovernanceAction era ->
  ConwayTallyState era ->
  ConwayTallyState era
addAction epoch gaid c addr act (ConwayTallyState st) =
  ConwayTallyState $
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

noSuchGovernanceAction ::
  ConwayTallyState era ->
  GovernanceActionId (EraCrypto era) ->
  Test (ConwayTallyPredFailure era)
noSuchGovernanceAction (ConwayTallyState st) gaid =
  failureUnless (Map.member gaid st) $
    GovernanceActionDoesNotExist gaid

tallyTransition :: forall era. TransitionRule (ConwayTALLY era)
tallyTransition = do
  -- TODO Check the signatures
  TRC (TallyEnv txid epoch, st, GovernanceProcedures {..}) <- judgmentContext

  let applyProps _ st' Empty = pure st'
      applyProps idx st' (ProposalProcedure {..} :<| ps) = do
        let st'' =
              addAction
                epoch
                (GovernanceActionId txid idx)
                pProcDeposit
                pProcReturnAddr
                pProcGovernanceAction
                st'
        applyProps (idx + 1) st'' ps
  stProps <- applyProps 0 st gpProposalProcedures

  let applyVotes st' Empty = pure st'
      applyVotes st' (vp@VotingProcedure {..} :<| vs) = do
        runTest $ noSuchGovernanceAction st vProcGovActionId
        let !st'' = addVote vp st'
        applyVotes st'' vs
  applyVotes stProps gpVotingProcedures

instance Inject (ConwayTallyPredFailure era) (ConwayTallyPredFailure era) where
  inject = id
