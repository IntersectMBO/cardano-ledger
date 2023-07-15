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

module Cardano.Ledger.Conway.Rules.Gov (
  ConwayGOV,
  GovEnv (..),
  ConwayGovPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
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

data GovEnv era = GovEnv
  { teTxId :: !(TxId (EraCrypto era))
  , teEpoch :: !EpochNo
  }

newtype ConwayGovPredFailure era
  = GovernanceActionDoesNotExist (GovernanceActionId (EraCrypto era))
  deriving (Eq, Show, Generic)

instance Era era => NFData (ConwayGovPredFailure era)

instance Era era => NoThunks (ConwayGovPredFailure era)

instance EraPParams era => DecCBOR (ConwayGovPredFailure era) where
  decCBOR = decode $ Summands "ConwayGovPredFailure" $ \case
    0 -> SumD GovernanceActionDoesNotExist <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (ConwayGovPredFailure era) where
  encCBOR =
    encode . \case
      GovernanceActionDoesNotExist gid -> Sum (GovernanceActionDoesNotExist @era) 0 !> To gid

instance EraPParams era => ToCBOR (ConwayGovPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovPredFailure era) where
  fromCBOR = fromEraCBOR @era

instance Era era => STS (ConwayGOV era) where
  type State (ConwayGOV era) = ConwayGovState era
  type Signal (ConwayGOV era) = GovernanceProcedures era
  type Environment (ConwayGOV era) = GovEnv era
  type BaseM (ConwayGOV era) = ShelleyBase
  type PredicateFailure (ConwayGOV era) = ConwayGovPredFailure era
  type Event (ConwayGOV era) = ()

  initialRules = []

  transitionRules = [govTransition]

addVote ::
  VotingProcedure era ->
  ConwayGovState era ->
  ConwayGovState era
addVote VotingProcedure {vProcGovActionId, vProcVoter, vProcVote} (ConwayGovState st) =
  ConwayGovState $
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

noSuchGovernanceAction ::
  ConwayGovState era ->
  GovernanceActionId (EraCrypto era) ->
  Test (ConwayGovPredFailure era)
noSuchGovernanceAction (ConwayGovState st) gaid =
  failureUnless (Map.member gaid st) $
    GovernanceActionDoesNotExist gaid

govTransition :: forall era. TransitionRule (ConwayGOV era)
govTransition = do
  TRC (GovEnv txid epoch, st, GovernanceProcedures {..}) <- judgmentContext

  let applyProps _ st' Empty = pure st'
      applyProps idx st' (ProposalProcedure {..} :<| ps) = do
        -- TODO: Verify wellformedness of PParamsUpdates
        -- named `proposalWellFormed` in the spec
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

instance Inject (ConwayGovPredFailure era) (ConwayGovPredFailure era) where
  inject = id
