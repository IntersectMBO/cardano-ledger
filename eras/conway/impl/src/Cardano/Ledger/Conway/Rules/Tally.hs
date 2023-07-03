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
{-# LANGUAGE TypeOperators #-}
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
  GovernanceProcedure (..),
  ProposalProcedure (..),
  Vote,
  VoterRole,
  VotingProcedure (..),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data TallyEnv era = TallyEnv
  { teTxId :: !(TxId (EraCrypto era))
  , teEpoch :: !EpochNo
  , teRoles :: !(Map (Credential 'Voting (EraCrypto era)) VoterRole)
  }

data ConwayTallyPredFailure era
  = VoterDoesNotHaveRole !(Credential 'Voting (EraCrypto era)) !VoterRole
  | GovernanceActionDoesNotExist !(GovernanceActionId (EraCrypto era))
  deriving (Eq, Show, Generic)

instance Era era => NFData (ConwayTallyPredFailure era)

instance Era era => NoThunks (ConwayTallyPredFailure era)

instance EraPParams era => DecCBOR (ConwayTallyPredFailure era) where
  decCBOR = decode $ Summands "ConwayTallyPredFailure" $ \case
    0 -> SumD VoterDoesNotHaveRole <! From <! From
    1 -> SumD GovernanceActionDoesNotExist <! From
    k -> Invalid k

instance EraPParams era => EncCBOR (ConwayTallyPredFailure era) where
  encCBOR =
    encode . \case
      VoterDoesNotHaveRole cred vr -> Sum (VoterDoesNotHaveRole @era) 0 !> To cred !> To vr
      GovernanceActionDoesNotExist gid -> Sum (GovernanceActionDoesNotExist @era) 1 !> To gid

instance EraPParams era => ToCBOR (ConwayTallyPredFailure era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayTallyPredFailure era) where
  fromCBOR = fromEraCBOR @era

instance Era era => STS (ConwayTALLY era) where
  type State (ConwayTALLY era) = ConwayTallyState era
  type Signal (ConwayTALLY era) = Seq (GovernanceProcedure era)
  type Environment (ConwayTALLY era) = TallyEnv era
  type BaseM (ConwayTALLY era) = ShelleyBase
  type PredicateFailure (ConwayTALLY era) = ConwayTallyPredFailure era
  type Event (ConwayTALLY era) = ()

  initialRules = []

  transitionRules = [tallyTransition]

addVote ::
  GovernanceActionId (EraCrypto era) ->
  VoterRole ->
  Credential 'Voting (EraCrypto era) ->
  Vote ->
  ConwayTallyState era ->
  ConwayTallyState era
addVote gaid role kh decision (ConwayTallyState st) =
  ConwayTallyState $
    Map.update (pure . updateVote) gaid st
  where
    updateVote GovernanceActionState {..} =
      GovernanceActionState
        { gasVotes = Map.insert (role, kh) decision gasVotes
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
        { gasVotes = mempty
        , gasDeposit = c
        , gasProposedIn = epoch
        , gasAction = act
        , gasReturnAddr = addr
        }

voterHasRole ::
  Credential 'Voting (EraCrypto era) ->
  VoterRole ->
  Map (Credential 'Voting (EraCrypto era)) VoterRole ->
  Test (ConwayTallyPredFailure era)
voterHasRole cred role vRoles = failureUnless cond $ VoterDoesNotHaveRole cred role
  where
    cond = Map.lookup cred vRoles == Just role

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
  TRC (TallyEnv txid epoch vRoles, st, govProcedures) <- judgmentContext

  let updateState st' Empty = pure st'
      updateState !st' ((x, idx) :<| xs) = do
        st'' <- case x of
          GovernanceVotingProcedure
            VotingProcedure {vProcGovActionId, vProcRole, vProcRoleKeyHash, vProcVote} -> do
              runTest $ noSuchGovernanceAction st vProcGovActionId
              runTest $ voterHasRole @era vProcRoleKeyHash vProcRole vRoles
              pure $ addVote vProcGovActionId vProcRole vProcRoleKeyHash vProcVote st'
          GovernanceProposalProcedure
            ProposalProcedure {pProcDeposit, pProcReturnAddr, pProcGovernanceAction} ->
              pure $
                addAction
                  epoch
                  (GovernanceActionId txid idx)
                  pProcDeposit
                  pProcReturnAddr
                  pProcGovernanceAction
                  st'
        updateState st'' xs

  updateState st $
    Seq.zip
      govProcedures
      (Seq.fromList [0 .. fromIntegral (Seq.length govProcedures - 1)])

instance Inject (ConwayTallyPredFailure era) (ConwayTallyPredFailure era) where
  inject = id
