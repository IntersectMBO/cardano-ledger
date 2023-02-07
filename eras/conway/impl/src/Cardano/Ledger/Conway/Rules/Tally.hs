{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Tally (
  ConwayTALLY,
  TallyEnv (..),
  GovernanceMetadata (..),
  GovernanceProcedure (..),
  ConwayTallyPredFailure,
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), ShelleyBase, Url)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayTALLY)
import Cardano.Ledger.Conway.Governance (
  ConwayTallyState (..),
  GovernanceAction,
  GovernanceActionId (..),
  GovernanceActionState (..),
  VoteDecision,
  VoterRole,
 )
import Cardano.Ledger.Core (Era (..), EraPParams)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Tx (TxId (..))
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import Data.ByteString (ByteString)
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

data GovernanceMetadata c = GovernanceMetadata
  { gmUrl :: !Url
  , gmHash :: !(SafeHash c ByteString)
  }
  deriving (Eq, Generic)

instance Crypto c => DecCBOR (GovernanceMetadata c) where
  decCBOR =
    decode $
      RecD GovernanceMetadata
        <! From
        <! From

instance Crypto c => EncCBOR (GovernanceMetadata c) where
  encCBOR GovernanceMetadata {..} =
    encode $
      Rec GovernanceMetadata
        !> To gmUrl
        !> To gmHash

instance NoThunks (GovernanceMetadata era)

instance NFData (GovernanceMetadata era)

deriving instance Show (GovernanceMetadata era)

data GovernanceProcedure era
  = VotingProcedure
      !(GovernanceActionId (EraCrypto era))
      !VoterRole
      !(Credential 'Voting (EraCrypto era))
      !VoteDecision
      !(GovernanceMetadata (EraCrypto era))
  | ProposalProcedure
      !Coin
      !(KeyHash 'Staking (EraCrypto era))
      !(GovernanceAction era)
      !(GovernanceMetadata (EraCrypto era))
  deriving (Eq, Generic)

instance Era era => NFData (ConwayTallyPredFailure era)

instance
  ( Era era
  , EraPParams era
  ) =>
  DecCBOR (GovernanceProcedure era)
  where
  decCBOR =
    decode $
      Summands "GovernanceProcedure" dec
    where
      dec 0 =
        SumD VotingProcedure
          <! From
          <! From
          <! From
          <! From
          <! From
      dec 1 =
        SumD ProposalProcedure
          <! From
          <! From
          <! From
          <! From
      dec n = Invalid n

instance (Era era, EraPParams era) => EncCBOR (GovernanceProcedure era) where
  encCBOR (VotingProcedure gaid role cred decision gmd) =
    encode @_ @(GovernanceProcedure era) $
      Sum VotingProcedure 0
        !> To gaid
        !> To role
        !> To cred
        !> To decision
        !> To gmd
  encCBOR (ProposalProcedure c kh ga gmd) =
    encode @_ @(GovernanceProcedure era) $
      Sum ProposalProcedure 1
        !> To c
        !> To kh
        !> To ga
        !> To gmd

instance EraPParams era => NoThunks (GovernanceProcedure era)

instance EraPParams era => NFData (GovernanceProcedure era)

deriving instance EraPParams era => Show (GovernanceProcedure era)

data ConwayTallyPredFailure era
  = VoterDoesNotHaveRole !(Credential 'Voting (EraCrypto era)) !VoterRole
  | GovernanceActionDoesNotExist !(GovernanceActionId (EraCrypto era))
  deriving (Eq, Show, Generic)

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
  VoteDecision ->
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
          VotingProcedure gaid role kh vote _ -> do
            runTest $ noSuchGovernanceAction st gaid
            runTest $ voterHasRole @era kh role vRoles
            pure $ addVote gaid role kh vote st'
          ProposalProcedure c kh action _ ->
            pure $
              addAction
                epoch
                (GovernanceActionId txid idx)
                c
                kh
                action
                st'
        updateState st'' xs

  updateState st $ Seq.zip govProcedures [0 ..]

instance Inject (ConwayTallyPredFailure era) (ConwayTallyPredFailure era) where
  inject = id
