{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Tally (
  ConwayTALLY,
  TallyEnv (..),
  GovernanceProcedure (..),
  ConwayTallyPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Era (ConwayTALLY)
import Cardano.Ledger.Conway.Governance (
  ConwayTallyState (..),
  GovernanceActionId (..),
  GovernanceActionInfo (..),
  GovernanceActionState (..),
  Vote (..),
  makeGovAction,
 )
import Cardano.Ledger.Core (Era (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.Shelley.Tx (TxId (..))
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Validation (failureUnless)

newtype TallyEnv era = TallyEnv (TxId (EraCrypto era))

data GovernanceProcedure era
  = VotingProcedure !(Vote era)
  | ProposalProcedure !(GovernanceActionInfo era)

newtype ConwayTallyPredFailure era
  = NoSuchGovernanceAction (Vote era)
  deriving (Eq, Show)

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
  Vote era ->
  ConwayTallyState era ->
  ConwayTallyState era
addVote gaid vote@Vote {..} (ConwayTallyState st) =
  ConwayTallyState $
    Map.update (pure . updateVote) gaid st
  where
    updateVote GovernanceActionState {..} =
      GovernanceActionState
        { gasVotes = Map.insert (voteRole, voteRoleKeyHash) vote gasVotes
        , ..
        }

addAction ::
  GovernanceActionId (EraCrypto era) ->
  GovernanceActionInfo era ->
  ConwayTallyState era ->
  ConwayTallyState era
addAction gaid gai (ConwayTallyState st) =
  ConwayTallyState $
    Map.insert gaid (makeGovAction gai) st

noSuchGovernanceAction ::
  ConwayTallyState era ->
  Vote era ->
  Test (ConwayTallyPredFailure era)
noSuchGovernanceAction (ConwayTallyState ts) vote@Vote {..} =
  failureUnless (Map.member voteGovActionId ts) $ NoSuchGovernanceAction vote

tallyTransition :: TransitionRule (ConwayTALLY era)
tallyTransition = do
  -- TODO Check the signatures
  TRC (TallyEnv txid, st, govProcedures) <- judgmentContext

  let updateState st' Empty = pure st'
      updateState !st' ((x, idx) :<| xs) = do
        let gaid = GovernanceActionId txid idx
        st'' <- case x of
          VotingProcedure vote -> do
            runTest $ noSuchGovernanceAction st vote
            pure $ addVote gaid vote st'
          ProposalProcedure action -> pure $ addAction gaid action st'
        updateState st'' xs

  updateState st $ Seq.zip govProcedures [0 ..]

instance Inject (ConwayTallyPredFailure era) (ConwayTallyPredFailure era) where
  inject = id
