{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ratify (
  RatifyState (..),
  RatifyEnv (..),
  RatifySignal (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayENACT, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  EraGovernance,
  GovernanceAction,
  GovernanceActionId,
  GovernanceActionState (..),
  RatifyState (..),
  VoteDecision (..),
  VoterRole (..),
 )
import Cardano.Ledger.Conway.Rules.Enact (EnactState (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (HasKeyRole (..), KeyRole (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.State.Transition.Extended (Embed (..), STS (..), TRC (..), TransitionRule, judgmentContext, trans)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import Data.Void (Void, absurd)

data RatifyEnv era = RatifyEnv
  { reStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , reCurrentEpoch :: !EpochNo
  , reRoles :: !(Map (Credential 'Voting (EraCrypto era)) VoterRole)
  }

newtype RatifySignal era
  = RatifySignal
      ( StrictSeq
          ( (GovernanceActionId (EraCrypto era))
          , (GovernanceActionState era)
          )
      )

instance
  ( Era era
  , Embed (EraRule "ENACT" era) (ConwayRATIFY era)
  , State (EraRule "ENACT" era) ~ EnactState era
  , Environment (EraRule "ENACT" era) ~ ()
  , Signal (EraRule "ENACT" era) ~ GovernanceAction era
  ) =>
  STS (ConwayRATIFY era)
  where
  type Environment (ConwayRATIFY era) = RatifyEnv era
  type PredicateFailure (ConwayRATIFY era) = Void
  type Signal (ConwayRATIFY era) = RatifySignal era
  type State (ConwayRATIFY era) = RatifyState era
  type BaseM (ConwayRATIFY era) = ShelleyBase

  initialRules = []
  transitionRules = [ratifyTransition]

--- Constants

ccThreshold :: Int
ccThreshold = 3

spoThreshold :: Rational
spoThreshold = 1 % 2

epochsToExpire :: EpochNo
epochsToExpire = 10

accepted :: RatifyEnv era -> GovernanceActionState era -> Bool
accepted RatifyEnv {..} GovernanceActionState {..} =
  length (votedYesHashes ConstitutionalCommittee) > ccThreshold
    && acceptedBySPOs
  where
    votedYesHashes role = Set.map snd . Map.keysSet $ Map.filterWithKey fil gasVotes
      where
        fil (role', _) Yes | role == role' = True
        fil _ _ = False
    allSPOKeyHashes = Map.keysSet $ Map.filter isSPO reRoles
    isSPO SPO = True
    isSPO _ = False
    spoStakeMap = Map.restrictKeys (Map.mapKeys coerceKeyRole reStakeDistr) allSPOKeyHashes
    Coin totalStakeHeldBySPOs = mconcat $ Map.elems spoStakeMap
    Coin acceptedStake = mconcat . Map.elems . Map.restrictKeys spoStakeMap $ votedYesHashes SPO
    acceptedBySPOs =
      totalStakeHeldBySPOs > 0 && acceptedStake % totalStakeHeldBySPOs > spoThreshold

ratifyTransition ::
  forall era.
  ( Embed (EraRule "ENACT" era) (ConwayRATIFY era)
  , State (EraRule "ENACT" era) ~ EnactState era
  , Environment (EraRule "ENACT" era) ~ ()
  , Signal (EraRule "ENACT" era) ~ GovernanceAction era
  , Era era
  ) =>
  TransitionRule (ConwayRATIFY era)
ratifyTransition = do
  TRC
    ( env@RatifyEnv {..}
      , st@RatifyState {..}
      , RatifySignal rsig
      ) <-
    judgmentContext

  case rsig of
    act@(_, ast@GovernanceActionState {..}) :<| sigs -> do
      let expired = gasProposedIn + epochsToExpire < reCurrentEpoch
      if accepted env ast
        then do
          -- Update ENACT state with the governance action that was ratified
          es <- trans @(EraRule "ENACT" era) $ TRC ((), rsES, gasAction)
          let st' = st {rsES = es}
          trans @(ConwayRATIFY era) $ TRC (env, st', RatifySignal sigs)
        else do
          st' <- trans @(ConwayRATIFY era) $ TRC (env, st, RatifySignal sigs)
          if expired
            then -- Action expired, do not include it in the next epoch
              pure st'
            else -- Include this action in the next epoch
              pure $ st' {rsFuture = act :<| rsFuture}
    Empty -> pure st

instance EraGovernance era => Embed (ConwayENACT era) (ConwayRATIFY era) where
  wrapFailed = absurd
  wrapEvent = absurd
