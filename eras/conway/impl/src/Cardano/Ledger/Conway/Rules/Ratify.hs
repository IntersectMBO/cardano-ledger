{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  Vote (..),
 )
import Cardano.Ledger.Conway.Rules.Enact (EnactPredFailure, EnactState (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..), individualPoolStake)
import Cardano.Ledger.Slot (EpochNo (..))
import Control.Monad (guard)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import Data.Void (absurd)

data RatifyEnv era = RatifyEnv
  { reStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , reStakePoolDistr :: !(PoolDistr (EraCrypto era))
  , reCurrentEpoch :: !EpochNo
  }

newtype RatifySignal era
  = RatifySignal
      ( StrictSeq
          ( GovernanceActionId (EraCrypto era)
          , GovernanceActionState era
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
  type PredicateFailure (ConwayRATIFY era) = EnactPredFailure era
  type Signal (ConwayRATIFY era) = RatifySignal era
  type State (ConwayRATIFY era) = RatifyState era
  type BaseM (ConwayRATIFY era) = ShelleyBase

  initialRules = []
  transitionRules = [ratifyTransition]

--- Constants

-- ccThreshold :: Int
-- ccThreshold = 0

-- Temporary threshold of 1 lovelace
spoThreshold :: Rational
spoThreshold = 1 % 45000000000000000

epochsToExpire :: EpochNo
epochsToExpire = 10

accepted :: RatifyEnv era -> GovernanceActionState era -> Bool
accepted RatifyEnv {reStakePoolDistr = PoolDistr poolDistr} gas =
  totalAcceptedStakePoolsRatio > spoThreshold
  where
    GovernanceActionState {gasStakePoolVotes} = gas
    totalAcceptedStakePoolsRatio =
      getSum $ Map.foldMapWithKey lookupStakeDistrForYesVotes gasStakePoolVotes

    lookupStakeDistrForYesVotes poolId vote = fromMaybe mempty $ do
      guard (vote == VoteYes)
      Sum . individualPoolStake <$> Map.lookup poolId poolDistr

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
    ( env@RatifyEnv {reCurrentEpoch}
      , st@RatifyState {rsEnactState, rsFuture, rsRemoved}
      , RatifySignal rsig
      ) <-
    judgmentContext

  case rsig of
    act@(_, ast@GovernanceActionState {gasAction, gasProposedIn}) :<| sigs -> do
      let expired = gasProposedIn + epochsToExpire < reCurrentEpoch
      if accepted env ast
        then do
          -- Update ENACT state with the governance action that was ratified
          es <- trans @(EraRule "ENACT" era) $ TRC ((), rsEnactState, gasAction)
          let st' =
                st
                  { rsEnactState = es
                  , rsRemoved = act :<| rsRemoved
                  }
          trans @(ConwayRATIFY era) $ TRC (env, st', RatifySignal sigs)
        else do
          st' <- trans @(ConwayRATIFY era) $ TRC (env, st, RatifySignal sigs)
          if expired
            then -- Action expired, do not include it in the next epoch
              pure $ st' {rsRemoved = act :<| rsRemoved}
            else -- Include this action in the next epoch
              pure $ st' {rsFuture = act :<| rsFuture}
    Empty -> pure st

instance EraGovernance era => Embed (ConwayENACT era) (ConwayRATIFY era) where
  wrapFailed = id
  wrapEvent = absurd
