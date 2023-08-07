{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
  GovernanceAction (..),
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
spoThreshold = 51 % 100

epochsToExpire :: EpochNo
epochsToExpire = 30

accepted :: RatifyEnv era -> GovernanceActionState era -> Bool
accepted RatifyEnv {reStakePoolDistr = PoolDistr poolDistr} gas =
  totalAcceptedStakePoolsRatio > getStakePoolThreshold gasAction
  where
    GovernanceActionState {gasStakePoolVotes, gasAction} = gas
    -- Final ratio for `totalAcceptedStakePoolsRatio` we want is: t = y / (s - a)
    -- Where:
    --  * `y` - total delegated stake that voted Yes
    --  * `a` - total delegated stake that voted Abstain
    --  * `s` - total delegated stake
    --
    -- However, computing the total stake again would be wasteful, since we already have
    -- distributions per pool computed. So, values that we have available are not exactly
    -- what we need, because we have:
    --  * `y/s` - stake that votes yes over the total stake
    --  * `a/s` - stake that voted abstain over the total stake
    --
    -- We divide both numerator and denominator by `s` and we'll get a formula that we can
    -- use:
    --
    -- t = y / (s - a) = (y / s) / (1 - a / s)
    totalAcceptedStakePoolsRatio
      | abstainVotesRatio == 1 = 0 -- guard against the degenerate case when all abstain.
      | otherwise = yesVotesRatio / (1 - abstainVotesRatio)
      where
        (Sum yesVotesRatio, Sum abstainVotesRatio) =
          Map.foldMapWithKey lookupStakePoolDistrForVotes gasStakePoolVotes

        lookupStakePoolDistrForVotes poolId vote = fromMaybe (mempty, mempty) $ do
          distr <- Sum . individualPoolStake <$> Map.lookup poolId poolDistr
          case vote of
            VoteNo -> Nothing
            VoteYes -> Just (distr, mempty)
            Abstain -> Just (mempty, distr)
    getStakePoolThreshold = \case
      -- Disable HardForks for now, in order to prevent SanchoNet from dying
      HardForkInitiation {} -> 101 % 100
      _ -> spoThreshold

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
