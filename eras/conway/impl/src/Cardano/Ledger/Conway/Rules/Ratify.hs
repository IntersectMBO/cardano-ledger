{-# LANGUAGE BangPatterns #-}
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
  dRepAccepted,
  dRepAcceptedRatio,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.CertState (DRepState (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway.Era (ConwayENACT, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance (
  EraGov,
  GovAction (..),
  GovActionState (..),
  RatifyState (..),
  Vote (..),
 )
import Cardano.Ledger.Conway.Rules.Enact (EnactPredFailure, EnactState (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
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
import Data.Word (Word64)

data RatifyEnv era = RatifyEnv
  { reStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , reStakePoolDistr :: !(PoolDistr (EraCrypto era))
  , reDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
  , reDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
  , reCurrentEpoch :: !EpochNo
  }
  deriving (Show)

newtype RatifySignal era = RatifySignal (StrictSeq (GovActionState era))

instance
  ( Era era
  , Embed (EraRule "ENACT" era) (ConwayRATIFY era)
  , State (EraRule "ENACT" era) ~ EnactState era
  , Environment (EraRule "ENACT" era) ~ ()
  , Signal (EraRule "ENACT" era) ~ GovAction era
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

dRepThreshold :: Rational
dRepThreshold = 51 % 100

spoAccepted :: RatifyEnv era -> GovActionState era -> Bool
spoAccepted RatifyEnv {reStakePoolDistr = PoolDistr poolDistr} gas =
  totalAcceptedStakePoolsRatio > getStakePoolThreshold gasAction
  where
    GovActionState {gasStakePoolVotes, gasAction} = gas
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

dRepAccepted :: forall era. RatifyEnv era -> GovActionState era -> Rational -> Bool
dRepAccepted ratifyEnv GovActionState {gasDRepVotes, gasAction} threshold =
  dRepAcceptedRatio ratifyEnv gasDRepVotes gasAction >= threshold

-- Compute the dRep ratio yes/(yes + no), where
-- yes: is the total stake of
--    - registered dReps that voted 'yes', plus
--    - the AlwaysNoConfidence dRep, in case the action is NoConfidence
-- no: is the total stake of
--    - registered dReps that voted 'no', plus
--    - registered dReps that did not vote for this action, plus
--    - the AlwaysNoConfidence dRep
-- In other words, the denominator `yes + no` is the total stake of all registered dReps, minus the abstain votes stake
-- (both credential DReps and AlwaysAbstain)
--
-- We iterate over the dRep distribution, and incrementally construct the numerator and denominator.
dRepAcceptedRatio ::
  forall era.
  RatifyEnv era ->
  Map (Credential 'DRepRole (EraCrypto era)) Vote ->
  GovAction era ->
  Rational
dRepAcceptedRatio RatifyEnv {reDRepDistr, reDRepState, reCurrentEpoch} gasDRepVotes gasAction
  | totalExcludingAbstainStake == 0 = 0
  | otherwise = toInteger yesStake % toInteger totalExcludingAbstainStake
  where
    accumStake :: (Word64, Word64) -> DRep (EraCrypto era) -> CompactForm Coin -> (Word64, Word64)
    accumStake (!yes, !tot) drep (CompactCoin stake) =
      case drep of
        DRepCredential cred ->
          case Map.lookup cred reDRepState of
            Nothing -> (yes, tot) -- drep is not registered, so we don't consider it
            Just (DRepState expiry _ _)
              | reCurrentEpoch > expiry -> (yes, tot) -- drep is expired, so we don't consider it
              | otherwise ->
                  case Map.lookup cred gasDRepVotes of
                    Nothing -> (yes, tot + stake) -- drep hasn't voted for this action, so we don't count
                    -- the vote but we consider it in the denominator
                    Just VoteYes -> (yes + stake, tot + stake)
                    Just Abstain -> (yes, tot)
                    Just VoteNo -> (yes, tot + stake)
        DRepAlwaysNoConfidence ->
          case gasAction of
            NoConfidence _ -> (yes + stake, tot + stake)
            _ -> (yes, tot + stake)
        DRepAlwaysAbstain -> (yes, tot)

    (yesStake, totalExcludingAbstainStake) = Map.foldlWithKey' accumStake (0, 0) reDRepDistr

ratifyTransition ::
  forall era.
  ( Embed (EraRule "ENACT" era) (ConwayRATIFY era)
  , State (EraRule "ENACT" era) ~ EnactState era
  , Environment (EraRule "ENACT" era) ~ ()
  , Signal (EraRule "ENACT" era) ~ GovAction era
  , Era era
  ) =>
  TransitionRule (ConwayRATIFY era)
ratifyTransition = do
  TRC
    ( env@RatifyEnv {reCurrentEpoch}
      , st@RatifyState {rsEnactState, rsRemoved}
      , RatifySignal rsig
      ) <-
    judgmentContext

  case rsig of
    ast :<| sigs -> do
      let GovActionState {gasAction, gasExpiresAfter} = ast
      if spoAccepted env ast && dRepAccepted env ast dRepThreshold
        then do
          -- Update ENACT state with the governance action that was ratified
          es <- trans @(EraRule "ENACT" era) $ TRC ((), rsEnactState, gasAction)
          let st' =
                st
                  { rsEnactState = es
                  , rsRemoved = ast :<| rsRemoved
                  }
          trans @(ConwayRATIFY era) $ TRC (env, st', RatifySignal sigs)
        else do
          -- This action hasn't been ratified yet. Process the remaining actions.
          st' <- trans @(ConwayRATIFY era) $ TRC (env, st, RatifySignal sigs)
          -- Finally, filter out actions that are not processed.
          if gasExpiresAfter < reCurrentEpoch
            then pure st' {rsRemoved = ast :<| rsRemoved} -- Action expires after current Epoch. Remove it.
            else pure st'
    Empty -> pure st

instance EraGov era => Embed (ConwayENACT era) (ConwayRATIFY era) where
  wrapFailed = id
  wrapEvent = absurd
