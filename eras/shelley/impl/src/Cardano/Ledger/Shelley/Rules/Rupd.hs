{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Rupd (
  ShelleyRUPD,
  RupdEnv (..),
  PredicateFailure,
  ShelleyRupdPredFailure,
  epochInfoRange,
  PulsingRewUpdate (..),
  startStep,
  pulseStep,
  completeStep,
  lift,
  Identity (..),
  RupdEvent (..),
)
where

import Cardano.Ledger.BaseTypes (
  BlocksMade,
  ShelleyBase,
  StrictMaybe (..),
  activeSlotCoeff,
  epochInfoPure,
  maxLovelaceSupply,
  randomnessStabilisationWindow,
  securityParameter,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Shelley.Era (ShelleyRUPD)
import Cardano.Ledger.Shelley.Governance (EraGov)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  PulsingRewUpdate (..),
  completeStep,
  pulseStep,
  startStep,
 )
import Cardano.Ledger.Slot (
  Duration (..),
  EpochNo (..),
  SlotNo,
  epochInfoEpoch,
  epochInfoFirst,
  epochInfoSize,
  (+*),
 )
import Cardano.Slotting.EpochInfo.API (epochInfoRange)
import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Rule,
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  tellEvent,
 )
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data RupdEnv era
  = RupdEnv !(BlocksMade (EraCrypto era)) !(EpochState era)

data ShelleyRupdPredFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyRupdPredFailure era)

instance NFData (ShelleyRupdPredFailure era)

instance
  ( Era era
  , EraGov era
  ) =>
  STS (ShelleyRUPD era)
  where
  type State (ShelleyRUPD era) = StrictMaybe (PulsingRewUpdate (EraCrypto era))
  type Signal (ShelleyRUPD era) = SlotNo
  type Environment (ShelleyRUPD era) = RupdEnv era
  type BaseM (ShelleyRUPD era) = ShelleyBase
  type PredicateFailure (ShelleyRUPD era) = ShelleyRupdPredFailure era
  type Event (ShelleyRUPD era) = RupdEvent (EraCrypto era)

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

data RupdEvent c
  = RupdEvent
      !EpochNo
      !(Map.Map (Credential 'Staking c) (Set (Reward c)))
  deriving (Generic, Eq)

instance NFData (RupdEvent c)

-- | tell a RupdEvent only if the map is non-empty
tellRupd :: String -> RupdEvent (EraCrypto era) -> Rule (ShelleyRUPD era) rtype ()
tellRupd _ (RupdEvent _ m) | Map.null m = pure ()
tellRupd _message x = tellEvent x

-- | The Goldilocks labeling of when to do the reward calculation.
data RewardTiming = RewardsTooEarly | RewardsJustRight | RewardsTooLate

determineRewardTiming :: SlotNo -> SlotNo -> SlotNo -> RewardTiming
determineRewardTiming currentSlot startAfterSlot endSlot
  | currentSlot > endSlot = RewardsTooLate
  | currentSlot <= startAfterSlot = RewardsTooEarly
  | otherwise = RewardsJustRight

rupdTransition :: EraGov era => TransitionRule (ShelleyRUPD era)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, slot, slotForce, maxLL, asc, k, e) <- liftSTS $ do
    ei <- asks epochInfoPure
    sr <- asks randomnessStabilisationWindow
    e <- epochInfoEpoch ei s
    slotsPerEpoch <- epochInfoSize ei e
    slot <- epochInfoFirst ei e <&> (+* Duration sr)
    maxLL <- asks maxLovelaceSupply
    asc <- asks activeSlotCoeff
    k <- asks securityParameter -- Maximum number of blocks we are allowed to roll back
    return (slotsPerEpoch, slot, slot +* Duration sr, maxLL, asc, k, e)
  let maxsupply = Coin (fromIntegral maxLL)
  case determineRewardTiming s slot slotForce of
    -- Waiting for the stability point, do nothing, keep waiting
    RewardsTooEarly -> pure SNothing
    -- More blocks to come, get things started or take a step
    RewardsJustRight ->
      case ru of
        SNothing ->
          -- This is the first opportunity to pulse, so start pulsing.
          -- SJust <$> tellLeaderEvents (e + 1) (fst (startStep slotsPerEpoch b es maxsupply asc k))
          (pure . SJust) (startStep slotsPerEpoch b es maxsupply asc k)
        (SJust p@(Pulsing _ _)) -> do
          -- We began pulsing earlier, so run another pulse
          (ans, event) <- liftSTS $ pulseStep p
          tellRupd "Pulsing Rupd" (RupdEvent (succ e) event)
          pure (SJust ans)
        (SJust p@(Complete _)) -> pure (SJust p)
    -- Time to force the completion of the pulser so that downstream tools such as db-sync
    -- have time to see the reward update before the epoch boundary rollover.
    RewardsTooLate ->
      case ru of
        SNothing -> do
          -- Nothing has been done, so start, and then complete the pulser. We hope this is very rare.
          let pulser = startStep slotsPerEpoch b es maxsupply asc k
          (reward, event) <- liftSTS $ completeStep pulser
          tellRupd "Starting too late" (RupdEvent (succ e) event)
          pure (SJust reward)
        SJust p@(Pulsing _ _) -> do
          -- We have been pulsing, but we ran out of time, so complete the pulser.
          (reward, event) <- liftSTS $ completeStep p
          tellRupd "Completing too late" (RupdEvent (succ e) event)
          pure (SJust reward)
        complete@(SJust (Complete _)) -> pure complete
