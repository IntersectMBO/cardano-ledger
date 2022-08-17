{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Rupd
  ( ShelleyRUPD,
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

import Cardano.Ledger.BaseTypes
  ( BlocksMade,
    NonNegativeInterval,
    ProtVer,
    ShelleyBase,
    StrictMaybe (..),
    UnitInterval,
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
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState,
    PulsingRewUpdate (..),
    completeStep,
    pulseStep,
    startStep,
  )
import Cardano.Ledger.Shelley.Rewards (Reward)
import Cardano.Ledger.Slot
  ( Duration (..),
    EpochNo,
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
    (+*),
  )
import Cardano.Slotting.EpochInfo.API (epochInfoRange)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Rule,
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
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

data ShelleyRUPD era

data RupdEnv era
  = RupdEnv (BlocksMade (Crypto era)) (EpochState era)

data ShelleyRupdPredFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyRupdPredFailure era)

instance
  ( Era era,
    HasField "_a0" (PParams era) NonNegativeInterval,
    HasField "_d" (PParams era) UnitInterval,
    HasField "_nOpt" (PParams era) Natural,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_rho" (PParams era) UnitInterval,
    HasField "_tau" (PParams era) UnitInterval
  ) =>
  STS (ShelleyRUPD era)
  where
  type State (ShelleyRUPD era) = StrictMaybe (PulsingRewUpdate (Crypto era))
  type Signal (ShelleyRUPD era) = SlotNo
  type Environment (ShelleyRUPD era) = RupdEnv era
  type BaseM (ShelleyRUPD era) = ShelleyBase
  type PredicateFailure (ShelleyRUPD era) = ShelleyRupdPredFailure era
  type Event (ShelleyRUPD era) = RupdEvent (Crypto era)

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

data RupdEvent crypto
  = RupdEvent
      !EpochNo
      !(Map.Map (Credential 'Staking crypto) (Set (Reward crypto)))

-- | tell a RupdEvent only if the map is non-empty
tellRupd :: String -> RupdEvent (Crypto era) -> Rule (ShelleyRUPD era) rtype ()
tellRupd _ (RupdEvent _ m) | Map.null m = pure ()
tellRupd _message x = tellEvent x

-- | The Goldilocks labeling of when to do the reward calculation.
data RewardTiming = RewardsTooEarly | RewardsJustRight | RewardsTooLate

determineRewardTiming :: SlotNo -> SlotNo -> SlotNo -> RewardTiming
determineRewardTiming currentSlot startAftterSlot endSlot
  | currentSlot > endSlot = RewardsTooLate
  | currentSlot <= startAftterSlot = RewardsTooEarly
  | otherwise = RewardsJustRight

rupdTransition ::
  ( Era era,
    HasField "_a0" (PParams era) NonNegativeInterval,
    HasField "_d" (PParams era) UnitInterval,
    HasField "_nOpt" (PParams era) Natural,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_rho" (PParams era) UnitInterval,
    HasField "_tau" (PParams era) UnitInterval
  ) =>
  TransitionRule (ShelleyRUPD era)
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
          tellRupd "Pulsing Rupd" (RupdEvent (e + 1) event)
          pure (SJust ans)
        (SJust p@(Complete _)) -> pure (SJust p)
    -- Time to force the completion of the pulser so that downstream tools such as db-sync
    -- have time to see the reward update before the epoch boundary rollover.
    RewardsTooLate ->
      case ru of
        SNothing -> do
          -- Nothing has been done, so start, and then complete the pulser. We hope this is very rare.
          let pulser = startStep slotsPerEpoch b es maxsupply asc k
          (reward, event) <- liftSTS . completeStep $ pulser
          tellRupd "Starting too late" (RupdEvent (e + 1) event)
          pure (SJust reward)
        SJust p@(Pulsing _ _) -> do
          -- We have been pulsing, but we ran out of time, so complete the pulser.
          (reward, event) <- liftSTS . completeStep $ p
          tellRupd "completing too late" (RupdEvent (e + 1) event)
          pure (SJust reward)
        complete@(SJust (Complete _)) -> pure complete
