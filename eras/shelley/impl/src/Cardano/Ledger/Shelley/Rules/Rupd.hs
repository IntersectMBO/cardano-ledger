{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Rupd
  ( RUPD,
    RupdEnv (..),
    PredicateFailure,
    RupdPredicateFailure,
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
    epochInfo,
    maxLovelaceSupply,
    randomnessStabilisationWindow,
    securityParameter,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (Crypto, Era)
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
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

data RUPD era

data RupdEnv era
  = RupdEnv (BlocksMade (Crypto era)) (EpochState era)

data RupdPredicateFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NoThunks (RupdPredicateFailure era)

instance
  ( Era era,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval
  ) =>
  STS (RUPD era)
  where
  type State (RUPD era) = StrictMaybe (PulsingRewUpdate (Crypto era))
  type Signal (RUPD era) = SlotNo
  type Environment (RUPD era) = RupdEnv era
  type BaseM (RUPD era) = ShelleyBase
  type PredicateFailure (RUPD era) = RupdPredicateFailure era
  type Event (RUPD era) = RupdEvent (Crypto era)

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

data RupdEvent crypto = RupdEvent !EpochNo !(Map.Map (Credential 'Staking crypto) (Set (Reward crypto)))

-- | tell a RupdEvent only if the map is non-empty
tellRupd :: String -> RupdEvent (Crypto era) -> Rule (RUPD era) rtype ()
tellRupd _ (RupdEvent _ m) | Map.null m = pure ()
tellRupd _message x = tellEvent x

-- | The Goldilocks labeling of when to do the reward calculation.
data RewardTiming = RewardsTooEarly | RewardsJustRight | RewardsTooLate

determineRewardTiming :: SlotNo -> SlotNo -> SlotNo -> RewardTiming
determineRewardTiming currentSlot startAftterSlot endSlot =
  if currentSlot > endSlot
    then RewardsTooLate
    else
      if currentSlot <= startAftterSlot
        then RewardsTooEarly
        else RewardsJustRight

rupdTransition ::
  ( Era era,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval
  ) =>
  TransitionRule (RUPD era)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, slot, slotForce, maxLL, asc, k, e) <- liftSTS $ do
    ei <- asks epochInfo
    sr <- asks randomnessStabilisationWindow
    e <- epochInfoEpoch ei s
    slotsPerEpoch <- epochInfoSize ei e
    slot <- epochInfoFirst ei e <&> (+* Duration sr)
    maxLL <- asks maxLovelaceSupply
    asc <- asks activeSlotCoeff
    k <- asks securityParameter -- Maximum number of blocks we are allowed to roll back
    return (slotsPerEpoch, slot, (slot +* Duration sr), maxLL, asc, k, e)
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
          (pure . SJust . fst) (startStep slotsPerEpoch b es maxsupply asc k)
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
          let pulser = fst (startStep slotsPerEpoch b es maxsupply asc k)
          (reward, event) <- liftSTS . completeStep $ pulser
          tellRupd "Starting too late" (RupdEvent (e + 1) event)
          pure (SJust reward)
        SJust p@(Pulsing _ _) -> do
          -- We have been pulsing, but we ran out of time, so complete the pulser.
          (reward, event) <- liftSTS . completeStep $ p
          tellRupd "completing too late" (RupdEvent (e + 1) event)
          pure (SJust reward)
        complete@(SJust (Complete _)) -> pure complete
