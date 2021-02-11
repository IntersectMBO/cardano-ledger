{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Rupd
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
  )
where

import Cardano.Ledger.Era (Crypto, Era)
-- RewardUpdate,

import Cardano.Slotting.EpochInfo.API (epochInfoRange)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)
import Control.Provenance (runProvM)
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
  )
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
  ( ShelleyBase,
    activeSlotCoeff,
    epochInfo,
    maxLovelaceSupply,
    randomnessStabilisationWindow,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    PulsingRewUpdate (..),
    completeStep,
    createRUpd,
    pulseStep,
    startStep,
  )
import Shelley.Spec.Ledger.Slot
  ( Duration (..),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
    (+*),
  )

data RUPD era

data RupdEnv era
  = RupdEnv (BlocksMade (Crypto era)) (EpochState era)

data RupdPredicateFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NoThunks (RupdPredicateFailure era)

{-
instance (Era era) => STS (RUPD era) where
  type State (RUPD era) = StrictMaybe (RewardUpdate (Crypto era))
  type Signal (RUPD era) = SlotNo
  type Environment (RUPD era) = RupdEnv era
  type BaseM (RUPD era) = ShelleyBase
  type PredicateFailure (RUPD era) = RupdPredicateFailure era

  initialRules = [pure SNothing]
  transitionRules = [rupdTransition]

rupdTransition :: Era era => TransitionRule (RUPD era)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, slot, maxLL, asc) <- liftSTS $ do
    ei <- asks epochInfo
    sr <- asks randomnessStabilisationWindow
    e <- epochInfoEpoch ei s
    slotsPerEpoch <- epochInfoSize ei e
    slot <- epochInfoFirst ei e <&> (+* (Duration sr))
    maxLL <- asks maxLovelaceSupply
    asc <- asks activeSlotCoeff
    return (slotsPerEpoch, slot, maxLL, asc)
  if s <= slot
    then pure ru
    else case ru of
      SNothing ->
        SJust
          <$> ( liftSTS $
                  runProvM $
                    createRUpd
                      slotsPerEpoch
                      b
                      es
                      (Coin (fromIntegral maxLL))
                      asc
              )
      SJust _ -> pure ru
-}

instance (Era era) => STS (RUPD era) where
  type State (RUPD era) = PulsingRewUpdate ShelleyBase era
  type Signal (RUPD era) = SlotNo
  type Environment (RUPD era) = RupdEnv era
  type BaseM (RUPD era) = ShelleyBase
  type PredicateFailure (RUPD era) = RupdPredicateFailure era

  initialRules = [pure Waiting]
  transitionRules = [rupdTransition]

rupdTransition :: Era era => TransitionRule (RUPD era)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, slot, maxLL, asc, lastblock) <- liftSTS $ do
    ei <- asks epochInfo
    sr <- asks randomnessStabilisationWindow
    e <- epochInfoEpoch ei s
    slotsPerEpoch <- epochInfoSize ei e
    slot <- epochInfoFirst ei e <&> (+* (Duration sr))
    (_first, lastblock) <- lift (epochInfoRange ei e)
    maxLL <- asks maxLovelaceSupply
    asc <- asks activeSlotCoeff
    return (slotsPerEpoch, slot, maxLL, asc, lastblock)
  let maxsupply = Coin (fromIntegral maxLL)
  case (s <= slot, s == lastblock) of
    -- Waiting for the stabiliy point, do nothing, keep waiting
    (True, _) -> pure Waiting
    (False, _) | ((2 * 2 == 2 + (2 :: Int))) -> liftSTS $ runProvM $ Complete <$> createRUpd slotsPerEpoch b es maxsupply asc
    -- We are in the last block, finish everything up
    (False, True) ->
      case ru of
        Waiting -> liftSTS $ runProvM $ completeStep $ startStep slotsPerEpoch b es maxsupply asc
        p@(Pulsing _ _) -> liftSTS $ runProvM $ completeStep p
        p@(Complete _) -> pure p
    -- More blocks to come, get things started or take a step
    (False, False) ->
      case ru of
        Waiting -> liftSTS $ runProvM $ pure $ startStep slotsPerEpoch b es maxsupply asc
        p@(Pulsing _ _) -> liftSTS $ runProvM $ pulseStep p
        p@(Complete _) -> pure p
