{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Tick
  ( ShelleyTICK,
    State,
    ShelleyTickPredFailure (..),
    ShelleyTickEvent (..),
    PredicateFailure,
    adoptGenesisDelegs,
    ShelleyTICKF,
    ShelleyTickfPredFailure (..),
    validatingTickTransition,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..), epochInfoPure)
import Cardano.Ledger.Core
import Cardano.Ledger.EpochBoundary (SnapShots (_pstakeMark))
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.Shelley.Era (ShelleyTICK, ShelleyTICKF)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    FutureGenDeleg (..),
    LedgerState (..),
    NewEpochState (..),
    PulsingRewUpdate,
  )
import Cardano.Ledger.Shelley.Rules.NewEpoch
  ( ShelleyNEWEPOCH,
    ShelleyNewEpochEvent,
    ShelleyNewEpochPredFailure,
  )
import Cardano.Ledger.Shelley.Rules.Rupd
  ( RupdEnv (..),
    RupdEvent,
    ShelleyRUPD,
    ShelleyRupdPredFailure,
  )
import Cardano.Ledger.Slot (EpochNo, SlotNo, epochInfoEpoch)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- ==================================================

data ShelleyTickPredFailure era
  = NewEpochFailure (PredicateFailure (EraRule "NEWEPOCH" era)) -- Subtransition Failures
  | RupdFailure (PredicateFailure (EraRule "RUPD" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (EraRule "NEWEPOCH" era)),
    Show (PredicateFailure (EraRule "RUPD" era))
  ) =>
  Show (ShelleyTickPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "NEWEPOCH" era)),
    Eq (PredicateFailure (EraRule "RUPD" era))
  ) =>
  Eq (ShelleyTickPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "NEWEPOCH" era)),
    NoThunks (PredicateFailure (EraRule "RUPD" era))
  ) =>
  NoThunks (ShelleyTickPredFailure era)

data ShelleyTickEvent era
  = TickNewEpochEvent (Event (EraRule "NEWEPOCH" era))
  | TickRupdEvent (Event (EraRule "RUPD" era))
  deriving (Generic)

instance
  ( Era era,
    Embed (EraRule "NEWEPOCH" era) (ShelleyTICK era),
    Embed (EraRule "RUPD" era) (ShelleyTICK era),
    State (ShelleyTICK era) ~ NewEpochState era,
    BaseM (ShelleyTICK era) ~ ShelleyBase,
    Environment (EraRule "RUPD" era) ~ RupdEnv era,
    State (EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (EraCrypto era)),
    Signal (EraRule "RUPD" era) ~ SlotNo,
    Environment (EraRule "NEWEPOCH" era) ~ (),
    State (EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  STS (ShelleyTICK era)
  where
  type State (ShelleyTICK era) = NewEpochState era
  type Signal (ShelleyTICK era) = SlotNo
  type Environment (ShelleyTICK era) = ()
  type BaseM (ShelleyTICK era) = ShelleyBase
  type PredicateFailure (ShelleyTICK era) = ShelleyTickPredFailure era
  type Event (ShelleyTICK era) = ShelleyTickEvent era

  initialRules = []
  transitionRules = [bheadTransition]

adoptGenesisDelegs ::
  EpochState era ->
  SlotNo ->
  EpochState era
adoptGenesisDelegs es slot = es'
  where
    ls = esLState es
    dp = lsDPState ls
    ds = dpsDState dp
    fGenDelegs = dsFutureGenDelegs ds
    GenDelegs genDelegs = dsGenDelegs ds
    (curr, fGenDelegs') = Map.partitionWithKey (\(FutureGenDeleg s _) _ -> s <= slot) fGenDelegs
    latestPerGKey (FutureGenDeleg s genKeyHash) delegate latest =
      case Map.lookup genKeyHash latest of
        Nothing -> Map.insert genKeyHash (s, delegate) latest
        Just (t, _) ->
          if s > t
            then Map.insert genKeyHash (s, delegate) latest
            else latest
    genDelegs' = Map.map snd $ Map.foldrWithKey latestPerGKey Map.empty curr
    ds' =
      ds
        { dsFutureGenDelegs = fGenDelegs',
          dsGenDelegs = GenDelegs $ eval (genDelegs ⨃ genDelegs')
        }
    dp' = dp {dpsDState = ds'}
    ls' = ls {lsDPState = dp'}
    es' = es {esLState = ls'}

-- | This is a limited version of 'bheadTransition' which is suitable for the
-- future ledger view.
validatingTickTransition ::
  forall tick era.
  ( Embed (EraRule "NEWEPOCH" era) (tick era),
    STS (tick era),
    State (tick era) ~ NewEpochState era,
    BaseM (tick era) ~ ShelleyBase,
    Environment (EraRule "NEWEPOCH" era) ~ (),
    State (EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  NewEpochState era ->
  SlotNo ->
  TransitionRule (tick era)
validatingTickTransition nes slot = do
  epoch <- liftSTS $ do
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot

  nes' <- trans @(EraRule "NEWEPOCH" era) $ TRC ((), nes, epoch)
  let es'' = adoptGenesisDelegs (nesEs nes') slot

  pure $ nes' {nesEs = es''}

bheadTransition ::
  forall era.
  ( Embed (EraRule "NEWEPOCH" era) (ShelleyTICK era),
    Embed (EraRule "RUPD" era) (ShelleyTICK era),
    STS (ShelleyTICK era),
    State (ShelleyTICK era) ~ NewEpochState era,
    BaseM (ShelleyTICK era) ~ ShelleyBase,
    Environment (EraRule "RUPD" era) ~ RupdEnv era,
    State (EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (EraCrypto era)),
    Signal (EraRule "RUPD" era) ~ SlotNo,
    Environment (EraRule "NEWEPOCH" era) ~ (),
    State (EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  TransitionRule (ShelleyTICK era)
bheadTransition = do
  TRC ((), nes@(NewEpochState _ bprev _ es _ _ _), slot) <-
    judgmentContext

  nes' <- validatingTickTransition @ShelleyTICK nes slot

  -- Here we force the evaluation of the mark snapshot.
  -- We do NOT force it in the TICKF and TICKN rule
  -- so that it can remain a thunk when the consensus
  -- layer computes the ledger view across the epoch boundary.
  let !_ = _pstakeMark . esSnapshots . nesEs $ nes'

  ru'' <-
    trans @(EraRule "RUPD" era) $
      TRC (RupdEnv bprev es, nesRu nes', slot)

  let nes'' = nes' {nesRu = ru''}
  pure nes''

instance
  ( STS (ShelleyNEWEPOCH era),
    PredicateFailure (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochPredFailure era,
    Event (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  ) =>
  Embed (ShelleyNEWEPOCH era) (ShelleyTICK era)
  where
  wrapFailed = NewEpochFailure
  wrapEvent = TickNewEpochEvent

instance
  ( Era era,
    STS (ShelleyRUPD era),
    PredicateFailure (EraRule "RUPD" era) ~ ShelleyRupdPredFailure era,
    Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
  ) =>
  Embed (ShelleyRUPD era) (ShelleyTICK era)
  where
  wrapFailed = RupdFailure
  wrapEvent = TickRupdEvent

{------------------------------------------------------------------------------
-- TICKF transition

-- This is a variant on the TICK transition called only by the consensus layer
to tick the ledger state to a future slot.
------------------------------------------------------------------------------}

newtype ShelleyTickfPredFailure era
  = TickfNewEpochFailure (PredicateFailure (EraRule "NEWEPOCH" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Era era,
    Show (PredicateFailure (EraRule "NEWEPOCH" era))
  ) =>
  Show (ShelleyTickfPredFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (EraRule "NEWEPOCH" era))
  ) =>
  Eq (ShelleyTickfPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "NEWEPOCH" era))
  ) =>
  NoThunks (ShelleyTickfPredFailure era)

newtype ShelleyTickfEvent era
  = TickfNewEpochEvent (Event (EraRule "NEWEPOCH" era)) -- Subtransition Events

instance
  ( Era era,
    Embed (EraRule "NEWEPOCH" era) (ShelleyTICKF era),
    Environment (EraRule "NEWEPOCH" era) ~ (),
    State (EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  STS (ShelleyTICKF era)
  where
  type
    State (ShelleyTICKF era) =
      NewEpochState era
  type
    Signal (ShelleyTICKF era) =
      SlotNo
  type Environment (ShelleyTICKF era) = ()
  type BaseM (ShelleyTICKF era) = ShelleyBase
  type PredicateFailure (ShelleyTICKF era) = ShelleyTickfPredFailure era
  type Event (ShelleyTICKF era) = ShelleyTickfEvent era

  initialRules = []
  transitionRules =
    [ do
        TRC ((), nes, slot) <- judgmentContext
        validatingTickTransition nes slot
    ]

instance
  ( STS (ShelleyNEWEPOCH era),
    PredicateFailure (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochPredFailure era,
    Event (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  ) =>
  Embed (ShelleyNEWEPOCH era) (ShelleyTICKF era)
  where
  wrapFailed = TickfNewEpochFailure
  wrapEvent = TickfNewEpochEvent
