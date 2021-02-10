{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Tick
  ( TICK,
    State,
    TickPredicateFailure (..),
    PredicateFailure,
    adoptGenesisDelegs,
    TICKF,
    TickfPredicateFailure (..),
  )
where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, StrictMaybe, epochInfo)
import Shelley.Spec.Ledger.Keys (GenDelegs (..))
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    FutureGenDeleg (..),
    LedgerState (..),
    NewEpochState (..),
    RewardUpdate,
  )
import Shelley.Spec.Ledger.STS.NewEpoch (NEWEPOCH, NewEpochPredicateFailure)
import Shelley.Spec.Ledger.STS.Rupd (RUPD, RupdEnv (..), RupdPredicateFailure)
import Shelley.Spec.Ledger.Slot (EpochNo, SlotNo, epochInfoEpoch)

-- ==================================================

data TICK era

data TickPredicateFailure era
  = NewEpochFailure (PredicateFailure (Core.EraRule "NEWEPOCH" era)) -- Subtransition Failures
  | RupdFailure (PredicateFailure (Core.EraRule "RUPD" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "NEWEPOCH" era)),
    Show (PredicateFailure (Core.EraRule "RUPD" era))
  ) =>
  Show (TickPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "NEWEPOCH" era)),
    Eq (PredicateFailure (Core.EraRule "RUPD" era))
  ) =>
  Eq (TickPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (Core.EraRule "NEWEPOCH" era)),
    NoThunks (PredicateFailure (Core.EraRule "RUPD" era))
  ) =>
  NoThunks (TickPredicateFailure era)

instance
  ( Era era,
    Embed (Core.EraRule "NEWEPOCH" era) (TICK era),
    Embed (Core.EraRule "RUPD" era) (TICK era),
    State (TICK era) ~ NewEpochState era,
    BaseM (TICK era) ~ ShelleyBase,
    Environment (Core.EraRule "RUPD" era) ~ RupdEnv era,
    State (Core.EraRule "RUPD" era) ~ StrictMaybe (RewardUpdate (Crypto era)),
    Signal (Core.EraRule "RUPD" era) ~ SlotNo,
    Environment (Core.EraRule "NEWEPOCH" era) ~ (),
    State (Core.EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (Core.EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  STS (TICK era)
  where
  type
    State (TICK era) =
      NewEpochState era
  type
    Signal (TICK era) =
      SlotNo
  type Environment (TICK era) = ()
  type BaseM (TICK era) = ShelleyBase
  type PredicateFailure (TICK era) = TickPredicateFailure era

  initialRules = []
  transitionRules = [bheadTransition]

adoptGenesisDelegs ::
  EpochState era ->
  SlotNo ->
  EpochState era
adoptGenesisDelegs es slot = es'
  where
    ls = esLState es
    dp = _delegationState ls
    ds = _dstate dp
    fGenDelegs = _fGenDelegs ds
    GenDelegs genDelegs = _genDelegs ds
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
        { _fGenDelegs = fGenDelegs',
          _genDelegs = GenDelegs $ eval (genDelegs ⨃ genDelegs')
        }
    dp' = dp {_dstate = ds'}
    ls' = ls {_delegationState = dp'}
    es' = es {esLState = ls'}

-- | This is a limited version of 'bheadTransition' which is suitable for the
-- future ledger view.
validatingTickTransition ::
  forall tick era.
  ( Embed (Core.EraRule "NEWEPOCH" era) (tick era),
    STS (tick era),
    State (tick era) ~ NewEpochState era,
    BaseM (tick era) ~ ShelleyBase,
    Environment (Core.EraRule "NEWEPOCH" era) ~ (),
    State (Core.EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (Core.EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  NewEpochState era ->
  SlotNo ->
  TransitionRule (tick era)
validatingTickTransition nes slot = do
  epoch <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoEpoch ei slot

  nes' <- trans @(Core.EraRule "NEWEPOCH" era) $ TRC ((), nes, epoch)
  let es'' = adoptGenesisDelegs (nesEs nes') slot

  pure $ nes' {nesEs = es''}

bheadTransition ::
  forall era.
  ( Embed (Core.EraRule "NEWEPOCH" era) (TICK era),
    Embed (Core.EraRule "RUPD" era) (TICK era),
    STS (TICK era),
    State (TICK era) ~ NewEpochState era,
    BaseM (TICK era) ~ ShelleyBase,
    Environment (Core.EraRule "RUPD" era) ~ RupdEnv era,
    State (Core.EraRule "RUPD" era) ~ StrictMaybe (RewardUpdate (Crypto era)),
    Signal (Core.EraRule "RUPD" era) ~ SlotNo,
    Environment (Core.EraRule "NEWEPOCH" era) ~ (),
    State (Core.EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (Core.EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  TransitionRule (TICK era)
bheadTransition = do
  TRC ((), nes@(NewEpochState _ bprev _ es _ _), slot) <-
    judgmentContext

  nes' <- validatingTickTransition @TICK nes slot

  ru'' <-
    trans @(Core.EraRule "RUPD" era) $
      TRC (RupdEnv bprev es, nesRu nes', slot)

  let nes'' = nes' {nesRu = ru''}
  pure nes''

instance
  ( UsesTxOut era,
    UsesValue era,
    STS (NEWEPOCH era),
    PredicateFailure (Core.EraRule "NEWEPOCH" era) ~ NewEpochPredicateFailure era
  ) =>
  Embed (NEWEPOCH era) (TICK era)
  where
  wrapFailed = NewEpochFailure

instance
  ( Era era,
    STS (RUPD era),
    PredicateFailure (Core.EraRule "RUPD" era) ~ RupdPredicateFailure era
  ) =>
  Embed (RUPD era) (TICK era)
  where
  wrapFailed = RupdFailure

{------------------------------------------------------------------------------
-- TICKF transition

-- This is a variant on the TICK transition called only by the consensus layer
to tick the ledger state to a future slot.
------------------------------------------------------------------------------}

data TICKF era

newtype TickfPredicateFailure era
  = TickfNewEpochFailure (PredicateFailure (Core.EraRule "NEWEPOCH" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Era era,
    Show (PredicateFailure (Core.EraRule "NEWEPOCH" era))
  ) =>
  Show (TickfPredicateFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "NEWEPOCH" era))
  ) =>
  Eq (TickfPredicateFailure era)

instance
  ( UsesTxOut era,
    UsesValue era,
    NoThunks (PredicateFailure (Core.EraRule "NEWEPOCH" era))
  ) =>
  NoThunks (TickfPredicateFailure era)

instance
  ( Era era,
    Embed (Core.EraRule "NEWEPOCH" era) (TICKF era),
    Environment (Core.EraRule "NEWEPOCH" era) ~ (),
    State (Core.EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (Core.EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  STS (TICKF era)
  where
  type
    State (TICKF era) =
      NewEpochState era
  type
    Signal (TICKF era) =
      SlotNo
  type Environment (TICKF era) = ()
  type BaseM (TICKF era) = ShelleyBase
  type PredicateFailure (TICKF era) = TickfPredicateFailure era

  initialRules = []
  transitionRules =
    [ do
        TRC ((), nes, slot) <- judgmentContext
        validatingTickTransition nes slot
    ]

instance
  ( UsesTxOut era,
    UsesValue era,
    STS (NEWEPOCH era),
    PredicateFailure (Core.EraRule "NEWEPOCH" era)
      ~ NewEpochPredicateFailure era
  ) =>
  Embed (NEWEPOCH era) (TICKF era)
  where
  wrapFailed = TickfNewEpochFailure
