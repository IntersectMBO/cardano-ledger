{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Tick
  ( TICK,
    State,
    TickPredicateFailure (..),
    TickEvent (..),
    PredicateFailure,
    adoptGenesisDelegs,
    TICKF,
    TickfPredicateFailure,
    TickfEvent,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..), epochInfoPure, quorum)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.Shelley.Constraints (UsesPParams, UsesTxOut, UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary (SnapShots (_pstakeMark, _pstakeMarkPoolDistr))
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    FutureGenDeleg (..),
    LedgerState (..),
    NewEpochState (..),
    PPUPState (..),
    PulsingRewUpdate,
    UTxOState (..),
    proposals,
  )
import Cardano.Ledger.Shelley.Rules.NewEpoch (NEWEPOCH, NewEpochEvent, NewEpochPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Rupd (RUPD, RupdEnv (..), RupdEvent, RupdPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Upec (votedValue)
import Cardano.Ledger.Slot (EpochNo (unEpochNo), SlotNo, epochInfoEpoch)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

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

data TickEvent era
  = NewEpochEvent (Event (Core.EraRule "NEWEPOCH" era))
  | RupdEvent (Event (Core.EraRule "RUPD" era))
  deriving (Generic)

instance
  ( Era era,
    Embed (Core.EraRule "NEWEPOCH" era) (TICK era),
    Embed (Core.EraRule "RUPD" era) (TICK era),
    State (TICK era) ~ NewEpochState era,
    BaseM (TICK era) ~ ShelleyBase,
    Environment (Core.EraRule "RUPD" era) ~ RupdEnv era,
    State (Core.EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (Crypto era)),
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
  type Event (TICK era) = TickEvent era

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
    dp' = dp {dpsDState = ds'}
    ls' = ls {lsDPState = dp'}
    es' = es {esLState = ls'}

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
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot

  nes' <- trans @(Core.EraRule "NEWEPOCH" era) $ TRC ((), nes, epoch)
  let es'' = adoptGenesisDelegs (nesEs nes') slot

  pure $ nes' {nesEs = es''}

-- | This is a limited version of 'validatingTickTransition' which is only suitable
-- for the future ledger view.
validatingTickTransitionFORECAST ::
  forall tick era.
  ( State (tick era) ~ NewEpochState era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    UsesPParams era,

    BaseM (tick era) ~ ShelleyBase,
    STS (tick era)
  ) =>
  NewEpochState era ->
  SlotNo ->
  TransitionRule (tick era)
validatingTickTransitionFORECAST nes slot = do
  -- This whole function is a specialization of an inlined 'NEWEPOCH'.
  --
  -- The ledger view is built entirely from the 'nesPd' and 'esPP' and
  -- '_genDelegs', so those are the only pieces we bother to update here.

  epoch <- liftSTS $ do
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot

  let es = nesEs nes
      EpochState
        { esSnapshots = ss,
          esLState = ls,
          esPp = pp
        } = es

  -- the relevant 'NEWEPOCH' logic
  let pd' = _pstakeMarkPoolDistr ss
  if unEpochNo epoch /= unEpochNo (nesEL nes) + 1 then pure nes else do

    -- We can skip 'SNAP'; we already have the equivalent pd'.

    -- We can skip 'MIR', 'POOLREAP', 'UPEC' (which calls 'NEWPP'); we don't
    -- need to do the checks: if the checks would fail, then the node will fail
    -- in the 'TICK' rule if it ever tries to validate blocks for which the
    -- return value here was used to validate their headers.

    coreNodeQuorum <- liftSTS $ asks quorum

    let pp' =
            maybe pp id
          $ votedValue
              (proposals $ _ppups $ lsUTxOState ls)
              pp
              (fromIntegral coreNodeQuorum)

    pure $! nes {
        nesPd = pd'
      , nesEs = adoptGenesisDelegs
                  (es { esPrevPp = pp, esPp = pp' })
                  slot
      }

bheadTransition ::
  forall era.
  ( Embed (Core.EraRule "NEWEPOCH" era) (TICK era),
    Embed (Core.EraRule "RUPD" era) (TICK era),
    STS (TICK era),
    State (TICK era) ~ NewEpochState era,
    BaseM (TICK era) ~ ShelleyBase,
    Environment (Core.EraRule "RUPD" era) ~ RupdEnv era,
    State (Core.EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (Crypto era)),
    Signal (Core.EraRule "RUPD" era) ~ SlotNo,
    Environment (Core.EraRule "NEWEPOCH" era) ~ (),
    State (Core.EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (Core.EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  TransitionRule (TICK era)
bheadTransition = do
  TRC ((), nes@(NewEpochState _ bprev _ es _ _ _), slot) <-
    judgmentContext

  nes' <- validatingTickTransition @TICK nes slot

  -- Here we force the evaluation of the mark snapshot.
  -- We do NOT force it in the TICKF and TICKN rule
  -- so that it can remain a thunk when the consensus
  -- layer computes the ledger view across the epoch boundary.
  let !_ = _pstakeMark . esSnapshots . nesEs $ nes'
      !_ = _pstakeMarkPoolDistr . esSnapshots . nesEs $ nes'

  ru'' <-
    trans @(Core.EraRule "RUPD" era) $
      TRC (RupdEnv bprev es, nesRu nes', slot)

  let nes'' = nes' {nesRu = ru''}
  pure nes''

instance
  ( UsesTxOut era,
    UsesValue era,
    STS (NEWEPOCH era),
    PredicateFailure (Core.EraRule "NEWEPOCH" era) ~ NewEpochPredicateFailure era,
    Event (Core.EraRule "NEWEPOCH" era) ~ NewEpochEvent era
  ) =>
  Embed (NEWEPOCH era) (TICK era)
  where
  wrapFailed = NewEpochFailure
  wrapEvent = NewEpochEvent

instance
  ( Era era,
    STS (RUPD era),
    PredicateFailure (Core.EraRule "RUPD" era) ~ RupdPredicateFailure era,
    Event (Core.EraRule "RUPD" era) ~ RupdEvent (Crypto era)
  ) =>
  Embed (RUPD era) (TICK era)
  where
  wrapFailed = RupdFailure
  wrapEvent = RupdEvent

{------------------------------------------------------------------------------
-- TICKF transition

-- This is a variant on the TICK transition called only by the consensus layer
to tick the ledger state to a future slot.
------------------------------------------------------------------------------}

data TICKF era

-- | TICKF cannot fail
data TickfPredicateFailure era
  deriving (Eq, Generic, NoThunks, Show)

-- | TICKF has no observable behavior
data TickfEvent era

instance
  ( State (Core.EraRule "PPUP" era) ~ PPUPState era,
    UsesPParams era,

    Era era
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
  type Event (TICKF era) = TickfEvent era

  initialRules = []
  transitionRules =
    [ do
        TRC ((), nes, slot) <- judgmentContext
        validatingTickTransitionFORECAST nes slot
    ]
