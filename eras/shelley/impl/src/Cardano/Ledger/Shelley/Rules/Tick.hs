{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Tick (
  TICK,
  State,
  ShelleyTickEvent (..),
  adoptGenesisDelegs,
  TICKF,
  validatingTickTransition,
  validatingTickTransitionFORECAST,
  solidifyNextEpochPParams,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra, TICK, TICKF)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  DState (..),
  EpochState (..),
  FutureGenDeleg (..),
  LedgerState (..),
  NewEpochState (..),
  PulsingRewUpdate,
  UTxOState (..),
  curPParamsEpochStateL,
  lsCertStateL,
  newEpochStateGovStateL,
 )
import Cardano.Ledger.Shelley.Rules.NewEpoch (NEWEPOCH, ShelleyNewEpochEvent)
import Cardano.Ledger.Shelley.Rules.Rupd (
  RUPD,
  RupdEnv (..),
  RupdEvent,
 )
import Cardano.Ledger.Shelley.Rules.Upec (UPEC, UpecState (..))
import Cardano.Ledger.Slot (EpochNo, SlotNo, getTheSlotOfNoReturn)
import Cardano.Ledger.State (EraCertState (..), SnapShots (ssStakeMark, ssStakeMarkPoolDistr))
import Control.DeepSeq (NFData)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (^.))

-- ==================================================

data ShelleyTickEvent era
  = TickNewEpochEvent (Event (EraRule "NEWEPOCH" era))
  | TickRupdEvent (Event (EraRule "RUPD" era))
  deriving (Generic)

type instance EraRuleEvent "TICK" ShelleyEra = ShelleyTickEvent ShelleyEra

deriving instance
  ( Eq (Event (EraRule "NEWEPOCH" era))
  , Eq (Event (EraRule "RUPD" era))
  ) =>
  Eq (ShelleyTickEvent era)

instance
  ( NFData (Event (EraRule "NEWEPOCH" era))
  , NFData (Event (EraRule "RUPD" era))
  ) =>
  NFData (ShelleyTickEvent era)

instance
  ( EraGov era
  , EraCertState era
  , Embed (EraRule "NEWEPOCH" era) (TICK era)
  , Embed (EraRule "RUPD" era) (TICK era)
  , State (TICK era) ~ NewEpochState era
  , BaseM (TICK era) ~ ShelleyBase
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , State (EraRule "RUPD" era) ~ StrictMaybe PulsingRewUpdate
  , Signal (EraRule "RUPD" era) ~ SlotNo
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  STS (TICK era)
  where
  type State (TICK era) = NewEpochState era
  type Signal (TICK era) = SlotNo
  type Environment (TICK era) = ()
  type BaseM (TICK era) = ShelleyBase
  type PredicateFailure (TICK era) = Void
  type Event (TICK era) = ShelleyTickEvent era

  initialRules = []
  transitionRules = [bheadTransition]

adoptGenesisDelegs ::
  EraCertState era =>
  EpochState era ->
  SlotNo ->
  EpochState era
adoptGenesisDelegs es slot = es'
  where
    ls = esLState es
    dp = lsCertState ls
    ds = dp ^. certDStateL
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
        { dsFutureGenDelegs = fGenDelegs'
        , dsGenDelegs = GenDelegs $ Map.union genDelegs' genDelegs
        }
    dp' = dp & certDStateL .~ ds'
    ls' = ls & lsCertStateL .~ dp'
    es' = es {esLState = ls'}

-- | This action ensures that once the current slot number is at the point of no return we
-- mark the future PParams to be updated at the next epoch boundary. Also returns the
-- current epoch number for convenience.
solidifyNextEpochPParams ::
  EraGov era =>
  NewEpochState era ->
  SlotNo ->
  ShelleyBase (EpochNo, NewEpochState era)
solidifyNextEpochPParams nes slot = do
  (curEpochNo, slotOfNoReturn, _) <- getTheSlotOfNoReturn slot
  pure
    ( curEpochNo
    , if slot < slotOfNoReturn
        then nes
        else nes & newEpochStateGovStateL . futurePParamsGovStateL %~ solidifyFuturePParams
    )

-- | This is a limited version of 'bheadTransition' which is suitable for the
-- future ledger view.
validatingTickTransition ::
  forall tick era.
  ( EraGov era
  , EraCertState era
  , Embed (EraRule "NEWEPOCH" era) (tick era)
  , STS (tick era)
  , State (tick era) ~ NewEpochState era
  , BaseM (tick era) ~ ShelleyBase
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  NewEpochState era ->
  SlotNo ->
  TransitionRule (tick era)
validatingTickTransition nes0 slot = do
  (curEpochNo, nes) <- liftSTS $ solidifyNextEpochPParams nes0 slot

  nes' <- trans @(EraRule "NEWEPOCH" era) $ TRC ((), nes, curEpochNo)
  let es'' = adoptGenesisDelegs (nesEs nes') slot

  pure $ nes' {nesEs = es''}

-- | This is a limited version of 'validatingTickTransition' which is only suitable
-- for the future ledger view.
validatingTickTransitionFORECAST ::
  forall tick era.
  ( State (tick era) ~ NewEpochState era
  , BaseM (tick era) ~ ShelleyBase
  , State (EraRule "UPEC" era) ~ UpecState era
  , Signal (EraRule "UPEC" era) ~ ()
  , Environment (EraRule "UPEC" era) ~ LedgerState era
  , Embed (EraRule "UPEC" era) (tick era)
  , STS (tick era)
  , GovState era ~ ShelleyGovState era
  , EraGov era
  , EraCertState era
  ) =>
  NewEpochState era ->
  SlotNo ->
  TransitionRule (tick era)
validatingTickTransitionFORECAST nes0 slot = do
  -- This whole function is a specialization of an inlined 'NEWEPOCH'.
  --
  -- The forecast is built entirely from the 'nesPd' and 'esPp' and
  -- 'dsGenDelegs', so the correctness of 'validatingTickTransitionFORECAST'
  -- only depends on getting these three fields correct.

  (curEpochNo, nes) <- liftSTS $ solidifyNextEpochPParams nes0 slot

  let es = nesEs nes
      ss = esSnapshots es

  -- the relevant 'NEWEPOCH' logic
  let pd' = ssStakeMarkPoolDistr ss

  -- note that the genesis delegates are updated not only on the epoch boundary.
  if curEpochNo /= succ (nesEL nes)
    then pure $ nes {nesEs = adoptGenesisDelegs es slot}
    else do
      -- We can skip 'SNAP'; we already have the equivalent pd'.

      -- We can skip 'MIR' and 'POOLREAP';
      -- we don't need to do the checks:
      -- if the checks would fail, then the node will fail in the 'TICK' rule
      -- if it ever then node tries to validate blocks for which the
      -- return value here was used to validate their headers.

      let pp = es ^. curPParamsEpochStateL
          ls = esLState es
          updates = utxosGovState $ lsUTxOState ls
      UpecState pp' _ <-
        trans @(EraRule "UPEC" era) $
          TRC (ls, UpecState pp updates, ())
      let es' =
            adoptGenesisDelegs es slot
              & curPParamsEpochStateL .~ pp'

      pure $!
        nes
          { nesPd = pd'
          , nesEs = es'
          }

bheadTransition ::
  forall era.
  ( EraGov era
  , EraCertState era
  , Embed (EraRule "NEWEPOCH" era) (TICK era)
  , Embed (EraRule "RUPD" era) (TICK era)
  , STS (TICK era)
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , State (EraRule "RUPD" era) ~ StrictMaybe PulsingRewUpdate
  , Signal (EraRule "RUPD" era) ~ SlotNo
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  TransitionRule (TICK era)
bheadTransition = do
  TRC ((), nes0@(NewEpochState _ bprev _ es _ _ _), slot) <-
    judgmentContext

  nes1 <- validatingTickTransition @TICK nes0 slot

  -- Here we force the evaluation of the mark snapshot
  -- and the per-pool stake distribution.
  -- We do NOT force it in the TICKF and TICKN rule
  -- so that it can remain a thunk when the consensus
  -- layer computes the ledger view across the epoch boundary.
  let !_ = ssStakeMark . esSnapshots . nesEs $ nes1
      !_ = ssStakeMarkPoolDistr . esSnapshots . nesEs $ nes1

  ru'' <-
    trans @(EraRule "RUPD" era) $
      TRC (RupdEnv bprev es, nesRu nes1, slot)

  let nes2 = nes1 {nesRu = ru''}
  pure nes2

instance
  ( STS (NEWEPOCH era)
  , Event (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  ) =>
  Embed (NEWEPOCH era) (TICK era)
  where
  wrapFailed = \case {}
  wrapEvent = TickNewEpochEvent

instance
  ( Era era
  , STS (RUPD era)
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  Embed (RUPD era) (TICK era)
  where
  wrapFailed = \case {}
  wrapEvent = TickRupdEvent

{------------------------------------------------------------------------------
-- TICKF transition

-- This is a variant on the TICK transition called only by the consensus layer
to tick the ledger state to a future slot.
------------------------------------------------------------------------------}

newtype ShelleyTickfEvent era
  = TickfUpecEvent (Event (EraRule "UPEC" era)) -- Subtransition Events

instance
  ( EraGov era
  , EraCertState era
  , GovState era ~ ShelleyGovState era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (EraRule "UPEC" era) ~ ()
  , State (EraRule "UPEC" era) ~ UpecState era
  , Environment (EraRule "UPEC" era) ~ LedgerState era
  , Embed (EraRule "UPEC" era) (TICKF era)
  ) =>
  STS (TICKF era)
  where
  type State (TICKF era) = NewEpochState era
  type Signal (TICKF era) = SlotNo
  type Environment (TICKF era) = ()
  type BaseM (TICKF era) = ShelleyBase
  type PredicateFailure (TICKF era) = Void
  type Event (TICKF era) = ShelleyTickfEvent era

  initialRules = []
  transitionRules =
    [ do
        TRC ((), nes, slot) <- judgmentContext
        validatingTickTransitionFORECAST nes slot
    ]

instance
  ( Era era
  , STS (UPEC era)
  , Event (EraRule "UPEC" era) ~ Void
  ) =>
  Embed (UPEC era) (TICKF era)
  where
  wrapFailed = \case {}
  wrapEvent = TickfUpecEvent
