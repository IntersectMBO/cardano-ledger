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
  ShelleyTICK,
  State,
  ShelleyTickEvent (..),
  PredicateFailure,
  adoptGenesisDelegs,
  ShelleyTICKF,
  validatingTickTransition,
  validatingTickTransitionFORECAST,
  solidifyNextEpochPParams,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyTICK, ShelleyTICKF)
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
import Cardano.Ledger.Shelley.Rules.NewEpoch (ShelleyNEWEPOCH, ShelleyNewEpochEvent)
import Cardano.Ledger.Shelley.Rules.Rupd (
  RupdEnv (..),
  RupdEvent,
  ShelleyRUPD,
 )
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC, UpecState (..))
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
  , Embed (EraRule "NEWEPOCH" era) (ShelleyTICK era)
  , Embed (EraRule "RUPD" era) (ShelleyTICK era)
  , State (ShelleyTICK era) ~ NewEpochState era
  , BaseM (ShelleyTICK era) ~ ShelleyBase
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , State (EraRule "RUPD" era) ~ StrictMaybe PulsingRewUpdate
  , Signal (EraRule "RUPD" era) ~ SlotNo
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  STS (ShelleyTICK era)
  where
  type State (ShelleyTICK era) = NewEpochState era
  type Signal (ShelleyTICK era) = SlotNo
  type Environment (ShelleyTICK era) = ()
  type BaseM (ShelleyTICK era) = ShelleyBase
  type PredicateFailure (ShelleyTICK era) = Void
  type Event (ShelleyTICK era) = ShelleyTickEvent era

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
  -- The ledger view, 'LedgerView', is built entirely from the 'nesPd' and 'esPp' and
  -- 'dsGenDelegs', so the correctness of 'validatingTickTransitionFORECAST' only
  -- depends on getting these three fields correct.

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
  , Embed (EraRule "NEWEPOCH" era) (ShelleyTICK era)
  , Embed (EraRule "RUPD" era) (ShelleyTICK era)
  , STS (ShelleyTICK era)
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , State (EraRule "RUPD" era) ~ StrictMaybe PulsingRewUpdate
  , Signal (EraRule "RUPD" era) ~ SlotNo
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  ) =>
  TransitionRule (ShelleyTICK era)
bheadTransition = do
  TRC ((), nes0@(NewEpochState _ bprev _ es _ _ _), slot) <-
    judgmentContext

  nes1 <- validatingTickTransition @ShelleyTICK nes0 slot

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
  ( STS (ShelleyNEWEPOCH era)
  , Event (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  ) =>
  Embed (ShelleyNEWEPOCH era) (ShelleyTICK era)
  where
  wrapFailed = \case {}
  wrapEvent = TickNewEpochEvent

instance
  ( Era era
  , STS (ShelleyRUPD era)
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  Embed (ShelleyRUPD era) (ShelleyTICK era)
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
  , Embed (EraRule "UPEC" era) (ShelleyTICKF era)
  ) =>
  STS (ShelleyTICKF era)
  where
  type State (ShelleyTICKF era) = NewEpochState era
  type Signal (ShelleyTICKF era) = SlotNo
  type Environment (ShelleyTICKF era) = ()
  type BaseM (ShelleyTICKF era) = ShelleyBase
  type PredicateFailure (ShelleyTICKF era) = Void
  type Event (ShelleyTICKF era) = ShelleyTickfEvent era

  initialRules = []
  transitionRules =
    [ do
        TRC ((), nes, slot) <- judgmentContext
        validatingTickTransitionFORECAST nes slot
    ]

instance
  ( Era era
  , STS (ShelleyUPEC era)
  , Event (EraRule "UPEC" era) ~ Void
  ) =>
  Embed (ShelleyUPEC era) (ShelleyTICKF era)
  where
  wrapFailed = \case {}
  wrapEvent = TickfUpecEvent
