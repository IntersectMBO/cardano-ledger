{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module Cardano.Ledger.Shelley.Rules.Tick (
  ShelleyTICK,
  State,
  ShelleyTickPredFailure (..),
  ShelleyTickEvent (..),
  PredicateFailure,
  adoptGenesisDelegs,
  ShelleyTICKF,
  ShelleyTickfPredFailure,
  validatingTickTransition,
  validatingTickTransitionFORECAST,
  solidifyNextEpochPParams,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Core
import Cardano.Ledger.EpochBoundary (SnapShots (ssStakeMark, ssStakeMarkPoolDistr))
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyTICK, ShelleyTICKF)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  EpochState (..),
  FutureGenDeleg (..),
  LedgerState (..),
  NewEpochState (..),
  PulsingRewUpdate,
  UTxOState (..),
  curPParamsEpochStateL,
  newEpochStateGovStateL,
 )
import Cardano.Ledger.Shelley.Rules.NewEpoch (
  ShelleyNEWEPOCH,
  ShelleyNewEpochEvent,
  ShelleyNewEpochPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Rupd (
  RupdEnv (..),
  RupdEvent,
  ShelleyRUPD,
  ShelleyRupdPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC, ShelleyUpecPredFailure, UpecState (..))
import Cardano.Ledger.Slot (EpochNo, SlotNo, getTheSlotOfNoReturn)
import Control.DeepSeq (NFData)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

-- ==================================================

data ShelleyTickPredFailure era
  = NewEpochFailure (PredicateFailure (EraRule "NEWEPOCH" era)) -- Subtransition Failures
  | RupdFailure (PredicateFailure (EraRule "RUPD" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (EraRule "NEWEPOCH" era))
  , Show (PredicateFailure (EraRule "RUPD" era))
  ) =>
  Show (ShelleyTickPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "NEWEPOCH" era))
  , Eq (PredicateFailure (EraRule "RUPD" era))
  ) =>
  Eq (ShelleyTickPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "NEWEPOCH" era))
  , NoThunks (PredicateFailure (EraRule "RUPD" era))
  ) =>
  NoThunks (ShelleyTickPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "NEWEPOCH" era))
  , NFData (PredicateFailure (EraRule "RUPD" era))
  ) =>
  NFData (ShelleyTickPredFailure era)

data ShelleyTickEvent era
  = TickNewEpochEvent (Event (EraRule "NEWEPOCH" era))
  | TickRupdEvent (Event (EraRule "RUPD" era))
  deriving (Generic)

type instance EraRuleEvent "TICK" (ShelleyEra c) = ShelleyTickEvent (ShelleyEra c)

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
  , Embed (EraRule "NEWEPOCH" era) (ShelleyTICK era)
  , Embed (EraRule "RUPD" era) (ShelleyTICK era)
  , State (ShelleyTICK era) ~ NewEpochState era
  , BaseM (ShelleyTICK era) ~ ShelleyBase
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , State (EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (EraCrypto era))
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
    dp = lsCertState ls
    ds = certDState dp
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
        , dsGenDelegs = GenDelegs $ eval (genDelegs ⨃ genDelegs')
        }
    dp' = dp {certDState = ds'}
    ls' = ls {lsCertState = dp'}
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
  , Embed (EraRule "NEWEPOCH" era) (ShelleyTICK era)
  , Embed (EraRule "RUPD" era) (ShelleyTICK era)
  , STS (ShelleyTICK era)
  , State (ShelleyTICK era) ~ NewEpochState era
  , BaseM (ShelleyTICK era) ~ ShelleyBase
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , State (EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (EraCrypto era))
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
  , PredicateFailure (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochPredFailure era
  , Event (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  ) =>
  Embed (ShelleyNEWEPOCH era) (ShelleyTICK era)
  where
  wrapFailed = NewEpochFailure
  wrapEvent = TickNewEpochEvent

instance
  ( Era era
  , STS (ShelleyRUPD era)
  , PredicateFailure (EraRule "RUPD" era) ~ ShelleyRupdPredFailure era
  , Event (EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
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
  = TickfUpecFailure (PredicateFailure (EraRule "UPEC" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "UPEC" era))
  ) =>
  Show (ShelleyTickfPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UPEC" era))
  ) =>
  Eq (ShelleyTickfPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "UPEC" era)) =>
  NoThunks (ShelleyTickfPredFailure era)

newtype ShelleyTickfEvent era
  = TickfUpecEvent (Event (EraRule "UPEC" era)) -- Subtransition Events

instance
  ( EraGov era
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
  type PredicateFailure (ShelleyTICKF era) = ShelleyTickfPredFailure era
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
  , PredicateFailure (EraRule "UPEC" era) ~ ShelleyUpecPredFailure era
  , Event (EraRule "UPEC" era) ~ Void
  ) =>
  Embed (ShelleyUPEC era) (ShelleyTICKF era)
  where
  wrapFailed = TickfUpecFailure
  wrapEvent = TickfUpecEvent
