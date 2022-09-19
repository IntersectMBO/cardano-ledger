{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Epoch
  ( ShelleyEPOCH,
    ShelleyEpochPredFailure (..),
    ShelleyEpochEvent (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.EpochBoundary (SnapShots)
import Cardano.Ledger.Shelley.Era (ShelleyEPOCH)
import Cardano.Ledger.Shelley.LedgerState (EpochState, LedgerState, PState (..), UTxOState (utxosDeposited, utxosPpups), UpecState (..), asReserves, esAccountState, esLState, esNonMyopic, esPp, esPrevPp, esSnapshots, lsDPState, lsUTxOState, obligationDPState, pattern DPState, pattern EpochState)
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Rules.PoolReap
  ( ShelleyPOOLREAP,
    ShelleyPoolreapEvent,
    ShelleyPoolreapPredFailure,
    ShelleyPoolreapState (..),
  )
import Cardano.Ledger.Shelley.Rules.Snap (ShelleySNAP, ShelleySnapPredFailure, SnapEvent)
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC, ShelleyUpecPredFailure)
import Cardano.Ledger.Slot (EpochNo)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Default.Class (Default)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data ShelleyEpochPredFailure era
  = PoolReapFailure (PredicateFailure (EraRule "POOLREAP" era)) -- Subtransition Failures
  | SnapFailure (PredicateFailure (EraRule "SNAP" era)) -- Subtransition Failures
  | UpecFailure (PredicateFailure (EraRule "UPEC" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "POOLREAP" era)),
    Eq (PredicateFailure (EraRule "SNAP" era)),
    Eq (PredicateFailure (EraRule "UPEC" era))
  ) =>
  Eq (ShelleyEpochPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "POOLREAP" era)),
    Show (PredicateFailure (EraRule "SNAP" era)),
    Show (PredicateFailure (EraRule "UPEC" era))
  ) =>
  Show (ShelleyEpochPredFailure era)

data ShelleyEpochEvent era
  = PoolReapEvent (Event (EraRule "POOLREAP" era))
  | SnapEvent (Event (EraRule "SNAP" era))
  | UpecEvent (Event (EraRule "UPEC" era))

instance
  ( EraTxOut era,
    Embed (EraRule "SNAP" era) (ShelleyEPOCH era),
    Environment (EraRule "SNAP" era) ~ LedgerState era,
    State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era),
    Signal (EraRule "SNAP" era) ~ (),
    Embed (EraRule "POOLREAP" era) (ShelleyEPOCH era),
    Environment (EraRule "POOLREAP" era) ~ PParams era,
    State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era,
    Signal (EraRule "POOLREAP" era) ~ EpochNo,
    Embed (EraRule "UPEC" era) (ShelleyEPOCH era),
    Environment (EraRule "UPEC" era) ~ EpochState era,
    State (EraRule "UPEC" era) ~ UpecState era,
    Signal (EraRule "UPEC" era) ~ (),
    Default (State (EraRule "PPUP" era)),
    Default (PParams era)
  ) =>
  STS (ShelleyEPOCH era)
  where
  type State (ShelleyEPOCH era) = EpochState era
  type Signal (ShelleyEPOCH era) = EpochNo
  type Environment (ShelleyEPOCH era) = ()
  type BaseM (ShelleyEPOCH era) = ShelleyBase
  type PredicateFailure (ShelleyEPOCH era) = ShelleyEpochPredFailure era
  type Event (ShelleyEPOCH era) = ShelleyEpochEvent era
  transitionRules = [epochTransition]

instance
  ( NoThunks (PredicateFailure (EraRule "POOLREAP" era)),
    NoThunks (PredicateFailure (EraRule "SNAP" era)),
    NoThunks (PredicateFailure (EraRule "UPEC" era))
  ) =>
  NoThunks (ShelleyEpochPredFailure era)

epochTransition ::
  forall era.
  ( Embed (EraRule "SNAP" era) (ShelleyEPOCH era),
    Environment (EraRule "SNAP" era) ~ LedgerState era,
    State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era),
    Signal (EraRule "SNAP" era) ~ (),
    Embed (EraRule "POOLREAP" era) (ShelleyEPOCH era),
    Environment (EraRule "POOLREAP" era) ~ PParams era,
    State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era,
    Signal (EraRule "POOLREAP" era) ~ EpochNo,
    Embed (EraRule "UPEC" era) (ShelleyEPOCH era),
    Environment (EraRule "UPEC" era) ~ EpochState era,
    State (EraRule "UPEC" era) ~ UpecState era,
    Signal (EraRule "UPEC" era) ~ ()
  ) =>
  TransitionRule (ShelleyEPOCH era)
epochTransition = do
  TRC
    ( _,
      EpochState
        { esAccountState = acnt,
          esSnapshots = ss,
          esLState = ls,
          esPrevPp = pr,
          esPp = pp,
          esNonMyopic = nm
        },
      e
      ) <-
    judgmentContext
  let utxoSt = lsUTxOState ls
  let DPState dstate pstate = lsDPState ls
  ss' <-
    trans @(EraRule "SNAP" era) $ TRC (ls, ss, ())

  let PState pParams fPParams _ _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { psStakePoolParams = ppp,
            psFutureStakePoolParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(EraRule "POOLREAP" era) $
      TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  let adjustedDPstate = DPState dstate' pstate''
      epochState' =
        EpochState
          acnt'
          ss'
          (ls {lsUTxOState = utxoSt', lsDPState = adjustedDPstate})
          pr
          pp
          nm

  UpecState pp' ppupSt' <-
    trans @(EraRule "UPEC" era) $
      TRC (epochState', UpecState pp (utxosPpups utxoSt'), ())
  let utxoSt'' = utxoSt' {utxosPpups = ppupSt'}

  let -- At the epoch boundary refunds are made, so we need to change what
      -- the utxosDeposited field is. The other two places where deposits are
      -- kept (dsDeposits of DState and psDeposits of PState) are adjusted by
      -- the rules, So we can recompute the utxosDeposited field using adjustedDPState
      -- since we have the invariant that: obligationDPState dpstate == utxosDeposited utxostate
      Coin oblgNew = obligationDPState adjustedDPstate
      Coin reserves = asReserves acnt'
      utxoSt''' = utxoSt'' {utxosDeposited = Coin oblgNew}
      acnt'' = acnt' {asReserves = Coin reserves}
  pure $
    epochState'
      { esAccountState = acnt'',
        esLState = (esLState epochState') {lsUTxOState = utxoSt'''},
        esPrevPp = pp,
        esPp = pp'
      }

instance
  ( EraTxOut era,
    PredicateFailure (EraRule "SNAP" era) ~ ShelleySnapPredFailure era,
    Event (EraRule "SNAP" era) ~ SnapEvent era
  ) =>
  Embed (ShelleySNAP era) (ShelleyEPOCH era)
  where
  wrapFailed = SnapFailure
  wrapEvent = SnapEvent

instance
  ( Era era,
    STS (ShelleyPOOLREAP era),
    PredicateFailure (EraRule "POOLREAP" era) ~ ShelleyPoolreapPredFailure era,
    Event (EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  ) =>
  Embed (ShelleyPOOLREAP era) (ShelleyEPOCH era)
  where
  wrapFailed = PoolReapFailure
  wrapEvent = PoolReapEvent

instance
  ( Era era,
    STS (ShelleyUPEC era),
    PredicateFailure (EraRule "UPEC" era) ~ ShelleyUpecPredFailure era,
    Event (EraRule "UPEC" era) ~ Void
  ) =>
  Embed (ShelleyUPEC era) (ShelleyEPOCH era)
  where
  wrapFailed = UpecFailure
  wrapEvent = UpecEvent
