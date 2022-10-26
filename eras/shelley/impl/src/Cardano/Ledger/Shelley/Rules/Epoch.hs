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
import Cardano.Ledger.Shelley.EpochBoundary (SnapShots, obligation)
import Cardano.Ledger.Shelley.Era (ShelleyEPOCH)
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState,
    LedgerState,
    PState (..),
    UpecState (..),
    esAccountState,
    esLState,
    esNonMyopic,
    esPp,
    esPrevPp,
    esSnapshots,
    lsDPState,
    lsUTxOState,
    rewards,
    _deposited,
    _ppups,
    _reserves,
    pattern DPState,
    pattern EpochState,
  )
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
    Default (PParams era),
    EraPParams era
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
    Signal (EraRule "UPEC" era) ~ (),
    EraPParams era
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

  let PState pParams fPParams _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { _pParams = ppp,
            _fPParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(EraRule "POOLREAP" era) $
      TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  let epochState' =
        EpochState
          acnt'
          ss'
          (ls {lsUTxOState = utxoSt', lsDPState = DPState dstate' pstate''})
          pr
          pp
          nm

  UpecState pp' ppupSt' <-
    trans @(EraRule "UPEC" era) $
      TRC (epochState', UpecState pp (_ppups utxoSt'), ())
  let utxoSt'' = utxoSt' {_ppups = ppupSt'}

  let Coin oblgCurr = obligation pp (rewards dstate') (_pParams pstate'')
      Coin oblgNew = obligation pp' (rewards dstate') (_pParams pstate'')
      Coin reserves = _reserves acnt'
      utxoSt''' = utxoSt'' {_deposited = Coin oblgNew}
      acnt'' = acnt' {_reserves = Coin $ reserves + oblgCurr - oblgNew}
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
