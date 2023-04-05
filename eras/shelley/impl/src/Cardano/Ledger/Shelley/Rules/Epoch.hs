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

module Cardano.Ledger.Shelley.Rules.Epoch (
  ShelleyEPOCH,
  ShelleyEpochPredFailure (..),
  ShelleyEpochEvent (..),
  PredicateFailure,
  UpecPredFailure,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.EpochBoundary (SnapShots)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEPOCH)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  PState (..),
  UTxOState (utxosDeposited, utxosGovernance),
  asReserves,
  esAccountState,
  esLState,
  esNonMyopic,
  esPp,
  esPrevPp,
  esSnapshots,
  lsCertState,
  lsUTxOState,
  obligationCertState,
  pattern CertState,
  pattern EpochState,
 )
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Rules.PoolReap (
  ShelleyPOOLREAP,
  ShelleyPoolreapEvent,
  ShelleyPoolreapPredFailure,
  ShelleyPoolreapState (..),
 )
import Cardano.Ledger.Shelley.Rules.Snap (ShelleySNAP, ShelleySnapPredFailure, SnapEnv (..), SnapEvent)
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC, ShelleyUpecPredFailure, UpecState (..))
import Cardano.Ledger.Slot (EpochNo)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition (
  Embed (..),
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

type UpecPredFailure era = UpecPredFailurePV (ProtVerLow era) era

type family UpecPredFailurePV pv era where
  UpecPredFailurePV 2 era = ShelleyUpecPredFailure era
  UpecPredFailurePV 3 era = ShelleyUpecPredFailure era
  UpecPredFailurePV 4 era = ShelleyUpecPredFailure era
  UpecPredFailurePV 5 era = ShelleyUpecPredFailure era
  UpecPredFailurePV 6 era = ShelleyUpecPredFailure era
  UpecPredFailurePV 7 era = ShelleyUpecPredFailure era
  UpecPredFailurePV 8 era = ShelleyUpecPredFailure era
  UpecPredFailurePV _ era = Void

data ShelleyEpochPredFailure era
  = PoolReapFailure (PredicateFailure (EraRule "POOLREAP" era)) -- Subtransition Failures
  | SnapFailure (PredicateFailure (EraRule "SNAP" era)) -- Subtransition Failures
  | UpecFailure (UpecPredFailure era) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "POOLREAP" era))
  , Eq (PredicateFailure (EraRule "SNAP" era))
  , Eq (UpecPredFailure era)
  ) =>
  Eq (ShelleyEpochPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "POOLREAP" era))
  , Show (PredicateFailure (EraRule "SNAP" era))
  , Show (UpecPredFailure era)
  ) =>
  Show (ShelleyEpochPredFailure era)

data ShelleyEpochEvent era
  = PoolReapEvent (Event (EraRule "POOLREAP" era))
  | SnapEvent (Event (EraRule "SNAP" era))
  | UpecEvent (Event (EraRule "UPEC" era))

instance
  ( EraTxOut era
  , EraGovernance era
  , GovernanceState era ~ ShelleyPPUPState era
  , Embed (EraRule "SNAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era)
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ PParams era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "UPEC" era) (ShelleyEPOCH era)
  , Environment (EraRule "UPEC" era) ~ EpochState era
  , State (EraRule "UPEC" era) ~ UpecState era
  , Signal (EraRule "UPEC" era) ~ ()
  , Default (PParams era)
  , Eq (UpecPredFailure era)
  , Show (UpecPredFailure era)
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
  ( NoThunks (PredicateFailure (EraRule "POOLREAP" era))
  , NoThunks (PredicateFailure (EraRule "SNAP" era))
  , NoThunks (UpecPredFailure era)
  ) =>
  NoThunks (ShelleyEpochPredFailure era)

epochTransition ::
  forall era.
  ( Embed (EraRule "SNAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era)
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ PParams era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "UPEC" era) (ShelleyEPOCH era)
  , Environment (EraRule "UPEC" era) ~ EpochState era
  , State (EraRule "UPEC" era) ~ UpecState era
  , Signal (EraRule "UPEC" era) ~ ()
  , GovernanceState era ~ ShelleyPPUPState era
  ) =>
  TransitionRule (ShelleyEPOCH era)
epochTransition = do
  TRC
    ( _
      , EpochState
          { esAccountState = acnt
          , esSnapshots = ss
          , esLState = ls
          , esPrevPp = pr
          , esPp = pp
          , esNonMyopic = nm
          }
      , e
      ) <-
    judgmentContext
  let utxoSt = lsUTxOState ls
  let CertState vstate pstate dstate = lsCertState ls
  ss' <-
    trans @(EraRule "SNAP" era) $ TRC (SnapEnv ls pp, ss, ())

  let PState pParams fPParams _ _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { psStakePoolParams = ppp
          , psFutureStakePoolParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(EraRule "POOLREAP" era) $
      TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  let adjustedCertState = CertState vstate pstate'' dstate'
      epochState' =
        EpochState
          acnt'
          ss'
          (ls {lsUTxOState = utxoSt', lsCertState = adjustedCertState})
          pr
          pp
          nm

  UpecState pp' ppupSt' <-
    trans @(EraRule "UPEC" era) $
      TRC (epochState', UpecState pp (utxosGovernance utxoSt'), ())
  let utxoSt'' = utxoSt' {utxosGovernance = ppupSt'}

  let
    -- At the epoch boundary refunds are made, so we need to change what
    -- the utxosDeposited field is. The other two places where deposits are
    -- kept (dsUnified of DState and psDeposits of PState) are adjusted by
    -- the rules, So we can recompute the utxosDeposited field using adjustedCertState
    -- since we have the invariant that: obligationCertState dpstate == utxosDeposited utxostate
    oblgNew = obligationCertState adjustedCertState
    reserves = asReserves acnt'
    utxoSt''' = utxoSt'' {utxosDeposited = oblgNew}
    acnt'' = acnt' {asReserves = reserves}
  pure $
    epochState'
      { esAccountState = acnt''
      , esLState = (esLState epochState') {lsUTxOState = utxoSt'''}
      , esPrevPp = pp
      , esPp = pp'
      }

instance
  ( EraTxOut era
  , PredicateFailure (EraRule "SNAP" era) ~ ShelleySnapPredFailure era
  , Event (EraRule "SNAP" era) ~ SnapEvent era
  ) =>
  Embed (ShelleySNAP era) (ShelleyEPOCH era)
  where
  wrapFailed = SnapFailure
  wrapEvent = SnapEvent

instance
  ( Era era
  , STS (ShelleyPOOLREAP era)
  , PredicateFailure (EraRule "POOLREAP" era) ~ ShelleyPoolreapPredFailure era
  , Event (EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  ) =>
  Embed (ShelleyPOOLREAP era) (ShelleyEPOCH era)
  where
  wrapFailed = PoolReapFailure
  wrapEvent = PoolReapEvent

instance
  ( Era era
  , STS (ShelleyUPEC era)
  , UpecPredFailure era ~ ShelleyUpecPredFailure era
  , Event (EraRule "UPEC" era) ~ Void
  ) =>
  Embed (ShelleyUPEC era) (ShelleyEPOCH era)
  where
  wrapFailed = UpecFailure
  wrapEvent = UpecEvent
