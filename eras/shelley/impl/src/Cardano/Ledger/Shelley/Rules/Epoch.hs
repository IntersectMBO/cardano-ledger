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
{-# LANGUAGE TypeOperators #-}
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
  LedgerState,
  PState (..),
  UTxOState (utxosDeposited, utxosGovState),
  curPParamsEpochStateL,
  esAccountState,
  esLState,
  esLStateL,
  esNonMyopic,
  esSnapshots,
  lsCertState,
  lsUTxOState,
  lsUTxOStateL,
  totalObligation,
  utxosGovStateL,
  pattern CertState,
  pattern EpochState,
 )
import Cardano.Ledger.Shelley.LedgerState.Types (prevPParamsEpochStateL)
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Rules.PoolReap (
  ShelleyPOOLREAP,
  ShelleyPoolreapEnv (..),
  ShelleyPoolreapEvent,
  ShelleyPoolreapPredFailure,
  ShelleyPoolreapState (..),
 )
import Cardano.Ledger.Shelley.Rules.Snap (
  ShelleySNAP,
  ShelleySnapPredFailure,
  SnapEnv (..),
  SnapEvent,
 )
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC, ShelleyUpecPredFailure, UpecState (..))
import Cardano.Ledger.Slot (EpochNo)
import Control.DeepSeq (NFData)
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
import Lens.Micro
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

instance
  ( NFData (PredicateFailure (EraRule "POOLREAP" era))
  , NFData (PredicateFailure (EraRule "SNAP" era))
  , NFData (UpecPredFailure era)
  ) =>
  NFData (ShelleyEpochPredFailure era)

data ShelleyEpochEvent era
  = PoolReapEvent (Event (EraRule "POOLREAP" era))
  | SnapEvent (Event (EraRule "SNAP" era))
  | UpecEvent (Event (EraRule "UPEC" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "POOLREAP" era))
  , Eq (Event (EraRule "SNAP" era))
  , Eq (Event (EraRule "UPEC" era))
  ) =>
  Eq (ShelleyEpochEvent era)

instance
  ( NFData (Event (EraRule "POOLREAP" era))
  , NFData (Event (EraRule "SNAP" era))
  , NFData (Event (EraRule "UPEC" era))
  ) =>
  NFData (ShelleyEpochEvent era)

instance
  ( EraTxOut era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , Embed (EraRule "SNAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era)
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ ShelleyPoolreapEnv era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "UPEC" era) (ShelleyEPOCH era)
  , Environment (EraRule "UPEC" era) ~ LedgerState era
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
  , Environment (EraRule "POOLREAP" era) ~ ShelleyPoolreapEnv era
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "UPEC" era) (ShelleyEPOCH era)
  , Environment (EraRule "UPEC" era) ~ LedgerState era
  , State (EraRule "UPEC" era) ~ UpecState era
  , Signal (EraRule "UPEC" era) ~ ()
  , GovState era ~ ShelleyGovState era
  , EraGov era
  ) =>
  TransitionRule (ShelleyEPOCH era)
epochTransition = do
  TRC
    ( _
      , es@EpochState
          { esAccountState = acnt
          , esSnapshots = ss
          , esLState = ls
          , esNonMyopic = nm
          }
      , e
      ) <-
    judgmentContext
  let pp = es ^. curPParamsEpochStateL
      utxoSt = lsUTxOState ls
      CertState vstate pstate dstate = lsCertState ls
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
      TRC (ShelleyPoolreapEnv vstate, PoolreapState utxoSt acnt dstate pstate', e)

  let adjustedCertState = CertState vstate pstate'' dstate'
      ls' = ls {lsUTxOState = utxoSt', lsCertState = adjustedCertState}

  UpecState pp' ppupSt' <-
    trans @(EraRule "UPEC" era) $
      TRC (ls', UpecState pp (utxosGovState utxoSt'), ())
  let utxoSt'' = utxoSt' {utxosGovState = ppupSt'}

  let
    -- At the epoch boundary refunds are made, so we need to change what
    -- the utxosDeposited field is. The other two places where deposits are
    -- kept (dsUnified of DState and psDeposits of PState) are adjusted by
    -- the rules, So we can recompute the utxosDeposited field using adjustedCertState
    -- since we have the invariant that: obligationCertState dpstate == utxosDeposited utxostate
    oblgNew = totalObligation adjustedCertState (utxoSt'' ^. utxosGovStateL)
    utxoSt''' = utxoSt'' {utxosDeposited = oblgNew}
  pure $
    EpochState acnt' ls' ss' nm
      & esLStateL . lsUTxOStateL .~ utxoSt'''
      & prevPParamsEpochStateL .~ pp
      & curPParamsEpochStateL .~ pp'

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
