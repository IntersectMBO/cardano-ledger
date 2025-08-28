{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
  ShelleyEpochEvent (..),
  PredicateFailure,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEPOCH)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  LedgerState,
  UTxOState (utxosDeposited, utxosGovState),
  curPParamsEpochStateL,
  esChainAccountState,
  esLState,
  esLStateL,
  esNonMyopic,
  esSnapshots,
  lsCertState,
  lsCertStateL,
  lsUTxOState,
  lsUTxOStateL,
  totalObligation,
  utxosGovStateL,
  pattern EpochState,
 )
import Cardano.Ledger.Shelley.LedgerState.Types (prevPParamsEpochStateL)
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Rules.PoolReap (
  ShelleyPOOLREAP,
  ShelleyPoolreapEvent,
  ShelleyPoolreapState (..),
 )
import Cardano.Ledger.Shelley.Rules.Snap (
  ShelleySNAP,
  SnapEnv (..),
  SnapEvent,
 )
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC, UpecState (..))
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Ledger.State
import Control.DeepSeq (NFData)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Default (Default)
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro

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
  , EraStake era
  , EraCertState era
  , GovState era ~ ShelleyGovState era
  , Embed (EraRule "SNAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ ()
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "UPEC" era) (ShelleyEPOCH era)
  , Environment (EraRule "UPEC" era) ~ LedgerState era
  , State (EraRule "UPEC" era) ~ UpecState era
  , Signal (EraRule "UPEC" era) ~ ()
  , Default (PParams era)
  ) =>
  STS (ShelleyEPOCH era)
  where
  type State (ShelleyEPOCH era) = EpochState era
  type Signal (ShelleyEPOCH era) = EpochNo
  type Environment (ShelleyEPOCH era) = ()
  type BaseM (ShelleyEPOCH era) = ShelleyBase
  type PredicateFailure (ShelleyEPOCH era) = Void
  type Event (ShelleyEPOCH era) = ShelleyEpochEvent era
  transitionRules = [epochTransition]

epochTransition ::
  forall era.
  ( Embed (EraRule "SNAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "SNAP" era) ~ SnapEnv era
  , State (EraRule "SNAP" era) ~ SnapShots
  , Signal (EraRule "SNAP" era) ~ ()
  , Embed (EraRule "POOLREAP" era) (ShelleyEPOCH era)
  , Environment (EraRule "POOLREAP" era) ~ ()
  , State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era
  , Signal (EraRule "POOLREAP" era) ~ EpochNo
  , Embed (EraRule "UPEC" era) (ShelleyEPOCH era)
  , Environment (EraRule "UPEC" era) ~ LedgerState era
  , State (EraRule "UPEC" era) ~ UpecState era
  , Signal (EraRule "UPEC" era) ~ ()
  , GovState era ~ ShelleyGovState era
  , EraGov era
  , EraCertState era
  ) =>
  TransitionRule (ShelleyEPOCH era)
epochTransition = do
  TRC
    ( _
      , es@EpochState
          { esChainAccountState = chainAccountState
          , esSnapshots = ss
          , esLState = ls
          , esNonMyopic = nm
          }
      , e
      ) <-
    judgmentContext
  let pp = es ^. curPParamsEpochStateL
      utxoSt = lsUTxOState ls
      certState = ls ^. lsCertStateL
  ss' <-
    trans @(EraRule "SNAP" era) $ TRC (SnapEnv ls pp, ss, ())

  PoolreapState utxoSt' chainAccountState' adjustedCertState <-
    trans @(EraRule "POOLREAP" era) $
      TRC ((), PoolreapState utxoSt chainAccountState certState, e)

  let ls' = ls {lsUTxOState = utxoSt', lsCertState = adjustedCertState}

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
    EpochState chainAccountState' ls' ss' nm
      & esLStateL . lsUTxOStateL .~ utxoSt'''
      & prevPParamsEpochStateL .~ pp
      & curPParamsEpochStateL .~ pp'

instance
  ( EraTxOut era
  , EraStake era
  , Event (EraRule "SNAP" era) ~ SnapEvent era
  , EraCertState era
  ) =>
  Embed (ShelleySNAP era) (ShelleyEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = SnapEvent

instance
  ( Era era
  , STS (ShelleyPOOLREAP era)
  , Event (EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  ) =>
  Embed (ShelleyPOOLREAP era) (ShelleyEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = PoolReapEvent

instance
  ( Era era
  , STS (ShelleyUPEC era)
  , Event (EraRule "UPEC" era) ~ Void
  ) =>
  Embed (ShelleyUPEC era) (ShelleyEPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = UpecEvent
