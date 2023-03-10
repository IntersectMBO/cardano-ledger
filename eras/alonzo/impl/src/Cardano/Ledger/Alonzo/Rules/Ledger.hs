{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Ledger (
  AlonzoLEDGER,
  ledgerTransition,
) where

import Cardano.Ledger.Alonzo.Era (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW, AlonzoUtxowEvent, AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), AlonzoTx (..), IsValid (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  LedgerState (..),
  UTxOState (..),
  obligationCertState,
 )
import Cardano.Ledger.Shelley.Rules (
  DelegsEnv (..),
  ShelleyDELEGS,
  ShelleyDelegsEvent,
  ShelleyDelegsPredFailure,
  ShelleyLEDGERS,
  UtxoEnv (..),
 )
import Cardano.Ledger.Shelley.Rules as Shelley (
  LedgerEnv (..),
  ShelleyLedgerEvent (..),
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  depositEqualsObligation,
 )
import Control.State.Transition (
  Assertion (..),
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro

-- =======================================

-- | An abstract Alonzo Era, Ledger transition. Fix 'someLedger' at a concrete type to
--   make it concrete.
ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "DELEGS" era) (someLEDGER era)
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , AlonzoEraTx era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, LedgerState utxoSt dpstate, tx) <- judgmentContext
  let txBody = tx ^. bodyTxL

  dpstate' <-
    if tx ^. isValidTxL == IsValid True
      then
        trans @(EraRule "DELEGS" era) $
          TRC
            ( DelegsEnv slot txIx pp tx account
            , dpstate
            , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
            )
      else pure dpstate

  let CertState _ _pstate dstate = dpstate
      genDelegs = dsGenDelegs dstate

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv @era slot pp dpstate genDelegs
        , utxoSt
        , tx
        )
  pure $ LedgerState utxoSt' dpstate'

instance
  ( DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , AlonzoEraTx era
  , Tx era ~ AlonzoTx era
  , Embed (EraRule "DELEGS" era) (AlonzoLEDGER era)
  , Embed (EraRule "UTXOW" era) (AlonzoLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ AlonzoTx era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert era)
  , ProtVerAtMost era 8
  ) =>
  STS (AlonzoLEDGER era)
  where
  type State (AlonzoLEDGER era) = LedgerState era
  type Signal (AlonzoLEDGER era) = AlonzoTx era
  type Environment (AlonzoLEDGER era) = LedgerEnv era
  type BaseM (AlonzoLEDGER era) = ShelleyBase
  type PredicateFailure (AlonzoLEDGER era) = ShelleyLedgerPredFailure era
  type Event (AlonzoLEDGER era) = ShelleyLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @AlonzoLEDGER]

  renderAssertionViolation = Shelley.depositEqualsObligation

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation (AlonzoLEDGER)"
        ( \(TRC (_, _, _))
           (LedgerState utxoSt dpstate) -> obligationCertState dpstate == utxosDeposited utxoSt
        )
    ]

instance
  ( Era era
  , STS (ShelleyDELEGS era)
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , Event (EraRule "DELEGS" era) ~ ShelleyDelegsEvent era
  ) =>
  Embed (ShelleyDELEGS era) (AlonzoLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era
  , STS (AlonzoUTXOW era)
  , PredicateFailure (EraRule "UTXOW" era) ~ AlonzoUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (AlonzoUTXOW era) (AlonzoLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Era era
  , STS (AlonzoLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ShelleyLedgerEvent era
  ) =>
  Embed (AlonzoLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
