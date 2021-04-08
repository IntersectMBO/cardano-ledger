{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Rules.Ledger
  ( AlonzoLEDGER,
    ledgerTransition,
  )
where

import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail, AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Tx (IsValidating (..), ValidatedTx (..))
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, TxInBlock)
import Cardano.Ledger.Shelley.Constraints (PParamsDelta)
import Control.State.Transition
  ( Assertion (..),
    AssertionViolation (..),
    Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Records (HasField, getField)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.EpochBoundary (obligation)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    PState (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.STS.Delegs (DELEGS, DelegsEnv (..), DelegsPredicateFailure)
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..), LedgerPredicateFailure (..))
import qualified Shelley.Spec.Ledger.STS.Ledgers as Shelley
import Shelley.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Shelley.Spec.Ledger.TxBody (DCert, EraIndependentTxBody)

-- =======================================

-- | The uninhabited type that marks the (STS Ledger) instance in the Alonzo Era.
data AlonzoLEDGER era

-- | An abstract Alonzo Era, Ledger transition. Fix 'someLedger' at a concrete type to
--   make it concrete. Depends only on the "certs" and "isValidating" HasField instances.
ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( Signal (someLEDGER era) ~ TxInBlock era,
    State (someLEDGER era) ~ (UTxOState era, DPState (Crypto era)),
    Environment (someLEDGER era) ~ LedgerEnv era,
    Embed (Core.EraRule "UTXOW" era) (someLEDGER era),
    Embed (Core.EraRule "DELEGS" era) (someLEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ TxInBlock era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "isValidating" (TxInBlock era) IsValidating,
    Era era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, (utxoSt, dpstate), tx) <- judgmentContext
  let txbody = getField @"body" tx

  dpstate' <-
    if getField @"isValidating" tx == IsValidating True
      then
        trans @(Core.EraRule "DELEGS" era) $
          TRC
            ( DelegsEnv slot txIx pp tx account,
              dpstate,
              StrictSeq.fromStrict $ getField @"certs" $ txbody
            )
      else pure dpstate

  let DPState dstate pstate = dpstate
      genDelegs = _genDelegs dstate
      stpools = _pParams pstate

  utxoSt' <-
    trans @(Core.EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv @era slot pp stpools genDelegs,
          utxoSt,
          tx
        )
  pure (utxoSt', dpstate')

instance
  ( Show (Core.Script era), -- All these Show instances arise because
    Show (Core.TxBody era), -- renderAssertionViolation, turns them into strings
    Show (Core.AuxiliaryData era),
    Show (Core.PParams era),
    Show (Core.Value era),
    Show (PParamsDelta era),
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era,
    TxInBlock era ~ ValidatedTx era,
    Embed (Core.EraRule "DELEGS" era) (AlonzoLEDGER era),
    Embed (Core.EraRule "UTXOW" era) (AlonzoLEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ ValidatedTx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    Show (UTxOState era)
  ) =>
  STS (AlonzoLEDGER era)
  where
  type
    State (AlonzoLEDGER era) =
      (UTxOState era, DPState (Crypto era))
  type Signal (AlonzoLEDGER era) = ValidatedTx era
  type Environment (AlonzoLEDGER era) = LedgerEnv era
  type BaseM (AlonzoLEDGER era) = ShelleyBase
  type PredicateFailure (AlonzoLEDGER era) = LedgerPredicateFailure era

  initialRules = []
  transitionRules = [ledgerTransition @AlonzoLEDGER]

  renderAssertionViolation AssertionViolation {avSTS, avMsg, avCtx, avState} =
    "AssertionViolation (" <> avSTS <> "): " <> avMsg
      <> "\n"
      <> show avCtx
      <> "\n"
      <> show avState

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (LedgerEnv {ledgerPp}, _, _))
           (utxoSt, DPState {_dstate, _pstate}) ->
              obligation ledgerPp (_rewards _dstate) (_pParams _pstate)
                == _deposited utxoSt
        )
    ]

instance
  ( Era era,
    STS (DELEGS era),
    PredicateFailure (Core.EraRule "DELEGS" era) ~ DelegsPredicateFailure era
  ) =>
  Embed (DELEGS era) (AlonzoLEDGER era)
  where
  wrapFailed = DelegsFailure

instance
  ( Era era,
    STS (AlonzoUTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ AlonzoPredFail era
  ) =>
  Embed (AlonzoUTXOW era) (AlonzoLEDGER era)
  where
  wrapFailed = UtxowFailure

instance
  ( Era era,
    STS (AlonzoLEDGER era),
    PredicateFailure (Core.EraRule "LEDGER" era) ~ LedgerPredicateFailure era
  ) =>
  Embed (AlonzoLEDGER era) (Shelley.LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
