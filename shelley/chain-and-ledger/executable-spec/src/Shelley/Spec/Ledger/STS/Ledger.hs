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

module Shelley.Spec.Ledger.STS.Ledger
  ( LEDGER,
    LedgerEnv (..),
    LedgerPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.Constraints
  ( UsesAuxiliary,
    UsesPParams,
    UsesScript,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
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
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.EpochBoundary (obligation)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState (..),
    DState (..),
    Ix,
    PState (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.STS.Delegs (DELEGS, DelegsEnv (..), DelegsPredicateFailure)
import Shelley.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Shelley.Spec.Ledger.STS.Utxow (UTXOW, UtxowPredicateFailure)
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxBody (DCert, EraIndependentTxBody, TransTxId)

data LEDGER era

data LedgerEnv era = LedgerEnv
  { ledgerSlotNo :: SlotNo,
    ledgerIx :: Ix,
    ledgerPp :: Core.PParams era,
    ledgerAccount :: AccountState
  }

deriving instance Show (Core.PParams era) => Show (LedgerEnv era)

data LedgerPredicateFailure era
  = UtxowFailure (PredicateFailure (Core.EraRule "UTXOW" era)) -- Subtransition Failures
  | DelegsFailure (PredicateFailure (Core.EraRule "DELEGS" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "DELEGS" era)),
    Show (PredicateFailure (Core.EraRule "UTXOW" era)),
    Era era
  ) =>
  Show (LedgerPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "DELEGS" era)),
    Eq (PredicateFailure (Core.EraRule "UTXOW" era)),
    Era era
  ) =>
  Eq (LedgerPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (Core.EraRule "DELEGS" era)),
    NoThunks (PredicateFailure (Core.EraRule "UTXOW" era)),
    Era era
  ) =>
  NoThunks (LedgerPredicateFailure era)

instance
  ( ToCBOR (PredicateFailure (Core.EraRule "DELEGS" era)),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOW" era)),
    Era era
  ) =>
  ToCBOR (LedgerPredicateFailure era)
  where
  toCBOR = \case
    (UtxowFailure a) -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR a
    (DelegsFailure a) -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR a

instance
  ( FromCBOR (PredicateFailure (Core.EraRule "DELEGS" era)),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOW" era)),
    Era era
  ) =>
  FromCBOR (LedgerPredicateFailure era)
  where
  fromCBOR =
    decodeRecordSum "PredicateFailure (LEDGER era)" $
      ( \case
          0 -> do
            a <- fromCBOR
            pure (2, UtxowFailure a)
          1 -> do
            a <- fromCBOR
            pure (2, DelegsFailure a)
          k -> invalidKey k
      )

instance
  ( UsesValue era,
    UsesScript era,
    UsesTxBody era,
    UsesTxOut era,
    UsesAuxiliary era,
    UsesPParams era,
    TransTxId Show era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    Show (UTxOState era)
  ) =>
  STS (LEDGER era)
  where
  type
    State (LEDGER era) =
      (UTxOState era, DPState (Crypto era))
  type Signal (LEDGER era) = Tx era
  type Environment (LEDGER era) = LedgerEnv era
  type BaseM (LEDGER era) = ShelleyBase
  type PredicateFailure (LEDGER era) = LedgerPredicateFailure era

  initialRules = []
  transitionRules = [ledgerTransition]

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

ledgerTransition ::
  forall era.
  ( UsesScript era,
    UsesTxBody era,
    UsesAuxiliary era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  TransitionRule (LEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, (utxoSt, dpstate), tx) <- judgmentContext

  dpstate' <-
    trans @(Core.EraRule "DELEGS" era) $
      TRC
        ( DelegsEnv slot txIx pp tx account,
          dpstate,
          StrictSeq.fromStrict $ getField @"certs" $ _body tx
        )

  let DPState dstate pstate = dpstate
      genDelegs = _genDelegs dstate
      stpools = _pParams pstate

  utxoSt' <-
    trans @(Core.EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv slot pp stpools genDelegs,
          utxoSt,
          tx
        )
  pure (utxoSt', dpstate')

instance
  ( Era era,
    STS (DELEGS era),
    PredicateFailure (Core.EraRule "DELEGS" era) ~ DelegsPredicateFailure era
  ) =>
  Embed (DELEGS era) (LEDGER era)
  where
  wrapFailed = DelegsFailure

instance
  ( Era era,
    STS (UTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ UtxowPredicateFailure era
  ) =>
  Embed (UTXOW era) (LEDGER era)
  where
  wrapFailed = UtxowFailure
