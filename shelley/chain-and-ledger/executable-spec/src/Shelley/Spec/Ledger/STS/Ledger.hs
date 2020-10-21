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
import Cardano.Ledger.Shelley (ShelleyBased)
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
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Delegs (DELEGS, DelegsEnv (..))
import Shelley.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Shelley.Spec.Ledger.STS.Utxow (UTXOW)
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxBody (DCert, EraIndependentTxBody)

data LEDGER era

data LedgerEnv era = LedgerEnv
  { ledgerSlotNo :: SlotNo,
    ledgerIx :: Ix,
    ledgerPp :: (PParams era),
    ledgerAccount :: AccountState
  }
  deriving (Show)

data LedgerPredicateFailure era
  = UtxowFailure (PredicateFailure (UTXOW era)) -- Subtransition Failures
  | DelegsFailure (PredicateFailure (DELEGS era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Show (PredicateFailure (DELEGS era)),
    Show (PredicateFailure (UTXOW era)),
    ShelleyBased era
  ) =>
  Show (LedgerPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (DELEGS era)),
    Eq (PredicateFailure (UTXOW era)),
    ShelleyBased era
  ) =>
  Eq (LedgerPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (DELEGS era)),
    NoThunks (PredicateFailure (UTXOW era)),
    ShelleyBased era
  ) =>
  NoThunks (LedgerPredicateFailure era)

instance
  ( ToCBOR (PredicateFailure (DELEGS era)),
    ToCBOR (PredicateFailure (UTXOW era)),
    ShelleyBased era
  ) =>
  ToCBOR (LedgerPredicateFailure era)
  where
  toCBOR = \case
    (UtxowFailure a) -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR a
    (DelegsFailure a) -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR a

instance
  ( FromCBOR (PredicateFailure (DELEGS era)),
    FromCBOR (PredicateFailure (UTXOW era)),
    ShelleyBased era
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
  ( Era era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    ShelleyBased era,
    Embed (DELEGS era) (LEDGER era),
    Embed (UTXOW era) (LEDGER era),
    Environment (UTXOW era) ~ UtxoEnv era,
    State (UTXOW era) ~ UTxOState era,
    Signal (UTXOW era) ~ Tx era,
    Environment (DELEGS era) ~ DelegsEnv era,
    State (DELEGS era) ~ DPState era,
    Signal (DELEGS era) ~ Seq (DCert era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert era))
  ) =>
  STS (LEDGER era)
  where
  type
    State (LEDGER era) =
      (UTxOState era, DPState era)
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
  ( ShelleyBased era,
    Embed (DELEGS era) (LEDGER era),
    Embed (UTXOW era) (LEDGER era),
    Environment (UTXOW era) ~ UtxoEnv era,
    State (UTXOW era) ~ UTxOState era,
    Signal (UTXOW era) ~ Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert era))
  ) =>
  TransitionRule (LEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, (utxoSt, dpstate), tx) <- judgmentContext

  dpstate' <-
    trans @(DELEGS era) $
      TRC
        ( DelegsEnv slot txIx pp tx account,
          dpstate,
          StrictSeq.getSeq $ getField @"certs" $ _body tx
        )

  let DPState dstate pstate = dpstate
      genDelegs = _genDelegs dstate
      stpools = _pParams pstate

  utxoSt' <-
    trans @(UTXOW era) $
      TRC
        ( UtxoEnv slot pp stpools genDelegs,
          utxoSt,
          tx
        )
  pure (utxoSt', dpstate')

instance
  ( ShelleyBased era,
    STS (DELEGS era)
  ) =>
  Embed (DELEGS era) (LEDGER era)
  where
  wrapFailed = DelegsFailure

instance
  ( ShelleyBased era,
    STS (UTXOW era),
    BaseM (UTXOW era) ~ ShelleyBase
  ) =>
  Embed (UTXOW era) (LEDGER era)
  where
  wrapFailed = UtxowFailure
