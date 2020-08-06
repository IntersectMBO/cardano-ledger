{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Ledger
  ( LEDGER,
    LedgerEnv (..),
    PredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState (..),
    DState (..),
    Ix,
    PState (..),
    UTxOState,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Delegs (DELEGS, DelegsEnv (..))
import Shelley.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
    pattern BadInputsUTxO,
    pattern ExpiredUTxO,
    pattern FeeTooSmallUTxO,
    pattern InputSetEmptyUTxO,
    pattern MaxTxSizeUTxO,
    pattern OutputTooSmallUTxO,
    pattern UpdateFailure,
    pattern ValueNotConservedUTxO,
  )
import Shelley.Spec.Ledger.STS.Utxow (PredicateFailure (..), UTXOW)
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..), TxBody (..))
import Shelley.Spec.Ledger.Value

data LEDGER crypto v

data LedgerEnv = LedgerEnv
  { ledgerSlotNo :: SlotNo,
    ledgerIx :: Ix,
    ledgerPp :: PParams,
    ledgerAccount :: AccountState
  }
  deriving (Show)

instance
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  STS (LEDGER crypto v)
  where
  type
    State (LEDGER crypto v) =
      (UTxOState crypto v, DPState crypto)
  type Signal (LEDGER crypto v) = Tx crypto v
  type Environment (LEDGER crypto v) = LedgerEnv
  type BaseM (LEDGER crypto v) = ShelleyBase
  data PredicateFailure (LEDGER crypto v)
    = UtxowFailure (PredicateFailure (UTXOW crypto v)) -- Subtransition Failures
    | DelegsFailure (PredicateFailure (DELEGS crypto v)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [ledgerTransition]

instance (CV crypto v) => NoUnexpectedThunks (PredicateFailure (LEDGER crypto v))

instance
  (CV crypto v) =>
  ToCBOR (PredicateFailure (LEDGER crypto v))
  where
  toCBOR = \case
    (UtxowFailure a) -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR a
    (DelegsFailure a) -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR a

instance
  (CV crypto v) =>
  FromCBOR (PredicateFailure (LEDGER crypto v))
  where
  fromCBOR =
    decodeRecordSum "PredicateFailure (LEDGER crypto v)" $
      ( \case
          0 -> do
            a <- fromCBOR
            pure (2, UtxowFailure a)
          1 -> do
            a <- fromCBOR
            pure (2, DelegsFailure a)
          k -> invalidKey k
      )

ledgerTransition ::
  forall crypto v.
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  TransitionRule (LEDGER crypto v)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, (utxoSt, dpstate), tx) <- judgmentContext

  dpstate' <-
    trans @(DELEGS crypto v) $
      TRC
        ( DelegsEnv slot txIx pp tx account,
          dpstate,
          StrictSeq.getSeq $ _certs $ _body tx
        )

  let DPState dstate pstate = dpstate
      genDelegs = _genDelegs dstate
      stpools = _pParams pstate

  utxoSt' <-
    trans @(UTXOW crypto v) $
      TRC
        ( UtxoEnv slot pp stpools genDelegs,
          utxoSt,
          tx
        )
  pure (utxoSt', dpstate')

instance
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  Embed (DELEGS crypto v) (LEDGER crypto v)
  where
  wrapFailed = DelegsFailure

instance
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  Embed (UTXOW crypto v) (LEDGER crypto v)
  where
  wrapFailed = UtxowFailure
