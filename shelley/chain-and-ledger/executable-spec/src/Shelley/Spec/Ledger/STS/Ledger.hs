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
    decodeListLen,
    decodeWord,
    encodeListLen,
    matchSize,
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
import Data.Typeable (Typeable)
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
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..), TxBody (..))

data LEDGER crypto

data LedgerEnv = LedgerEnv
  { ledgerSlotNo :: SlotNo,
    ledgerIx :: Ix,
    ledgerPp :: PParams,
    ledgerAccount :: AccountState
  }
  deriving (Show)

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  STS (LEDGER crypto)
  where
  type
    State (LEDGER crypto) =
      (UTxOState crypto, DPState crypto)
  type Signal (LEDGER crypto) = Tx crypto
  type Environment (LEDGER crypto) = LedgerEnv
  type BaseM (LEDGER crypto) = ShelleyBase
  data PredicateFailure (LEDGER crypto)
    = UtxowFailure (PredicateFailure (UTXOW crypto)) -- Subtransition Failures
    | DelegsFailure (PredicateFailure (DELEGS crypto)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [ledgerTransition]

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (LEDGER crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (LEDGER crypto))
  where
  toCBOR = \case
    (UtxowFailure a) -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR a
    (DelegsFailure a) -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR a

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (LEDGER crypto))
  where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "UtxowFailure" 2 n
        a <- fromCBOR
        pure $ UtxowFailure a
      1 -> do
        matchSize "DelegsFailure" 2 n
        a <- fromCBOR
        pure $ DelegsFailure a
      k -> invalidKey k

ledgerTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  TransitionRule (LEDGER crypto)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, (utxoSt, dpstate), tx) <- judgmentContext

  dpstate' <-
    trans @(DELEGS crypto) $
      TRC (DelegsEnv slot txIx pp tx account, dpstate, StrictSeq.getSeq $ _certs $ _body tx)

  let DPState dstate pstate = dpstate
      DState stkCreds _ _ _ _ genDelegs _ = dstate
      PState stpools _ _ _ = pstate

  utxoSt' <-
    trans @(UTXOW crypto) $
      TRC
        ( UtxoEnv slot pp stkCreds stpools genDelegs,
          utxoSt,
          tx
        )
  pure (utxoSt', dpstate')

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Embed (DELEGS crypto) (LEDGER crypto)
  where
  wrapFailed = DelegsFailure

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Embed (UTXOW crypto) (LEDGER crypto)
  where
  wrapFailed = UtxowFailure
