{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Utxo
  ( UTXO
  , UtxoEnv (..)
  , PredicateFailure(..)
  )
where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.State.Transition
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Ledger.Core (dom, range, (∪), (⊆), (⋪))
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.Coin
import           Shelley.Spec.Ledger.Delegation.Certificates
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.LedgerState (UTxOState (..), consumed, decayedTx, keyRefunds,
                     minfee, produced, txsize)
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Slot
import           Shelley.Spec.Ledger.STS.Up
import           Shelley.Spec.Ledger.Tx
import           Shelley.Spec.Ledger.TxData (getValue)
import           Shelley.Spec.Ledger.Updates (emptyUpdateState)
import           Shelley.Spec.Ledger.UTxO
import           Shelley.Spec.Ledger.Value

data UTXO crypto

data UtxoEnv crypto
  = UtxoEnv
      SlotNo
      PParams
      (StakeCreds crypto)
      (StakePools crypto)
      (GenDelegs crypto)
      deriving(Show)

instance
  Crypto crypto
  => STS (UTXO crypto)
 where
  type State (UTXO crypto) = UTxOState crypto
  type Signal (UTXO crypto) = Tx crypto
  type Environment (UTXO crypto) = UtxoEnv crypto
  type BaseM (UTXO crypto) = ShelleyBase
  data PredicateFailure (UTXO crypto)
    = BadInputsUTxO
    | ExpiredUTxO SlotNo SlotNo
    | MaxTxSizeUTxO Integer Integer
    | InputSetEmptyUTxO
    | FeeTooSmallUTxO Coin Coin
    | ValueNotConservedUTxO ValueBSType ValueBSType
    | NegativeOutputsUTxO
    | ForgingAda
    | UpdateFailure (PredicateFailure (UP crypto))
    deriving (Eq, Show, Generic)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

instance NoUnexpectedThunks (PredicateFailure (UTXO crypto))

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (UTXO crypto))
 where
   toCBOR = \case
     BadInputsUTxO               -> encodeListLen 1 <> toCBOR (0 :: Word8)
     (ExpiredUTxO a b)           -> encodeListLen 3 <> toCBOR (1 :: Word8)
                                      <> toCBOR a <> toCBOR b
     (MaxTxSizeUTxO a b)         -> encodeListLen 3 <> toCBOR (2 :: Word8)
                                      <> toCBOR a <> toCBOR b
     InputSetEmptyUTxO           -> encodeListLen 1 <> toCBOR (3 :: Word8)
     (FeeTooSmallUTxO a b)       -> encodeListLen 3 <> toCBOR (4 :: Word8)
                                      <> toCBOR a <> toCBOR b
     (ValueNotConservedUTxO a b) -> encodeListLen 3 <> toCBOR (5 :: Word8)
                                      <> toCBOR a <> toCBOR b
     NegativeOutputsUTxO         -> encodeListLen 1 <> toCBOR (6 :: Word8)
     ForgingAda                  -> encodeListLen 1 <> toCBOR (7 :: Word8)
     (UpdateFailure a)           -> encodeListLen 2 <> toCBOR (8 :: Word8)
                                      <> toCBOR a

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (UTXO crypto))
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "BadInputsUTxO" 1 n >> pure BadInputsUTxO
      1 -> do
        matchSize "ExpiredUTxO" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ ExpiredUTxO a b
      2 -> do
        matchSize "MaxTxSizeUTxO" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ MaxTxSizeUTxO a b
      3 -> matchSize "InputSetEmptyUTxO" 1 n >> pure InputSetEmptyUTxO
      4 -> do
        matchSize "FeeTooSmallUTxO" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ FeeTooSmallUTxO a b
      5 -> do
        matchSize "ValueNotConservedUTxO" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ ValueNotConservedUTxO a b
      6 -> matchSize "NegativeOutputsUTxO" 1 n >> pure NegativeOutputsUTxO
      7 -> matchSize "ForgingAda" 1 n >> pure ForgingAda
      8 -> do
        matchSize "UpdateFailure" 2 n
        a <- fromCBOR
        pure $ UpdateFailure a
      k -> invalidKey k

initialLedgerState :: InitialRule (UTXO crypto)
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

utxoInductive
  :: forall crypto
   . Crypto crypto
  => TransitionRule (UTXO crypto)
utxoInductive = do
  TRC (UtxoEnv slot pp stakeCreds stakepools genDelegs, u, tx) <- judgmentContext
  let UTxOState utxo deposits' fees ups = u
  let txb = _body tx

  _ttl txb >= slot ?! ExpiredUTxO (_ttl txb) slot

  txins txb /= Set.empty ?! InputSetEmptyUTxO

  let minFee = minfee pp tx
      txFee  = _txfee txb
  minFee <= txFee ?! FeeTooSmallUTxO minFee txFee

  txins txb ⊆ dom utxo ?! BadInputsUTxO

  let consumed_ = consumed pp utxo stakeCreds txb
      produced_ = produced pp stakepools txb
  consumed_ == produced_ ?! ValueNotConservedUTxO (toValBST consumed_) (toValBST produced_)

  -- process Update Proposals
  ups' <- trans @(UP crypto) $ TRC (UpdateEnv slot pp genDelegs, ups, txup tx)

  let outputValues = [getValue utxoout | utxoout <- Set.toList (range (txouts txb))]
  all (zeroV <=) outputValues ?! NegativeOutputsUTxO

  let (Value vls) = _forge txb
  let cids = Map.keys vls
  all (adaID /=) cids  ?! ForgingAda


  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = txsize tx
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = keyRefunds pp stakeCreds txb
  decayed <- liftSTS $ decayedTx pp stakeCreds txb
  let txCerts = toList $ _certs txb
  let depositChange = totalDeposits pp stakepools txCerts - (refunded + decayed)

  pure UTxOState
        { _utxo      = (txins txb ⋪ utxo) ∪ txouts txb
        , _deposited = deposits' + depositChange
        , _fees      = fees + (_txfee txb) + decayed
        , _ups       = ups'
        }

instance Crypto crypto
  => Embed (UP crypto) (UTXO crypto)
 where
  wrapFailed = UpdateFailure
