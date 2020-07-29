{-# LANGUAGE DataKinds #-}
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
  ( UTXO,
    UtxoEnv (..),
    PredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Prelude (NoUnexpectedThunks (..), asks)
import Control.Iterate.SetAlgebra (dom, eval, rng, (∪), (⊆), (⋪))
import Control.State.Transition
  ( Assertion (..),
    Embed,
    IRC (..),
    InitialRule,
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
    wrapFailed,
    (?!),
  )
import Data.Foldable (foldl', toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address
  ( Addr (AddrBootstrap),
    bootstrapAddressAttrsSize,
    getNetwork,
    getRwdNetwork,
  )
import Shelley.Spec.Ledger.BaseTypes (Network, ShelleyBase, invalidKey, networkId)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( UTxOState (..),
    consumed,
    emptyPPUPState,
    keyRefunds,
    minfee,
    produced,
    txsize,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.STS.Ppup (PPUP, PPUPEnv (..))
import Shelley.Spec.Ledger.Serialization
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
  )
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..), TxIn, TxOut (..))
import Shelley.Spec.Ledger.TxData
  ( PoolParams,
    RewardAcnt,
    TxBody (..),
    unWdrl,
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    totalDeposits,
    txCreatesNoScriptAddrs,
    txins,
    txouts,
    txup,
  )

data UTXO crypto

data UtxoEnv crypto
  = UtxoEnv
      SlotNo
      PParams
      (Map (KeyHash 'StakePool crypto) (PoolParams crypto))
      (GenDelegs crypto)
  deriving (Show)

instance
  Crypto crypto =>
  STS (UTXO crypto)
  where
  type State (UTXO crypto) = UTxOState crypto
  type Signal (UTXO crypto) = Tx crypto
  type Environment (UTXO crypto) = UtxoEnv crypto
  type BaseM (UTXO crypto) = ShelleyBase
  data PredicateFailure (UTXO crypto)
    = BadInputsUTxO
        !(Set (TxIn crypto)) -- The bad transaction inputs
    | ExpiredUTxO
        !SlotNo -- transaction's time to live
        !SlotNo -- current slot
    | MaxTxSizeUTxO
        !Integer -- the actual transaction size
        !Integer -- the max transaction size
    | InputSetEmptyUTxO
    | FeeTooSmallUTxO
        !Coin -- the minimum fee for this transaction
        !Coin -- the fee supplied in this transaction
    | ValueNotConservedUTxO
        !Coin -- the Coin consumed by this transaction
        !Coin -- the Coin produced by this transaction
    | WrongNetwork
        !Network -- the expected network id
        !(Set (Addr crypto)) -- the set of addresses with incorrect network IDs
    | WrongNetworkWithdrawal
        !Network -- the expected network id
        !(Set (RewardAcnt crypto)) -- the set of reward addresses with incorrect network IDs
    | OutputTooSmallUTxO
        ![TxOut crypto] -- list of supplied transaction outputs that are too small
    | UpdateFailure (PredicateFailure (PPUP crypto)) -- Subtransition Failures
    | OutputBootAddrAttrsTooBig
        ![TxOut crypto] -- list of supplied bad transaction outputs
    | ScriptsEmbargoed -- blocking use of scripts for the moment
    deriving (Eq, Show, Generic)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

  assertions =
    [ PostCondition
        "UTxO must increase fee pot"
        (\(TRC (_, st, _)) st' -> _fees st' >= _fees st),
      PostCondition
        "Deposit pot must not be negative"
        (\_ st' -> _deposited st' >= 0),
      let utxoBalance us = _deposited us + _fees us + balance (_utxo us)
          withdrawals txb = foldl' (+) (Coin 0) $ unWdrl $ _wdrls txb
       in PostCondition
            "Should preserve ADA in the UTxO state"
            ( \(TRC (_, us, tx)) us' ->
                utxoBalance us + withdrawals (_body tx) == utxoBalance us'
            )
    ]

instance NoUnexpectedThunks (PredicateFailure (UTXO crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (UTXO crypto))
  where
  toCBOR = \case
    BadInputsUTxO ins ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> encodeFoldable ins
    (ExpiredUTxO a b) ->
      encodeListLen 3 <> toCBOR (1 :: Word8)
        <> toCBOR a
        <> toCBOR b
    (MaxTxSizeUTxO a b) ->
      encodeListLen 3 <> toCBOR (2 :: Word8)
        <> toCBOR a
        <> toCBOR b
    InputSetEmptyUTxO -> encodeListLen 1 <> toCBOR (3 :: Word8)
    (FeeTooSmallUTxO a b) ->
      encodeListLen 3 <> toCBOR (4 :: Word8)
        <> toCBOR a
        <> toCBOR b
    (ValueNotConservedUTxO a b) ->
      encodeListLen 3 <> toCBOR (5 :: Word8)
        <> toCBOR a
        <> toCBOR b
    OutputTooSmallUTxO outs ->
      encodeListLen 2 <> toCBOR (6 :: Word8)
        <> encodeFoldable outs
    (UpdateFailure a) ->
      encodeListLen 2 <> toCBOR (7 :: Word8)
        <> toCBOR a
    (WrongNetwork right wrongs) ->
      encodeListLen 3 <> toCBOR (8 :: Word8)
        <> toCBOR right
        <> encodeFoldable wrongs
    (WrongNetworkWithdrawal right wrongs) ->
      encodeListLen 3 <> toCBOR (9 :: Word8)
        <> toCBOR right
        <> encodeFoldable wrongs
    OutputBootAddrAttrsTooBig outs ->
      encodeListLen 2 <> toCBOR (10 :: Word8)
        <> encodeFoldable outs
    ScriptsEmbargoed ->
      encodeListLen 1 <> toCBOR (11 :: Word8)

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (UTXO crypto))
  where
  fromCBOR =
    decodeRecordSum "PredicateFailureUTXO" $
      \case
        0 -> do
          ins <- decodeSet fromCBOR
          pure (2, BadInputsUTxO ins) -- The (2,..) indicates the number of things decoded, INCLUDING the tags, which are decoded by decodeRecordSumNamed
        1 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, ExpiredUTxO a b)
        2 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, MaxTxSizeUTxO a b)
        3 -> pure (1, InputSetEmptyUTxO)
        4 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, FeeTooSmallUTxO a b)
        5 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, ValueNotConservedUTxO a b)
        6 -> do
          outs <- decodeList fromCBOR
          pure (2, OutputTooSmallUTxO outs)
        7 -> do
          a <- fromCBOR
          pure (2, UpdateFailure a)
        8 -> do
          right <- fromCBOR
          wrongs <- decodeSet fromCBOR
          pure (3, WrongNetwork right wrongs)
        9 -> do
          right <- fromCBOR
          wrongs <- decodeSet fromCBOR
          pure (3, WrongNetworkWithdrawal right wrongs)
        10 -> do
          outs <- decodeList fromCBOR
          pure (2, OutputBootAddrAttrsTooBig outs)
        11 ->
          pure (1, ScriptsEmbargoed)
        k -> invalidKey k

initialLedgerState :: InitialRule (UTXO crypto)
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPUPState

utxoInductive ::
  forall crypto.
  Crypto crypto =>
  TransitionRule (UTXO crypto)
utxoInductive = do
  TRC (UtxoEnv slot pp stakepools genDelegs, u, tx) <- judgmentContext
  let UTxOState utxo deposits' fees ppup = u
  let txb = _body tx

  _ttl txb >= slot ?! ExpiredUTxO (_ttl txb) slot

  txins txb /= Set.empty ?! InputSetEmptyUTxO

  let minFee = minfee pp tx
      txFee = _txfee txb
  minFee <= txFee ?! FeeTooSmallUTxO minFee txFee

  eval (txins txb ⊆ dom utxo) ?! BadInputsUTxO (txins txb `Set.difference` eval (dom utxo))

  ni <- liftSTS $ asks networkId
  let addrsWrongNetwork =
        filter
          (\a -> getNetwork a /= ni)
          (fmap (\(TxOut a _) -> a) $ toList $ _outputs txb)
  null addrsWrongNetwork ?! WrongNetwork ni (Set.fromList addrsWrongNetwork)
  let wdrlsWrongNetwork =
        filter
          (\a -> getRwdNetwork a /= ni)
          (Map.keys . unWdrl . _wdrls $ txb)
  null wdrlsWrongNetwork ?! WrongNetworkWithdrawal ni (Set.fromList wdrlsWrongNetwork)

  let consumed_ = consumed pp utxo txb
      produced_ = produced pp stakepools txb
  consumed_ == produced_ ?! ValueNotConservedUTxO consumed_ produced_

  -- process Protocol Parameter Update Proposals
  ppup' <- trans @(PPUP crypto) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  let outputs = Set.toList (eval (rng (txouts txb)))
      minUTxOValue = _minUTxOValue pp
      outputsTooSmall = [out | out@(TxOut _ c) <- outputs, c < minUTxOValue]
  null outputsTooSmall ?! OutputTooSmallUTxO outputsTooSmall

  -- Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
  -- It is important to limit their overall size.
  let outputsAttrsTooBig =
        [out | out@(TxOut (AddrBootstrap addr) _) <- outputs, bootstrapAddressAttrsSize addr > 64]
  null outputsAttrsTooBig ?! OutputBootAddrAttrsTooBig outputsAttrsTooBig

  -- Block use of script addresses until we fix the ScriptHash size mismatch.
  txCreatesNoScriptAddrs txb ?! ScriptsEmbargoed

  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = txsize tx
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = keyRefunds pp txb
  let txCerts = toList $ _certs txb
  let depositChange = totalDeposits pp stakepools txCerts - refunded

  pure
    UTxOState
      { _utxo = eval ((txins txb ⋪ utxo) ∪ txouts txb),
        _deposited = deposits' + depositChange,
        _fees = fees + (_txfee txb),
        _ppups = ppup'
      }

instance
  Crypto crypto =>
  Embed (PPUP crypto) (UTXO crypto)
  where
  wrapFailed = UpdateFailure
