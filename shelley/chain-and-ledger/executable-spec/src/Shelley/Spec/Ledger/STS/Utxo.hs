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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Utxo
  ( UTXO,
    UtxoEnv (..),
    UtxoPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (∪), (⊆), (⋪))
import Control.State.Transition
  ( Assertion (..),
    AssertionViolation (..),
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
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Address
  ( Addr (AddrBootstrap),
    bootstrapAddressAttrsSize,
    getNetwork,
    getRwdNetwork,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network,
    ShelleyBase,
    StrictMaybe,
    invalidKey,
    networkId,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
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
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), Update)
import Shelley.Spec.Ledger.STS.Ppup (PPUP, PPUPEnv (..))
import Shelley.Spec.Ledger.Serialization
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
  )
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..), TxIn, TxOut (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert,
    PoolParams,
    RewardAcnt,
    TxBody (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    totalDeposits,
    txins,
    txouts,
    txup,
  )

data UTXO era

data UtxoEnv era
  = UtxoEnv
      SlotNo
      (PParams era)
      (Map (KeyHash 'StakePool (Crypto era)) (PoolParams era))
      (GenDelegs (Crypto era))
  deriving (Show)

data UtxoPredicateFailure era
  = BadInputsUTxO
      !(Set (TxIn era)) -- The bad transaction inputs
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
      !(Delta (Core.Value era)) -- the Coin consumed by this transaction
      !(Delta (Core.Value era)) -- the Coin produced by this transaction
  | WrongNetwork
      !Network -- the expected network id
      !(Set (Addr era)) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network -- the expected network id
      !(Set (RewardAcnt era)) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure (PredicateFailure (PPUP era)) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

deriving stock instance
  ShelleyBased era =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  ShelleyBased era =>
  Eq (UtxoPredicateFailure era)

instance NoThunks (Delta (Core.Value era)) => NoThunks (UtxoPredicateFailure era)

instance
  ShelleyBased era =>
  ToCBOR (UtxoPredicateFailure era)
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

instance
  ShelleyBased era =>
  FromCBOR (UtxoPredicateFailure era)
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
        k -> invalidKey k

instance
  (CryptoClass.Crypto c, Core.TxBody (ShelleyEra c) ~ TxBody (ShelleyEra c)) =>
  STS (UTXO (ShelleyEra c))
  where
  type State (UTXO (ShelleyEra c)) = UTxOState (ShelleyEra c)
  type Signal (UTXO (ShelleyEra c)) = Tx (ShelleyEra c)
  type Environment (UTXO (ShelleyEra c)) = UtxoEnv (ShelleyEra c)
  type BaseM (UTXO (ShelleyEra c)) = ShelleyBase
  type PredicateFailure (UTXO (ShelleyEra c)) = UtxoPredicateFailure (ShelleyEra c)

  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

  renderAssertionViolation AssertionViolation {avSTS, avMsg, avCtx, avState} =
    "AssertionViolation (" <> avSTS <> "): " <> avMsg
      <> "\n"
      <> show avCtx
      <> "\n"
      <> show avState

  assertions =
    [ PreCondition
        "Deposit pot must not be negative (pre)"
        (\(TRC (_, st, _)) -> _deposited st >= mempty),
      PostCondition
        "UTxO must increase fee pot"
        (\(TRC (_, st, _)) st' -> _fees st' >= _fees st),
      PostCondition
        "Deposit pot must not be negative (post)"
        (\_ st' -> _deposited st' >= mempty),
      let utxoBalance us = (Val.inject $ _deposited us <> _fees us) <> balance (_utxo us)
          withdrawals :: TxBody (ShelleyEra c) -> Core.Value (ShelleyEra c)
          withdrawals txb = Val.inject $ foldl' (<>) mempty $ unWdrl $ _wdrls txb
       in PostCondition
            "Should preserve value in the UTxO state"
            ( \(TRC (_, us, tx)) us' ->
                utxoBalance us <> withdrawals (_body tx) == utxoBalance us'
            )
    ]

initialLedgerState :: InitialRule (UTXO (ShelleyEra c))
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPUPState

utxoInductive ::
  forall era.
  ( ShelleyBased era,
    STS (UTXO era),
    Embed (PPUP era) (UTXO era),
    BaseM (UTXO era) ~ ShelleyBase,
    Environment (UTXO era) ~ UtxoEnv era,
    State (UTXO era) ~ UTxOState era,
    Signal (UTXO era) ~ Tx era,
    PredicateFailure (UTXO era) ~ UtxoPredicateFailure era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn era)),
    HasField "outputs" (Core.TxBody era) (StrictSeq (TxOut era)),
    HasField "wdrls" (Core.TxBody era) (Wdrl era),
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "ttl" (Core.TxBody era) SlotNo,
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  TransitionRule (UTXO era)
utxoInductive = do
  TRC (UtxoEnv slot pp stakepools genDelegs, u, tx) <- judgmentContext
  let UTxOState utxo deposits' fees ppup = u
  let txb = _body tx

  getField @"ttl" txb >= slot ?! ExpiredUTxO (getField @"ttl" txb) slot

  txins txb /= Set.empty ?! InputSetEmptyUTxO

  let minFee = minfee pp tx
      txFee = getField @"txfee" txb
  minFee <= txFee ?! FeeTooSmallUTxO minFee txFee

  eval (txins txb ⊆ dom utxo)
    ?! BadInputsUTxO (Set.filter (\x -> not (Map.member x (unUTxO utxo))) (txins txb))

  ni <- liftSTS $ asks networkId
  let addrsWrongNetwork =
        filter
          (\a -> getNetwork a /= ni)
          (fmap (\(TxOut a _) -> a) $ toList $ getField @"outputs" txb)
  null addrsWrongNetwork ?! WrongNetwork ni (Set.fromList addrsWrongNetwork)
  let wdrlsWrongNetwork =
        filter
          (\a -> getRwdNetwork a /= ni)
          (Map.keys . unWdrl . getField @"wdrls" $ txb)
  null wdrlsWrongNetwork ?! WrongNetworkWithdrawal ni (Set.fromList wdrlsWrongNetwork)

  let consumed_ = consumed pp utxo txb
      produced_ = produced pp stakepools txb
  consumed_ == produced_ ?! ValueNotConservedUTxO (toDelta consumed_) (toDelta produced_)

  -- process Protocol Parameter Update Proposals
  ppup' <- trans @(PPUP era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  let outputs = Map.elems $ unUTxO (txouts txb)
      minUTxOValue = _minUTxOValue pp
      -- minUTxOValue deposit comparison done as Coin because this rule
      -- is correct strictly in the Shelley era (in shelleyMA we would need to
      -- additionally check that all amounts are non-negative)
      outputsTooSmall = [out | out@(TxOut _ c) <- outputs, (Val.coin c) < (Val.scaledMinDeposit c minUTxOValue)]
  null outputsTooSmall ?! OutputTooSmallUTxO outputsTooSmall

  -- Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
  -- It is important to limit their overall size.
  let outputsAttrsTooBig =
        [out | out@(TxOut (AddrBootstrap addr) _) <- outputs, bootstrapAddressAttrsSize addr > 64]
  null outputsAttrsTooBig ?! OutputBootAddrAttrsTooBig outputsAttrsTooBig

  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = txsize tx
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = keyRefunds pp txb
  let txCerts = toList $ getField @"certs" txb
  let depositChange = totalDeposits pp stakepools txCerts <-> refunded

  pure
    UTxOState
      { _utxo = eval ((txins txb ⋪ utxo) ∪ txouts txb),
        _deposited = deposits' <> depositChange,
        _fees = fees <> (getField @"txfee" txb),
        _ppups = ppup'
      }

instance
  (CryptoClass.Crypto c) =>
  Embed (PPUP (ShelleyEra c)) (UTXO (ShelleyEra c))
  where
  wrapFailed = UpdateFailure
