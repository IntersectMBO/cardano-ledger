{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Cardano.Ledger.Shelley.Rules.Utxo
  ( UTXO,
    UtxoEnv (..),
    UtxoPredicateFailure (..),
    UtxoEvent (..),
    PredicateFailure,
    updateUTxOState,
    -- * Validations
    validateInputSetEmptyUTxO,
    validateFeeTooSmallUTxO,
    validateBadInputsUTxO,
    validateWrongNetwork,
    validateWrongNetworkWithdrawal,
    validateOutputBootAddrAttrsTooBig,
    validateMaxTxSizeUTxO,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Address
  ( Addr (..),
    bootstrapAddressAttrsSize,
    getNetwork,
    getRwdNetwork,
  )
import Cardano.Ledger.BaseTypes
  ( Network,
    ShelleyBase,
    StrictMaybe,
    invalidKey,
    networkId,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..), getTxOutBootstrapAddress)
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (runValidation)
import Cardano.Ledger.Serialization
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
  )
import Cardano.Ledger.Shelley.Constraints
  ( TransValue,
    UsesAuxiliary,
    UsesPParams,
    UsesScript,
    UsesTxOut,
    UsesValue,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( PPUPState,
    TransUTxOState,
    UTxOState (..),
    consumed,
    keyRefunds,
    minfee,
    produced,
    updateStakeDistribution,
  )
import Cardano.Ledger.Shelley.PParams (PParams, PParams' (..), Update)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP, PPUPEnv (..), PpupEvent, PpupPredicateFailure)
import Cardano.Ledger.Shelley.Tx (Tx (..), TxIn, TxOut (..))
import Cardano.Ledger.Shelley.TxBody
  ( DCert,
    PoolParams,
    RewardAcnt,
    TxBody (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO
  ( TransUTxO,
    UTxO (..),
    balance,
    totalDeposits,
    txouts,
    txup,
  )
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Assertion (..),
    AssertionViolation (..),
    Embed,
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    tellEvent,
    trans,
    wrapEvent,
    wrapFailed,
  )
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Validation

data UTXO era

data UtxoEnv era
  = UtxoEnv
      SlotNo
      (Core.PParams era)
      (Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era)))
      (GenDelegs (Crypto era))

deriving instance Show (Core.PParams era) => Show (UtxoEnv era)

data UtxoEvent era
  = TotalDeposits Coin
  | UpdateEvent (Event (Core.EraRule "PPUP" era))

data UtxoPredicateFailure era
  = BadInputsUTxO
      !(Set (TxIn (Crypto era))) -- The bad transaction inputs
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
      !(Core.Value era) -- the Coin consumed by this transaction
      !(Core.Value era) -- the Coin produced by this transaction
  | WrongNetwork
      !Network -- the expected network id
      !(Set (Addr (Crypto era))) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network -- the expected network id
      !(Set (RewardAcnt (Crypto era))) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![Core.TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure (PredicateFailure (Core.EraRule "PPUP" era)) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![Core.TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

deriving stock instance
  ( UsesValue era,
    TransUTxOState Show era,
    Show (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  ( TransUTxOState Eq era,
    TransValue Eq era,
    Eq (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Eq (UtxoPredicateFailure era)

instance
  ( TransUTxOState NoThunks era,
    NoThunks (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  NoThunks (UtxoPredicateFailure era)

instance
  ( TransUTxOState ToCBOR era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
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
  ( TransValue FromCBOR era,
    TransUTxO FromCBOR era,
    Val.DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
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
  ( UsesTxOut era,
    Core.TxOut era ~ TxOut era,
    UsesValue era,
    UsesScript era,
    UsesAuxiliary era,
    UsesPParams era,
    Show (Core.Witnesses era),
    Core.TxBody era ~ TxBody era,
    Core.PParams era ~ PParams era,
    Core.Tx era ~ Tx era,
    Core.Value era ~ Coin,
    Eq (PredicateFailure (Core.EraRule "PPUP" era)),
    Embed (Core.EraRule "PPUP" era) (UTXO era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era)
  ) =>
  STS (UTXO era)
  where
  type State (UTXO era) = UTxOState era
  type Signal (UTXO era) = Tx era
  type Environment (UTXO era) = UtxoEnv era
  type BaseM (UTXO era) = ShelleyBase
  type PredicateFailure (UTXO era) = UtxoPredicateFailure era
  type Event (UTXO era) = UtxoEvent era

  transitionRules = [utxoInductive]

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
      let utxoBalance us = Val.inject (_deposited us <> _fees us) <> balance (_utxo us)
          withdrawals :: TxBody era -> Core.Value era
          withdrawals txb = Val.inject $ foldl' (<>) mempty $ unWdrl $ _wdrls txb
       in PostCondition
            "Should preserve value in the UTxO state"
            ( \(TRC (_, us, tx)) us' ->
                utxoBalance us <> withdrawals (getField @"body" tx) == utxoBalance us'
            )
    ]

utxoInductive ::
  forall era utxo.
  ( Core.TxOut era ~ TxOut era,
    Core.Value era ~ Coin,
    UsesAuxiliary era,
    STS (utxo era),
    Embed (Core.EraRule "PPUP" era) (utxo era),
    BaseM (utxo era) ~ ShelleyBase,
    Environment (utxo era) ~ UtxoEnv era,
    State (utxo era) ~ UTxOState era,
    Signal (utxo era) ~ Core.Tx era,
    PredicateFailure (utxo era) ~ UtxoPredicateFailure era,
    Event (utxo era) ~ UtxoEvent era,
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "ttl" (Core.TxBody era) SlotNo,
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_minUTxOValue" (Core.PParams era) Coin,
    HasField "_maxTxSize" (Core.PParams era) Natural
  ) =>
  TransitionRule (utxo era)
utxoInductive = do
  TRC (UtxoEnv slot pp stakepools genDelegs, u, tx) <- judgmentContext
  let UTxOState utxo _ _ ppup _ = u
  let txb = getField @"body" tx

  {- txttl txb ≥ slot -}
  runValidation $ validateTimeToLive txb slot

  {- txins txb ≠ ∅ -}
  runValidation $ validateInputSetEmptyUTxO txb

  {- minfee pp tx ≤ txfee txb -}
  runValidation $ validateFeeTooSmallUTxO pp tx

  {- txins txb ⊆ dom utxo -}
  runValidation $ validateBadInputsUTxO utxo $ getField @"inputs" txb

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runValidation $ validateWrongNetwork netId txb

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runValidation $ validateWrongNetworkWithdrawal netId txb

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runValidation $ validateValueNotConservedUTxO pp utxo stakepools txb

  -- process Protocol Parameter Update Proposals
  ppup' <- trans @(Core.EraRule "PPUP" era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  let outputs = txouts txb
  {- ∀(_ → (_, c)) ∈ txouts txb, c ≥ (minUTxOValue pp) -}
  runValidation $ validateOutputTooSmallUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runValidation $ validateOutputBootAddrAttrsTooBig outputs

  {- txsize tx ≤ maxTxSize pp -}
  runValidation $ validateMaxTxSizeUTxO pp tx

  let refunded = keyRefunds pp txb
  let txCerts = toList $ getField @"certs" txb
  let totalDeposits' = totalDeposits pp (`Map.notMember` stakepools) txCerts
  tellEvent $ TotalDeposits totalDeposits'
  let depositChange = totalDeposits' <-> refunded
  pure $! updateUTxOState u txb depositChange ppup'

-- | The ttl field marks the top of an open interval, so it must be strictly
-- less than the slot, so fail if it is (>=).
--
-- > txttl txb ≥ slot
validateTimeToLive ::
  HasField "ttl" (Core.TxBody era) SlotNo =>
  Core.TxBody era ->
  SlotNo ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateTimeToLive txb slot = failureUnless (ttl >= slot) $ ExpiredUTxO ttl slot
  where
    ttl = getField @"ttl" txb

-- | Ensure that there is at least one input in the `Core.TxBody`
--
-- > txins txb ≠ ∅
validateInputSetEmptyUTxO ::
  HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))) =>
  Core.TxBody era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateInputSetEmptyUTxO txb =
  failureUnless (txins txb /= Set.empty) InputSetEmptyUTxO
  where
    txins = getField @"inputs"

-- | Ensure that the fee is at least the amount specified by the `minfee`
--
-- > minfee pp tx ≤ txfee txb
validateFeeTooSmallUTxO ::
  ( HasField "body" (Core.Tx era) (Core.TxBody era),
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "txsize" (Core.Tx era) Integer
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateFeeTooSmallUTxO pp tx =
  failureUnless (minFee <= txFee) $ FeeTooSmallUTxO minFee txFee
  where
    minFee = minfee pp tx
    txFee = getField @"txfee" txb
    txb = getField @"body" tx

-- | Ensure all transaction inputs are present in `UTxO`
--
-- > inputs ⊆ dom utxo
validateBadInputsUTxO ::
  UTxO era ->
  Set (TxIn (Crypto era)) ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateBadInputsUTxO utxo txins =
  failureUnless (Set.null badInputs) $ BadInputsUTxO badInputs
  where
    {- inputs ➖ dom utxo -}
    badInputs = Set.filter (`SplitMap.notMember` unUTxO utxo) txins

-- | Make sure all addresses match the supplied NetworkId
--
-- > ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId
validateWrongNetwork ::
  Era era =>
  Network ->
  Core.TxBody era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateWrongNetwork netId txb =
  failureUnless (null addrsWrongNetwork) $ WrongNetwork netId (Set.fromList addrsWrongNetwork)
  where
    txOutputs = getField @"outputs" txb
    addrsWrongNetwork =
      filter
        (\a -> getNetwork a /= netId)
        (getTxOutAddr <$> toList txOutputs)

-- | Make sure all addresses match the supplied NetworkId
--
-- > ∀(a → ) ∈ txwdrls txb, netId a = NetworkId
validateWrongNetworkWithdrawal ::
  (HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))) =>
  Network ->
  Core.TxBody era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateWrongNetworkWithdrawal netId txb =
  failureUnless (null wdrlsWrongNetwork) $
    WrongNetworkWithdrawal netId (Set.fromList wdrlsWrongNetwork)
  where
    wdrlsWrongNetwork =
      filter
        (\a -> getRwdNetwork a /= netId)
        (Map.keys . unWdrl . getField @"wdrls" $ txb)

-- | Ensure that value consumed and produced matches up exactly
--
-- > consumed pp utxo txb = produced pp poolParams txb
validateValueNotConservedUTxO ::
  ( Era era,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  Core.PParams era ->
  UTxO era ->
  Map (KeyHash 'StakePool (Crypto era)) a ->
  Core.TxBody era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateValueNotConservedUTxO pp utxo stakepools txb =
  failureUnless (consumedValue == producedValue) $
    ValueNotConservedUTxO consumedValue producedValue
  where
    consumedValue = consumed pp utxo txb
    producedValue = produced pp (`Map.notMember` stakepools) txb

-- | Ensure there are no `Core.TxOut`s that have less than @minUTxOValue@
--
-- > ∀(_ → (_, c)) ∈ txouts txb, c ≥ (minUTxOValue pp)
validateOutputTooSmallUTxO ::
  ( HasField "_minUTxOValue" (Core.PParams era) Coin,
    HasField "value" (Core.TxOut era) Coin
  ) =>
  Core.PParams era ->
  UTxO era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateOutputTooSmallUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    minUTxOValue = getField @"_minUTxOValue" pp
    -- minUTxOValue deposit comparison done as Coin because this rule is correct
    -- strictly in the Shelley era (in ShelleyMA we additionally check that all
    -- amounts are non-negative)
    outputsTooSmall =
      filter
        ( \x ->
            let c = getField @"value" x
             in c < minUTxOValue
        )
        (SplitMap.elems outputs)

-- | Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
-- It is important to limit their overall size.
--
-- > ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64
validateOutputBootAddrAttrsTooBig ::
  Era era =>
  UTxO era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateOutputBootAddrAttrsTooBig (UTxO outputs) =
  failureUnless (null outputsAttrsTooBig) $ OutputBootAddrAttrsTooBig outputsAttrsTooBig
  where
    outputsAttrsTooBig =
      filter
        ( \txOut ->
            case getTxOutBootstrapAddress txOut of
              Just addr -> bootstrapAddressAttrsSize addr > 64
              _ -> False
        )
        (SplitMap.elems outputs)

-- | Ensure that the size of the transaction does not exceed the @maxTxSize@ protocol parameter
--
-- > txsize tx ≤ maxTxSize pp
validateMaxTxSizeUTxO ::
  ( HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "txsize" (Core.Tx era) Integer
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateMaxTxSizeUTxO pp tx =
  failureUnless (txSize <= maxTxSize) $ MaxTxSizeUTxO txSize maxTxSize
  where
    maxTxSize = toInteger (getField @"_maxTxSize" pp)
    txSize = getField @"txsize" tx

updateUTxOState ::
  (Era era, HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))) =>
  UTxOState era ->
  Core.TxBody era ->
  Coin ->
  State (Core.EraRule "PPUP" era) ->
  UTxOState era
updateUTxOState UTxOState {_utxo, _deposited, _fees, _stakeDistro} txb depositChange ppups =
  let UTxO utxo = _utxo
      !utxoAdd = txouts txb -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(!utxoWithout, !utxoDel) = SplitMap.extractKeysSet utxo (getField @"inputs" txb)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `SplitMap.union` unUTxO utxoAdd
      newIncStakeDistro = updateStakeDistribution _stakeDistro (UTxO utxoDel) utxoAdd
   in UTxOState
        { _utxo = UTxO newUTxO,
          _deposited = _deposited <> depositChange,
          _fees = _fees <> getField @"txfee" txb,
          _ppups = ppups,
          _stakeDistro = newIncStakeDistro
        }

instance
  ( Era era,
    STS (PPUP era),
    PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era,
    Event (Core.EraRule "PPUP" era) ~ PpupEvent era
  ) =>
  Embed (PPUP era) (UTXO era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent
