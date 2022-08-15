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
  ( ShelleyUTXO,
    ShelleyUtxoEnv (..),
    ShelleyUtxoPredFailure (..),
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
    invalidKey,
    networkId,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState.IncrementalStake
import Cardano.Ledger.Shelley.LedgerState.Types
  ( UTxOState (..),
  )
import Cardano.Ledger.Shelley.PParams
  ( PPUPState (..),
    ShelleyPParams,
    ShelleyPParamsHKD (..),
    Update,
  )
import Cardano.Ledger.Shelley.Rules.Ppup
  ( PpupEvent,
    ShelleyPPUP,
    ShelleyPPUPEnv (..),
    ShelleyPpupPredFailure,
  )
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx (..),
    ShelleyTxOut (..),
    TxIn,
    minfee,
  )
import Cardano.Ledger.Shelley.TxBody
  ( PoolParams,
    RewardAcnt,
    ShelleyEraTxBody (..),
    ShelleyTxBody,
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO
  ( UTxO (..),
    balance,
    consumed,
    keyRefunds,
    produced,
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
import Data.Coders
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
  )
import Data.Foldable (foldl', toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Validation (failureUnless)

data ShelleyUTXO era

data ShelleyUtxoEnv era
  = UtxoEnv
      SlotNo
      (PParams era)
      (Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era)))
      (GenDelegs (Crypto era))

deriving instance Show (PParams era) => Show (ShelleyUtxoEnv era)

data UtxoEvent era
  = TotalDeposits Coin
  | UpdateEvent (Event (EraRule "PPUP" era))

data ShelleyUtxoPredFailure era
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
      !(Value era) -- the Coin consumed by this transaction
      !(Value era) -- the Coin produced by this transaction
  | WrongNetwork
      !Network -- the expected network id
      !(Set (Addr (Crypto era))) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network -- the expected network id
      !(Set (RewardAcnt (Crypto era))) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure (PredicateFailure (EraRule "PPUP" era)) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

deriving stock instance
  ( Show (Value era),
    Show (TxOut era),
    Show (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Show (ShelleyUtxoPredFailure era)

deriving stock instance
  ( Eq (Value era),
    Eq (TxOut era),
    Eq (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Eq (ShelleyUtxoPredFailure era)

instance
  ( NoThunks (Value era),
    NoThunks (TxOut era),
    NoThunks (PredicateFailure (EraRule "PPUP" era))
  ) =>
  NoThunks (ShelleyUtxoPredFailure era)

instance
  ( Typeable era,
    CC.Crypto (Crypto era),
    ToCBOR (Value era),
    ToCBOR (TxOut era),
    ToCBOR (PredicateFailure (EraRule "PPUP" era))
  ) =>
  ToCBOR (ShelleyUtxoPredFailure era)
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
  ( EraTxOut era,
    FromCBOR (TxOut era),
    FromCBOR (PredicateFailure (EraRule "PPUP" era))
  ) =>
  FromCBOR (ShelleyUtxoPredFailure era)
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
  ( EraTx era,
    ShelleyEraTxBody era,
    ProtVerInEra era (ShelleyEra c),
    TxOut era ~ ShelleyTxOut era,
    Show (Value era),
    Show (TxWits era),
    Show (PParamsUpdate era),
    TxBody era ~ ShelleyTxBody era,
    PParams era ~ ShelleyPParams era,
    Tx era ~ ShelleyTx era,
    Value era ~ Coin,
    Show (ShelleyTx era),
    Eq (PredicateFailure (EraRule "PPUP" era)),
    Embed (EraRule "PPUP" era) (ShelleyUTXO era),
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  ) =>
  STS (ShelleyUTXO era)
  where
  type State (ShelleyUTXO era) = UTxOState era
  type Signal (ShelleyUTXO era) = ShelleyTx era
  type Environment (ShelleyUTXO era) = ShelleyUtxoEnv era
  type BaseM (ShelleyUTXO era) = ShelleyBase
  type PredicateFailure (ShelleyUTXO era) = ShelleyUtxoPredFailure era
  type Event (ShelleyUTXO era) = UtxoEvent era

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
          withdrawals :: ShelleyTxBody era -> Value era
          withdrawals txb = Val.inject $ foldl' (<>) mempty $ unWdrl $ txb ^. wdrlsTxBodyL
       in PostCondition
            "Should preserve value in the UTxO state"
            ( \(TRC (_, us, tx)) us' ->
                utxoBalance us <> withdrawals (tx ^. bodyTxL) == utxoBalance us'
            )
    ]

utxoInductive ::
  forall era utxo c.
  ( EraTx era,
    ShelleyEraTxBody era,
    ProtVerInEra era (ShelleyEra c),
    TxOut era ~ ShelleyTxOut era,
    STS (utxo era),
    Embed (EraRule "PPUP" era) (utxo era),
    BaseM (utxo era) ~ ShelleyBase,
    Environment (utxo era) ~ ShelleyUtxoEnv era,
    State (utxo era) ~ UTxOState era,
    Signal (utxo era) ~ Tx era,
    PredicateFailure (utxo era) ~ ShelleyUtxoPredFailure era,
    Event (utxo era) ~ UtxoEvent era,
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era),
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_minUTxOValue" (PParams era) Coin,
    HasField "_maxTxSize" (PParams era) Natural
  ) =>
  TransitionRule (utxo era)
utxoInductive = do
  TRC (UtxoEnv slot pp stakepools genDelegs, u, tx) <- judgmentContext
  let UTxOState utxo _ _ ppup _ = u
  let txb = tx ^. bodyTxL

  {- txttl txb ≥ slot -}
  runTest $ validateTimeToLive txb slot

  {- txins txb ≠ ∅ -}
  runTest $ validateInputSetEmptyUTxO txb

  {- minfee pp tx ≤ txfee txb -}
  runTest $ validateFeeTooSmallUTxO pp tx

  {- txins txb ⊆ dom utxo -}
  runTest $ validateBadInputsUTxO utxo $ txb ^. inputsTxBodyL

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTest . validateWrongNetwork netId . toList $ txb ^. outputsTxBodyL

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTest $ validateWrongNetworkWithdrawal netId txb

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ validateValueNotConservedUTxO pp utxo stakepools txb

  -- process Protocol Parameter Update Proposals
  ppup' <- trans @(EraRule "PPUP" era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  let outputs = txouts txb
  {- ∀(_ → (_, c)) ∈ txouts txb, c ≥ (minUTxOValue pp) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTest $ validateOutputBootAddrAttrsTooBig outputs

  {- txsize tx ≤ maxTxSize pp -}
  runTest $ validateMaxTxSizeUTxO pp tx

  let refunded = keyRefunds pp txb
  let txCerts = toList $ txb ^. certsTxBodyL
  let totalDeposits' = totalDeposits pp (`Map.notMember` stakepools) txCerts
  tellEvent $ TotalDeposits totalDeposits'
  let depositChange = totalDeposits' <-> refunded
  pure $! updateUTxOState u txb depositChange ppup'

-- | The ttl field marks the top of an open interval, so it must be strictly
-- less than the slot, so fail if it is (>=).
--
-- > txttl txb ≥ slot
validateTimeToLive ::
  (ShelleyEraTxBody era, ProtVerInEra era (ShelleyEra c)) =>
  TxBody era ->
  SlotNo ->
  Test (ShelleyUtxoPredFailure era)
validateTimeToLive txb slot = failureUnless (ttl >= slot) $ ExpiredUTxO ttl slot
  where
    ttl = txb ^. ttlTxBodyL

-- | Ensure that there is at least one input in the `TxBody`
--
-- > txins txb ≠ ∅
validateInputSetEmptyUTxO ::
  EraTxBody era =>
  TxBody era ->
  Test (ShelleyUtxoPredFailure era)
validateInputSetEmptyUTxO txb =
  failureUnless (txins /= Set.empty) InputSetEmptyUTxO
  where
    txins = txb ^. inputsTxBodyL

-- | Ensure that the fee is at least the amount specified by the `minfee`
--
-- > minfee pp tx ≤ txfee txb
validateFeeTooSmallUTxO ::
  ( EraTx era,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural
  ) =>
  PParams era ->
  Tx era ->
  Test (ShelleyUtxoPredFailure era)
validateFeeTooSmallUTxO pp tx =
  failureUnless (minFee <= txFee) $ FeeTooSmallUTxO minFee txFee
  where
    minFee = minfee pp tx
    txFee = txb ^. feeTxBodyL
    txb = tx ^. bodyTxL

-- | Ensure all transaction inputs are present in `UTxO`
--
-- > inputs ⊆ dom utxo
validateBadInputsUTxO ::
  UTxO era ->
  Set (TxIn (Crypto era)) ->
  Test (ShelleyUtxoPredFailure era)
validateBadInputsUTxO utxo txins =
  failureUnless (Set.null badInputs) $ BadInputsUTxO badInputs
  where
    {- inputs ➖ dom utxo -}
    badInputs = Set.filter (`Map.notMember` unUTxO utxo) txins

-- | Make sure all addresses match the supplied NetworkId
--
-- > ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId
validateWrongNetwork ::
  EraTxOut era =>
  Network ->
  [TxOut era] ->
  Test (ShelleyUtxoPredFailure era)
validateWrongNetwork netId outs =
  failureUnless (null addrsWrongNetwork) $ WrongNetwork netId (Set.fromList addrsWrongNetwork)
  where
    addrsWrongNetwork =
      filter
        (\a -> getNetwork a /= netId)
        (view addrTxOutL <$> outs)

-- | Make sure all addresses match the supplied NetworkId
--
-- > ∀(a → ) ∈ txwdrls txb, netId a = NetworkId
validateWrongNetworkWithdrawal ::
  ShelleyEraTxBody era =>
  Network ->
  TxBody era ->
  Test (ShelleyUtxoPredFailure era)
validateWrongNetworkWithdrawal netId txb =
  failureUnless (null wdrlsWrongNetwork) $
    WrongNetworkWithdrawal netId (Set.fromList wdrlsWrongNetwork)
  where
    wdrlsWrongNetwork =
      filter
        (\a -> getRwdNetwork a /= netId)
        (Map.keys . unWdrl $ txb ^. wdrlsTxBodyL)

-- | Ensure that value consumed and produced matches up exactly
--
-- > consumed pp utxo txb = produced pp poolParams txb
validateValueNotConservedUTxO ::
  ( ShelleyEraTxBody era,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin
  ) =>
  PParams era ->
  UTxO era ->
  Map (KeyHash 'StakePool (Crypto era)) a ->
  TxBody era ->
  Test (ShelleyUtxoPredFailure era)
validateValueNotConservedUTxO pp utxo stakepools txb =
  failureUnless (consumedValue == producedValue) $
    ValueNotConservedUTxO consumedValue producedValue
  where
    consumedValue = consumed pp utxo txb
    producedValue = produced pp (`Map.notMember` stakepools) txb

-- | Ensure there are no `TxOut`s that have less than @minUTxOValue@
--
-- > ∀(_ → (_, c)) ∈ txouts txb, c ≥ (minUTxOValue pp)
validateOutputTooSmallUTxO ::
  ( HasField "_minUTxOValue" (PParams era) Coin,
    EraTxOut era
  ) =>
  PParams era ->
  UTxO era ->
  Test (ShelleyUtxoPredFailure era)
validateOutputTooSmallUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    minUTxOValue = getField @"_minUTxOValue" pp
    -- minUTxOValue deposit comparison done as Coin because this rule is correct
    -- strictly in the Shelley era (in ShelleyMA we additionally check that all
    -- amounts are non-negative)
    outputsTooSmall =
      filter
        (\txOut -> txOut ^. coinTxOutL < minUTxOValue)
        (Map.elems outputs)

-- | Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
-- It is important to limit their overall size.
--
-- > ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64
validateOutputBootAddrAttrsTooBig ::
  EraTxOut era =>
  UTxO era ->
  Test (ShelleyUtxoPredFailure era)
validateOutputBootAddrAttrsTooBig (UTxO outputs) =
  failureUnless (null outputsAttrsTooBig) $ OutputBootAddrAttrsTooBig outputsAttrsTooBig
  where
    outputsAttrsTooBig =
      filter
        ( \txOut ->
            case txOut ^. bootAddrTxOutF of
              Just addr -> bootstrapAddressAttrsSize addr > 64
              _ -> False
        )
        (Map.elems outputs)

-- | Ensure that the size of the transaction does not exceed the @maxTxSize@ protocol parameter
--
-- > txsize tx ≤ maxTxSize pp
validateMaxTxSizeUTxO ::
  ( HasField "_maxTxSize" (PParams era) Natural,
    EraTx era
  ) =>
  PParams era ->
  Tx era ->
  Test (ShelleyUtxoPredFailure era)
validateMaxTxSizeUTxO pp tx =
  failureUnless (txSize <= maxTxSize) $ MaxTxSizeUTxO txSize maxTxSize
  where
    maxTxSize = toInteger (getField @"_maxTxSize" pp)
    txSize = tx ^. sizeTxF

updateUTxOState ::
  EraTxBody era =>
  UTxOState era ->
  TxBody era ->
  Coin ->
  State (EraRule "PPUP" era) ->
  UTxOState era
updateUTxOState UTxOState {_utxo, _deposited, _fees, _stakeDistro} txb depositChange ppups =
  let UTxO utxo = _utxo
      !utxoAdd = txouts txb -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(utxoWithout, utxoDel) = extractKeys utxo (txb ^. inputsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `Map.union` unUTxO utxoAdd
      newIncStakeDistro = updateStakeDistribution _stakeDistro (UTxO utxoDel) utxoAdd
   in UTxOState
        { _utxo = UTxO newUTxO,
          _deposited = _deposited <> depositChange,
          _fees = _fees <> txb ^. feeTxBodyL,
          _ppups = ppups,
          _stakeDistro = newIncStakeDistro
        }

instance
  ( Era era,
    STS (ShelleyPPUP era),
    PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era,
    Event (EraRule "PPUP" era) ~ PpupEvent era
  ) =>
  Embed (ShelleyPPUP era) (ShelleyUTXO era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent

-- =================================

instance
  PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era =>
  Inject (ShelleyPpupPredFailure era) (ShelleyUtxoPredFailure era)
  where
  inject = UpdateFailure

instance Inject (ShelleyUtxoPredFailure era) (ShelleyUtxoPredFailure era) where
  inject = id
