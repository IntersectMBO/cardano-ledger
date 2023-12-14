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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Utxo (
  ShelleyUTXO,
  UtxoEnv (..),
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
  validateValueNotConservedUTxO,
  utxoEnvSlotL,
  utxoEnvPParamsL,
  utxoEnvCertStateL,
  utxoEnvGenDelegsL,
)
where

import Cardano.Ledger.Address (
  Addr (..),
  bootstrapAddressAttrsSize,
  getNetwork,
  getRwdNetwork,
 )
import Cardano.Ledger.BaseTypes (
  Network,
  ShelleyBase,
  invalidKey,
  networkId,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.CertState (
  certsTotalDepositsTxBody,
  certsTotalRefundsTxBody,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs)
import Cardano.Ledger.Rules.ValidationMode (
  Inject (..),
  Test,
  runTest,
 )
import Cardano.Ledger.SafeHash (
  SafeHash,
  hashAnnotated,
 )
import Cardano.Ledger.Shelley.AdaPots (
  consumedTxBody,
  producedTxBody,
 )
import Cardano.Ledger.Shelley.Era (
  ShelleyEra,
  ShelleyUTXO,
 )
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  PPUPPredFailure,
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.LedgerState.IncrementalStake
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (
  PpupEnv (..),
  PpupEvent,
  ShelleyPPUP,
  ShelleyPpupPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Reports (showTxCerts)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (
  RewardAcnt,
  ShelleyEraTxBody (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.UTxO (
  consumed,
  produced,
  txup,
 )
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (
  EraUTxO,
  UTxO (..),
  balance,
  txouts,
 )
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Assertion (..),
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
import Data.Foldable (
  foldl',
  toList,
 )
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data UtxoEnv era = UtxoEnv
  { ueSlot :: SlotNo
  , uePParams :: PParams era
  , ueCertState :: CertState era
  , ueGenDelegs :: GenDelegs (EraCrypto era)
  }

utxoEnvSlotL :: Lens' (UtxoEnv era) SlotNo
utxoEnvSlotL = lens ueSlot $ \x y -> x {ueSlot = y}

utxoEnvPParamsL :: Lens' (UtxoEnv era) (PParams era)
utxoEnvPParamsL = lens uePParams $ \x y -> x {uePParams = y}

utxoEnvCertStateL :: Lens' (UtxoEnv era) (CertState era)
utxoEnvCertStateL = lens ueCertState $ \x y -> x {ueCertState = y}

utxoEnvGenDelegsL :: Lens' (UtxoEnv era) (GenDelegs (EraCrypto era))
utxoEnvGenDelegsL = lens ueGenDelegs $ \x y -> x {ueGenDelegs = y}

deriving instance Show (PParams era) => Show (UtxoEnv era)

data UtxoEvent era
  = TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
  | UpdateEvent (Event (EraRule "PPUP" era))
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)

data ShelleyUtxoPredFailure era
  = BadInputsUTxO
      !(Set (TxIn (EraCrypto era))) -- The bad transaction inputs
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
      !(Set (Addr (EraCrypto era))) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network -- the expected network id
      !(Set (RewardAcnt (EraCrypto era))) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure (PPUPPredFailure era) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

deriving stock instance
  ( Show (Value era)
  , Show (TxOut era)
  , Show (PPUPPredFailure era)
  ) =>
  Show (ShelleyUtxoPredFailure era)

deriving stock instance
  ( Eq (Value era)
  , Eq (TxOut era)
  , Eq (PPUPPredFailure era)
  ) =>
  Eq (ShelleyUtxoPredFailure era)

instance
  ( NoThunks (Value era)
  , NoThunks (TxOut era)
  , NoThunks (PPUPPredFailure era)
  ) =>
  NoThunks (ShelleyUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PPUPPredFailure era)
  ) =>
  NFData (ShelleyUtxoPredFailure era)

instance
  ( Era era
  , EncCBOR (Value era)
  , EncCBOR (TxOut era)
  , EncCBOR (PPUPPredFailure era)
  ) =>
  EncCBOR (ShelleyUtxoPredFailure era)
  where
  encCBOR = \case
    BadInputsUTxO ins ->
      encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR ins
    ExpiredUTxO a b ->
      encodeListLen 3
        <> encCBOR (1 :: Word8)
        <> encCBOR a
        <> encCBOR b
    MaxTxSizeUTxO a b ->
      encodeListLen 3
        <> encCBOR (2 :: Word8)
        <> encCBOR a
        <> encCBOR b
    InputSetEmptyUTxO -> encodeListLen 1 <> encCBOR (3 :: Word8)
    FeeTooSmallUTxO a b ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR a
        <> encCBOR b
    ValueNotConservedUTxO a b ->
      encodeListLen 3
        <> encCBOR (5 :: Word8)
        <> encCBOR a
        <> encCBOR b
    OutputTooSmallUTxO outs ->
      encodeListLen 2
        <> encCBOR (6 :: Word8)
        <> encCBOR outs
    UpdateFailure a ->
      encodeListLen 2
        <> encCBOR (7 :: Word8)
        <> encCBOR a
    WrongNetwork right wrongs ->
      encodeListLen 3
        <> encCBOR (8 :: Word8)
        <> encCBOR right
        <> encCBOR wrongs
    WrongNetworkWithdrawal right wrongs ->
      encodeListLen 3
        <> encCBOR (9 :: Word8)
        <> encCBOR right
        <> encCBOR wrongs
    OutputBootAddrAttrsTooBig outs ->
      encodeListLen 2
        <> encCBOR (10 :: Word8)
        <> encCBOR outs

instance
  ( EraTxOut era
  , DecCBOR (PPUPPredFailure era)
  ) =>
  DecCBOR (ShelleyUtxoPredFailure era)
  where
  decCBOR =
    decodeRecordSum "PredicateFailureUTXO" $
      \case
        0 -> do
          ins <- decCBOR
          pure (2, BadInputsUTxO ins)
        1 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, ExpiredUTxO a b)
        2 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, MaxTxSizeUTxO a b)
        3 -> pure (1, InputSetEmptyUTxO)
        4 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, FeeTooSmallUTxO a b)
        5 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, ValueNotConservedUTxO a b)
        6 -> do
          outs <- decCBOR
          pure (2, OutputTooSmallUTxO outs)
        7 -> do
          a <- decCBOR
          pure (2, UpdateFailure a)
        8 -> do
          right <- decCBOR
          wrongs <- decCBOR
          pure (3, WrongNetwork right wrongs)
        9 -> do
          right <- decCBOR
          wrongs <- decCBOR
          pure (3, WrongNetworkWithdrawal right wrongs)
        10 -> do
          outs <- decCBOR
          pure (2, OutputBootAddrAttrsTooBig outs)
        k -> invalidKey k

instance
  ( EraTx era
  , EraUTxO era
  , ShelleyEraTxBody era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , ExactEra ShelleyEra era
  , Tx era ~ ShelleyTx era
  , Show (ShelleyTx era)
  , Embed (EraRule "PPUP" era) (ShelleyUTXO era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , ProtVerAtMost era 8
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  STS (ShelleyUTXO era)
  where
  type State (ShelleyUTXO era) = UTxOState era
  type Signal (ShelleyUTXO era) = ShelleyTx era
  type Environment (ShelleyUTXO era) = UtxoEnv era
  type BaseM (ShelleyUTXO era) = ShelleyBase
  type PredicateFailure (ShelleyUTXO era) = ShelleyUtxoPredFailure era
  type Event (ShelleyUTXO era) = UtxoEvent era

  transitionRules = [utxoInductive]

  renderAssertionViolation
    AssertionViolation
      { avSTS
      , avMsg
      , avCtx = TRC (UtxoEnv _slot pp dpstate _, UTxOState {utxosDeposited, utxosUtxo}, tx)
      } =
      "AssertionViolation ("
        <> avSTS
        <> "): "
        <> avMsg
        <> "\n PParams\n"
        <> show pp
        <> "\n Certs\n"
        <> showTxCerts (tx ^. bodyTxL)
        <> "\n Deposits\n"
        <> show utxosDeposited
        <> "\n Consumed\n"
        <> show (consumedTxBody (tx ^. bodyTxL) pp dpstate utxosUtxo)
        <> "\n Produced\n"
        <> show (producedTxBody (tx ^. bodyTxL) pp dpstate)

  assertions =
    [ PreCondition
        "Deposit pot must not be negative (pre)"
        (\(TRC (_, st, _)) -> utxosDeposited st >= mempty)
    , PostCondition
        "UTxO must increase fee pot"
        (\(TRC (_, st, _)) st' -> utxosFees st' >= utxosFees st)
    , PostCondition
        "Deposit pot must not be negative (post)"
        (\_ st' -> utxosDeposited st' >= mempty)
    , let utxoBalance us = Val.inject (utxosDeposited us <> utxosFees us) <> balance (utxosUtxo us)
          withdrawals :: TxBody era -> Value era
          withdrawals txb = Val.inject $ foldl' (<>) mempty $ unWithdrawals $ txb ^. withdrawalsTxBodyL
       in PostCondition
            "Should preserve value in the UTxO state"
            ( \(TRC (UtxoEnv _ _pp _pools _, us, tx)) us' ->
                utxoBalance us <> withdrawals (tx ^. bodyTxL) == utxoBalance us'
            )
    ]

utxoInductive ::
  forall era utxo.
  ( EraUTxO era
  , ShelleyEraTxBody era
  , ExactEra ShelleyEra era
  , STS (utxo era)
  , Embed (EraRule "PPUP" era) (utxo era)
  , BaseM (utxo era) ~ ShelleyBase
  , Environment (utxo era) ~ UtxoEnv era
  , State (utxo era) ~ UTxOState era
  , Signal (utxo era) ~ Tx era
  , PredicateFailure (utxo era) ~ ShelleyUtxoPredFailure era
  , Event (utxo era) ~ UtxoEvent era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , ProtVerAtMost era 8
  , GovState era ~ ShelleyGovState era
  ) =>
  TransitionRule (utxo era)
utxoInductive = do
  TRC (UtxoEnv slot pp certState genDelegs, utxos, tx) <- judgmentContext
  let UTxOState utxo _ _ ppup _ _ = utxos
      txBody = tx ^. bodyTxL
      outputs = txBody ^. outputsTxBodyL

  {- txttl txb ≥ slot -}
  runTest $ validateTimeToLive txBody slot

  {- txins txb ≠ ∅ -}
  runTest $ validateInputSetEmptyUTxO txBody

  {- minfee pp tx ≤ txfee txb -}
  runTest $ validateFeeTooSmallUTxO pp tx

  {- txins txb ⊆ dom utxo -}
  runTest $ validateBadInputsUTxO utxo $ txBody ^. inputsTxBodyL

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTest $ validateWrongNetwork netId outputs

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTest $ validateWrongNetworkWithdrawal netId txBody

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ validateValueNotConservedUTxO pp utxo certState txBody

  -- process Protocol Parameter Update Proposals
  ppup' <- trans @(EraRule "PPUP" era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  {- ∀(_ → (_, c)) ∈ txouts txb, c ≥ (minUTxOValue pp) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTest $ validateOutputBootAddrAttrsTooBig outputs

  {- txsize tx ≤ maxTxSize pp -}
  runTest $ validateMaxTxSizeUTxO pp tx

  updateUTxOState
    pp
    utxos
    txBody
    certState
    ppup'
    (tellEvent . TotalDeposits (hashAnnotated txBody))
    (\a b -> tellEvent $ TxUTxODiff a b)

-- | The ttl field marks the top of an open interval, so it must be strictly
-- less than the slot, so fail if it is (>=).
--
-- > txttl txb ≥ slot
validateTimeToLive ::
  (ShelleyEraTxBody era, ExactEra ShelleyEra era) =>
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
  EraTx era =>
  PParams era ->
  Tx era ->
  Test (ShelleyUtxoPredFailure era)
validateFeeTooSmallUTxO pp tx =
  failureUnless (minFee <= txFee) $ FeeTooSmallUTxO minFee txFee
  where
    minFee = getMinFeeTx pp tx
    txFee = txb ^. feeTxBodyL
    txb = tx ^. bodyTxL

-- | Ensure all transaction inputs are present in `UTxO`
--
-- > inputs ⊆ dom utxo
validateBadInputsUTxO ::
  UTxO era ->
  Set (TxIn (EraCrypto era)) ->
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
  (EraTxOut era, Foldable f) =>
  Network ->
  f (TxOut era) ->
  Test (ShelleyUtxoPredFailure era)
validateWrongNetwork netId outputs =
  failureUnless (null addrsWrongNetwork) $ WrongNetwork netId (Set.fromList addrsWrongNetwork)
  where
    addrsWrongNetwork =
      filter
        (\a -> getNetwork a /= netId)
        (view addrTxOutL <$> toList outputs)

-- | Make sure all addresses match the supplied NetworkId
--
-- > ∀(a → ) ∈ txwdrls txb, netId a = NetworkId
validateWrongNetworkWithdrawal ::
  ShelleyEraTxBody era =>
  Network ->
  TxBody era ->
  Test (ShelleyUtxoPredFailure era)
validateWrongNetworkWithdrawal netId txb =
  failureUnless (null withdrawalsWrongNetwork) $
    WrongNetworkWithdrawal netId (Set.fromList withdrawalsWrongNetwork)
  where
    withdrawalsWrongNetwork =
      filter
        (\a -> getRwdNetwork a /= netId)
        (Map.keys . unWithdrawals $ txb ^. withdrawalsTxBodyL)

-- | Ensure that value consumed and produced matches up exactly
--
-- > consumed pp utxo txb = produced pp poolParams txb
validateValueNotConservedUTxO ::
  ( ShelleyEraTxBody era
  , EraUTxO era
  ) =>
  PParams era ->
  UTxO era ->
  CertState era ->
  TxBody era ->
  Test (ShelleyUtxoPredFailure era)
validateValueNotConservedUTxO pp utxo dpstate txb =
  failureUnless (consumedValue == producedValue) $ ValueNotConservedUTxO consumedValue producedValue
  where
    consumedValue = consumed pp dpstate utxo txb
    producedValue = produced pp dpstate txb

-- | Ensure there are no `TxOut`s that have less than @minUTxOValue@
--
-- > ∀(_ → (_, c)) ∈ txouts txb, c ≥ (minUTxOValue pp)
validateOutputTooSmallUTxO ::
  (EraTxOut era, Foldable f) =>
  PParams era ->
  f (TxOut era) ->
  Test (ShelleyUtxoPredFailure era)
validateOutputTooSmallUTxO pp outputs =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    -- minUTxOValue deposit comparison done as Coin because this rule is correct
    -- strictly in the Shelley era (in ShelleyMA we additionally check that all
    -- amounts are non-negative)
    outputsTooSmall =
      filter
        (\txOut -> txOut ^. coinTxOutL < getMinCoinTxOut pp txOut)
        (toList outputs)

-- | Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
-- It is important to limit their overall size.
--
-- > ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64
validateOutputBootAddrAttrsTooBig ::
  (EraTxOut era, Foldable f) =>
  f (TxOut era) ->
  Test (ShelleyUtxoPredFailure era)
validateOutputBootAddrAttrsTooBig outputs =
  failureUnless (null outputsAttrsTooBig) $ OutputBootAddrAttrsTooBig outputsAttrsTooBig
  where
    outputsAttrsTooBig =
      filter
        ( \txOut ->
            case txOut ^. bootAddrTxOutF of
              Just addr -> bootstrapAddressAttrsSize addr > 64
              _ -> False
        )
        (toList outputs)

-- | Ensure that the size of the transaction does not exceed the @maxTxSize@ protocol parameter
--
-- > txsize tx ≤ maxTxSize pp
validateMaxTxSizeUTxO ::
  EraTx era =>
  PParams era ->
  Tx era ->
  Test (ShelleyUtxoPredFailure era)
validateMaxTxSizeUTxO pp tx =
  failureUnless (txSize <= maxTxSize) $ MaxTxSizeUTxO txSize maxTxSize
  where
    maxTxSize = toInteger (pp ^. ppMaxTxSizeL)
    txSize = tx ^. sizeTxF

-- | This monadic action captures the final stages of the UTXO(S) rule. In particular it
-- applies all of the UTxO related aditions and removals, gathers all of the fees into the
-- fee pot `utxosFees` and updates the `utxosDeposited` field. Continuation supplied will
-- be called on the @deposit - refund@ change, which is normally used to emit the
-- `TotalDeposits` event.
updateUTxOState ::
  (EraTxBody era, Monad m) =>
  PParams era ->
  UTxOState era ->
  TxBody era ->
  CertState era ->
  GovState era ->
  (Coin -> m ()) ->
  (UTxO era -> UTxO era -> m ()) ->
  m (UTxOState era)
updateUTxOState pp utxos txBody certState govState depositChangeEvent txUtxODiffEvent = do
  let UTxOState {utxosUtxo, utxosDeposited, utxosFees, utxosStakeDistr, utxosDonation} = utxos
      UTxO utxo = utxosUtxo
      !utxoAdd = txouts txBody -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(utxoWithout, utxoDel) = extractKeys utxo (txBody ^. inputsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `Map.union` unUTxO utxoAdd
      deletedUTxO = UTxO utxoDel
      newIncStakeDistro = updateStakeDistribution pp utxosStakeDistr deletedUTxO utxoAdd
      totalRefunds = certsTotalRefundsTxBody pp certState txBody
      totalDeposits = certsTotalDepositsTxBody pp certState txBody
      depositChange = totalDeposits <-> totalRefunds
  depositChangeEvent depositChange
  txUtxODiffEvent deletedUTxO utxoAdd
  pure $!
    UTxOState
      { utxosUtxo = UTxO newUTxO
      , utxosDeposited = utxosDeposited <> depositChange
      , utxosFees = utxosFees <> txBody ^. feeTxBodyL
      , utxosGovState = govState
      , utxosStakeDistr = newIncStakeDistro
      , utxosDonation = utxosDonation
      }

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , PPUPPredFailure era ~ ShelleyPpupPredFailure era
  , Event (EraRule "PPUP" era) ~ PpupEvent era
  ) =>
  Embed (ShelleyPPUP era) (ShelleyUTXO era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent

-- =================================

instance
  PPUPPredFailure era ~ ShelleyPpupPredFailure era =>
  Inject (ShelleyPpupPredFailure era) (ShelleyUtxoPredFailure era)
  where
  inject = UpdateFailure

instance Inject (ShelleyUtxoPredFailure era) (ShelleyUtxoPredFailure era) where
  inject = id
