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
{-# LANGUAGE RecordWildCards #-}
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
  validSizeComputationCheck,
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
) where

import Cardano.Ledger.Address (bootstrapAddressAttrsSize, getNetwork)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network,
  Relation (..),
  ShelleyBase,
  StrictMaybe,
  networkId,
 )
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyUTXO)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (
  PpupEnv (..),
  PpupEvent,
  ShelleyPPUP,
  ShelleyPpupPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Reports (showTxCerts)
import Cardano.Ledger.Shelley.UTxO (produced)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn)
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
  failureOnNonEmpty,
  failureOnNonEmptySet,
  judgmentContext,
  liftSTS,
  tellEvent,
  trans,
  wrapEvent,
  wrapFailed,
 )
import Data.Foldable as F (foldl', toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (NoThunks (..))
import Validation (failureUnless)

data UtxoEnv era = UtxoEnv
  { ueSlot :: SlotNo
  , uePParams :: PParams era
  , ueCertState :: CertState era
  }
  deriving (Generic)

instance (EraPParams era, EraCertState era) => EncCBOR (UtxoEnv era) where
  encCBOR x@(UtxoEnv _ _ _) =
    let UtxoEnv {..} = x
     in encode $
          Rec UtxoEnv
            !> To ueSlot
            !> To uePParams
            !> To ueCertState

instance (EraPParams era, EraCertState era, Typeable (CertState era)) => DecCBOR (UtxoEnv era) where
  decCBOR =
    decode $
      RecD UtxoEnv
        <! From
        <! From
        <! D decNoShareCBOR

utxoEnvSlotL :: Lens' (UtxoEnv era) SlotNo
utxoEnvSlotL = lens ueSlot $ \x y -> x {ueSlot = y}

utxoEnvPParamsL :: Lens' (UtxoEnv era) (PParams era)
utxoEnvPParamsL = lens uePParams $ \x y -> x {uePParams = y}

utxoEnvCertStateL :: Lens' (UtxoEnv era) (CertState era)
utxoEnvCertStateL = lens ueCertState $ \x y -> x {ueCertState = y}

deriving instance (Show (PParams era), Show (CertState era)) => Show (UtxoEnv era)

deriving instance (Eq (PParams era), Eq (CertState era)) => Eq (UtxoEnv era)

instance (Era era, NFData (PParams era), NFData (CertState era)) => NFData (UtxoEnv era)

data UtxoEvent era
  = TotalDeposits (SafeHash EraIndependentTxBody) Coin
  | UpdateEvent (Event (EraRule "PPUP" era))
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  deriving (Generic)

deriving instance
  ( Era era
  , Eq (TxOut era)
  , Eq (Event (EraRule "PPUP" era))
  ) =>
  Eq (UtxoEvent era)

instance (Era era, NFData (Event (EraRule "PPUP" era)), NFData (TxOut era)) => NFData (UtxoEvent era)

data ShelleyUtxoPredFailure era
  = BadInputsUTxO
      (NonEmptySet TxIn) -- The bad transaction inputs
  | ExpiredUTxO
      (Mismatch RelLTEQ SlotNo)
  | MaxTxSizeUTxO
      (Mismatch RelLTEQ Word32)
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      (Mismatch RelGTEQ Coin)
  | ValueNotConservedUTxO
      (Mismatch RelEQ (Value era))
  | WrongNetwork
      Network -- the expected network id
      (NonEmptySet Addr) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      Network -- the expected network id
      (NonEmptySet AccountAddress) -- the set of account addresses with incorrect network IDs
  | OutputTooSmallUTxO
      (NonEmpty (TxOut era)) -- list of supplied transaction outputs that are too small
  | UpdateFailure (EraRuleFailure "PPUP" era) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      (NonEmpty (TxOut era)) -- list of supplied bad transaction outputs
  deriving (Generic)

type instance EraRuleFailure "UTXO" ShelleyEra = ShelleyUtxoPredFailure ShelleyEra

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure ShelleyEra

instance InjectRuleFailure "UTXO" ShelleyPpupPredFailure ShelleyEra where
  injectFailure = UpdateFailure

deriving stock instance
  ( Show (Value era)
  , Show (TxOut era)
  , Show (EraRuleFailure "PPUP" era)
  ) =>
  Show (ShelleyUtxoPredFailure era)

deriving stock instance
  ( Eq (Value era)
  , Eq (TxOut era)
  , Eq (EraRuleFailure "PPUP" era)
  ) =>
  Eq (ShelleyUtxoPredFailure era)

instance
  ( NoThunks (Value era)
  , NoThunks (TxOut era)
  , NoThunks (EraRuleFailure "PPUP" era)
  ) =>
  NoThunks (ShelleyUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (EraRuleFailure "PPUP" era)
  ) =>
  NFData (ShelleyUtxoPredFailure era)

instance
  ( Era era
  , EncCBOR (Value era)
  , EncCBOR (TxOut era)
  , EncCBOR (EraRuleFailure "PPUP" era)
  ) =>
  EncCBOR (ShelleyUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      BadInputsUTxO ins -> Sum BadInputsUTxO 0 !> To ins
      ExpiredUTxO m -> Sum ExpiredUTxO 1 !> To m
      MaxTxSizeUTxO m -> Sum MaxTxSizeUTxO 2 !> To m
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 3
      FeeTooSmallUTxO m -> Sum FeeTooSmallUTxO 4 !> To m
      ValueNotConservedUTxO m -> Sum ValueNotConservedUTxO 5 !> To m
      OutputTooSmallUTxO outs -> Sum OutputTooSmallUTxO 6 !> To outs
      UpdateFailure a -> Sum UpdateFailure 7 !> To a
      WrongNetwork right wrongs -> Sum WrongNetwork 8 !> To right !> To wrongs
      WrongNetworkWithdrawal right wrongs -> Sum WrongNetworkWithdrawal 9 !> To right !> To wrongs
      OutputBootAddrAttrsTooBig outs -> Sum OutputBootAddrAttrsTooBig 10 !> To outs

instance
  ( EraTxOut era
  , DecCBOR (EraRuleFailure "PPUP" era)
  ) =>
  DecCBOR (ShelleyUtxoPredFailure era)
  where
  decCBOR = decode . Summands "PredicateFailureUTXO" $ \case
    0 -> SumD BadInputsUTxO <! From
    1 -> SumD ExpiredUTxO <! From
    2 -> SumD MaxTxSizeUTxO <! From
    3 -> SumD InputSetEmptyUTxO
    4 -> SumD FeeTooSmallUTxO <! From
    5 -> SumD ValueNotConservedUTxO <! From
    6 -> SumD OutputTooSmallUTxO <! From
    7 -> SumD UpdateFailure <! From
    8 -> SumD WrongNetwork <! From <! From
    9 -> SumD WrongNetworkWithdrawal <! From <! From
    10 -> SumD OutputBootAddrAttrsTooBig <! From
    k -> Invalid k

instance
  ( EraTx era
  , EraUTxO era
  , EraStake era
  , ShelleyEraTxBody era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , ExactEra ShelleyEra era
  , Embed (EraRule "PPUP" era) (ShelleyUTXO era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , EraRule "UTXO" era ~ ShelleyUTXO era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , EraCertState era
  , SafeToHash (TxWits era)
  ) =>
  STS (ShelleyUTXO era)
  where
  type State (ShelleyUTXO era) = UTxOState era
  type Signal (ShelleyUTXO era) = Tx TopTx era
  type Environment (ShelleyUTXO era) = UtxoEnv era
  type BaseM (ShelleyUTXO era) = ShelleyBase
  type PredicateFailure (ShelleyUTXO era) = ShelleyUtxoPredFailure era
  type Event (ShelleyUTXO era) = UtxoEvent era

  transitionRules = [utxoInductive]

  renderAssertionViolation
    AssertionViolation
      { avSTS
      , avMsg
      , avCtx = TRC (UtxoEnv _slot pp certState, UTxOState {utxosDeposited, utxosUtxo}, tx)
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
        <> show (consumedTxBody (tx ^. bodyTxL) pp certState utxosUtxo)
        <> "\n Produced\n"
        <> show (producedTxBody (tx ^. bodyTxL) pp certState)

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
    , let utxoBalance us = Val.inject (utxosDeposited us <> utxosFees us) <> sumUTxO (utxosUtxo us)
          withdrawals :: TxBody TopTx era -> Value era
          withdrawals txb = Val.inject $ F.foldl' (<>) mempty $ unWithdrawals $ txb ^. withdrawalsTxBodyL
       in PostCondition
            "Should preserve value in the UTxO state"
            ( \(TRC (_, us, tx)) us' ->
                utxoBalance us <> withdrawals (tx ^. bodyTxL) == utxoBalance us'
            )
    , validSizeComputationCheck @era
    ]

utxoInductive ::
  forall era.
  ( EraUTxO era
  , EraStake era
  , ShelleyEraTxBody era
  , ExactEra ShelleyEra era
  , STS (EraRule "UTXO" era)
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , Embed (EraRule "PPUP" era) (EraRule "UTXO" era)
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , Event (EraRule "UTXO" era) ~ UtxoEvent era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , GovState era ~ ShelleyGovState era
  , EraCertState era
  ) =>
  TransitionRule (EraRule "UTXO" era)
utxoInductive = do
  TRC (UtxoEnv slot pp certState, utxos, tx) <- judgmentContext
  let utxo = utxos ^. utxoL
      UTxOState _ _ _ ppup _ _ = utxos
      txBody = tx ^. bodyTxL
      outputs = txBody ^. outputsTxBodyL
      genDelegs = dsGenDelegs (certState ^. certDStateL)

  {- txttl txb ≥ slot -}
  runTest $ validateTimeToLive txBody slot

  {- txins txb ≠ ∅ -}
  runTest $ validateInputSetEmptyUTxO txBody

  {- minfee pp tx ≤ txfee txb -}
  runTest $ validateFeeTooSmallUTxO pp tx utxo

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
  ppup' <-
    trans @(EraRule "PPUP" era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txBody ^. updateTxBodyL)

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
  TxBody TopTx era ->
  SlotNo ->
  Test (ShelleyUtxoPredFailure era)
validateTimeToLive txb slot =
  failureUnless (ttl >= slot) $
    ExpiredUTxO Mismatch {mismatchSupplied = ttl, mismatchExpected = slot}
  where
    ttl = txb ^. ttlTxBodyL

-- | Ensure that there is at least one input in the `TxBody`
--
-- > txins txb ≠ ∅
validateInputSetEmptyUTxO ::
  EraTxBody era =>
  TxBody t era ->
  Test (ShelleyUtxoPredFailure era)
validateInputSetEmptyUTxO txb =
  failureUnless (inputs /= Set.empty) InputSetEmptyUTxO
  where
    inputs = txb ^. inputsTxBodyL

-- | Ensure that the fee is at least the amount specified by the `minfee`
--
-- > minfee pp tx ≤ txfee txb
validateFeeTooSmallUTxO ::
  EraUTxO era =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  Test (ShelleyUtxoPredFailure era)
validateFeeTooSmallUTxO pp tx utxo =
  failureUnless (minFee <= txFee) $
    FeeTooSmallUTxO
      Mismatch
        { mismatchSupplied = txFee
        , mismatchExpected = minFee
        }
  where
    minFee = getMinFeeTxUtxo pp tx utxo
    txFee = txb ^. feeTxBodyL
    txb = tx ^. bodyTxL

-- | Ensure all transaction inputs are present in `UTxO`
--
-- > inputs ⊆ dom utxo
validateBadInputsUTxO ::
  UTxO era ->
  Set TxIn ->
  Test (ShelleyUtxoPredFailure era)
validateBadInputsUTxO utxo inputs =
  failureOnNonEmptySet badInputs BadInputsUTxO
  where
    {- inputs ➖ dom utxo -}
    badInputs = Set.filter (`Map.notMember` unUTxO utxo) inputs

-- | Make sure all addresses match the supplied NetworkId
--
-- > ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId
validateWrongNetwork ::
  (EraTxOut era, Foldable f) =>
  Network ->
  f (TxOut era) ->
  Test (ShelleyUtxoPredFailure era)
validateWrongNetwork netId outputs =
  failureOnNonEmptySet addrsWrongNetwork (WrongNetwork netId)
  where
    addrsWrongNetwork =
      filter
        (\a -> getNetwork a /= netId)
        (view addrTxOutL <$> toList outputs)

-- | Make sure all addresses match the supplied NetworkId
--
-- > ∀(a → ) ∈ txwdrls txb, netId a = NetworkId
validateWrongNetworkWithdrawal ::
  EraTxBody era =>
  Network ->
  TxBody t era ->
  Test (ShelleyUtxoPredFailure era)
validateWrongNetworkWithdrawal netId txb =
  failureOnNonEmptySet withdrawalsWrongNetwork (WrongNetworkWithdrawal netId)
  where
    withdrawalsWrongNetwork =
      filter
        (\a -> aaNetworkId a /= netId)
        (Map.keys . unWithdrawals $ txb ^. withdrawalsTxBodyL)

-- | Ensure that value consumed and produced matches up exactly
--
-- > consumed pp utxo txb = produced pp poolParams txb
validateValueNotConservedUTxO ::
  (EraUTxO era, EraCertState era) =>
  PParams era ->
  UTxO era ->
  CertState era ->
  TxBody TopTx era ->
  Test (ShelleyUtxoPredFailure era)
validateValueNotConservedUTxO pp utxo certState txBody =
  failureUnless (consumedValue == producedValue) $
    ValueNotConservedUTxO Mismatch {mismatchSupplied = consumedValue, mismatchExpected = producedValue}
  where
    consumedValue = consumed pp certState utxo txBody
    producedValue = produced pp certState txBody

-- | Ensure there are no `TxOut`s that have less than @minUTxOValue@
--
-- > ∀(_ → (_, c)) ∈ txouts txb, c ≥ (minUTxOValue pp)
validateOutputTooSmallUTxO ::
  (EraTxOut era, Foldable f) =>
  PParams era ->
  f (TxOut era) ->
  Test (ShelleyUtxoPredFailure era)
validateOutputTooSmallUTxO pp outputs =
  failureOnNonEmpty outputsTooSmall OutputTooSmallUTxO
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
  failureOnNonEmpty outputsAttrsTooBig OutputBootAddrAttrsTooBig
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
  Tx l era ->
  Test (ShelleyUtxoPredFailure era)
validateMaxTxSizeUTxO pp tx =
  failureUnless (txSize <= maxTxSize) $
    MaxTxSizeUTxO
      Mismatch
        { mismatchSupplied = txSize
        , mismatchExpected = maxTxSize
        }
  where
    maxTxSize = pp ^. ppMaxTxSizeL
    txSize = tx ^. sizeTxF

-- | This monadic action captures the final stages of the UTXO(S) rule. In particular it
-- applies all of the UTxO related aditions and removals, gathers all of the fees into the
-- fee pot `utxosFees` and updates the `utxosDeposited` field. Continuation supplied will
-- be called on the @deposit - refund@ change, which is normally used to emit the
-- `TotalDeposits` event.
updateUTxOState ::
  (EraTxBody era, EraStake era, EraCertState era, Monad m) =>
  PParams era ->
  UTxOState era ->
  TxBody TopTx era ->
  CertState era ->
  GovState era ->
  (Coin -> m ()) ->
  (UTxO era -> UTxO era -> m ()) ->
  m (UTxOState era)
updateUTxOState pp utxos txBody certState govState depositChangeEvent txUtxODiffEvent = do
  let UTxOState {utxosUtxo, utxosDeposited, utxosFees, utxosDonation} = utxos
      UTxO utxo = utxosUtxo
      !utxoAdd = txouts txBody -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(utxoWithout, utxoDel) = extractKeys utxo (txBody ^. inputsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `Map.union` unUTxO utxoAdd
      deletedUTxO = UTxO utxoDel
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
      , utxosInstantStake =
          deleteInstantStake deletedUTxO (addInstantStake utxoAdd (utxos ^. instantStakeL))
      , utxosDonation = utxosDonation
      }

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , EraRuleFailure "PPUP" era ~ ShelleyPpupPredFailure era
  , Event (EraRule "PPUP" era) ~ PpupEvent era
  ) =>
  Embed (ShelleyPPUP era) (ShelleyUTXO era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent

validSizeComputationCheck ::
  ( EraTx era
  , SafeToHash (TxWits era)
  , Signal (rule era) ~ Tx TopTx era
  ) =>
  Assertion (rule era)
validSizeComputationCheck =
  PreCondition
    "Tx size should be the length of the serialization bytestring"
    ( \(TRC (_, _, tx)) ->
        tx ^. sizeTxF == sizeTxForFeeCalculation tx
    )
