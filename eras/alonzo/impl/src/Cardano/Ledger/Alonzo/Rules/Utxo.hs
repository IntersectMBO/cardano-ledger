{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Utxo (
  AlonzoUTXO,
  AlonzoUtxoPredFailure (..),
  allegraToAlonzoUtxoPredFailure,
  AlonzoUtxoEvent (..),
  validateCollateralContainsNonADA,
  validateExUnitsTooBigUTxO,
  validateOutputTooBigUTxO,
  validateInsufficientCollateral,
  validateOutsideForecast,
  validateScriptsNotPaidUTxO,
  validateTooManyCollateralInputs,
  validateWrongNetworkInTxBody,
  vKeyLocked,
) where

import Cardano.Ledger.Address (
  Addr (..),
  CompactAddr,
  RewardAccount,
  isBootstrapCompactAddr,
  isPayCredScriptCompactAddr,
 )
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure, shelleyToAllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Era (AlonzoEra, AlonzoUTXO)
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Rules.Ppup ()
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUTXOS, AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), totExUnits)
import Cardano.Ledger.Alonzo.TxBody (
  AllegraEraTxBody (..),
  AlonzoEraTxBody (..),
  AlonzoEraTxOut (..),
  MaryEraTxBody (..),
 )
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), unRedeemersL)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network,
  ProtVer (..),
  Relation (..),
  ShelleyBase,
  StrictMaybe (..),
  epochInfo,
  knownNonZero,
  networkId,
  systemStart,
  (%.),
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), serialize)
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  Wrapped (Open),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (unCoin), DeltaCoin, rationalToCoinViaCeiling, toDeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Rules.ValidationMode (
  Test,
  runTest,
  runTestOnSignal,
 )
import Cardano.Ledger.Shelley.LedgerState (UTxOState (utxosUtxo))
import Cardano.Ledger.Shelley.Rules (ShelleyPpupPredFailure, ShelleyUtxoPredFailure, UtxoEnv (..))
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (◁))
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Coerce (coerce)
import Data.Either (isRight)
import Data.Foldable as F (foldl', sequenceA_, toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Validation

-- ==========================================================

data AlonzoUtxoPredFailure era
  = -- | The bad transaction inputs
    BadInputsUTxO
      (Set TxIn)
  | OutsideValidityIntervalUTxO
      -- | transaction's validity interval
      ValidityInterval
      -- | current slot
      SlotNo
  | MaxTxSizeUTxO (Mismatch 'RelLTEQ Integer)
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO (Mismatch 'RelGTEQ Coin)
  | ValueNotConservedUTxO (Mismatch 'RelEQ (Value era))
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      -- | the expected network id
      Network
      -- | the set of addresses with incorrect network IDs
      (Set Addr)
  | WrongNetworkWithdrawal
      -- | the expected network id
      Network
      -- | the set of reward addresses with incorrect network IDs
      (Set RewardAccount)
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO
      [TxOut era]
  | -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      [TxOut era]
  | -- Kept for backwards compatibility: no longer used because the `MultiAsset` type of mint doesn't allow for this possibility
    TriesToForgeADA
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      [(Integer, Integer, TxOut era)]
  | InsufficientCollateral
      -- | balance computed
      DeltaCoin
      -- | the required collateral for the given fee
      Coin
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO
      (UTxO era)
  | ExUnitsTooBigUTxO (Mismatch 'RelLTEQ ExUnits)
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA (Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody (Mismatch 'RelEQ Network)
  | -- | slot number outside consensus forecast range
    OutsideForecast
      SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs (Mismatch 'RelLTEQ Natural)
  | NoCollateralInputs
  deriving (Generic)

type instance EraRuleFailure "UTXO" AlonzoEra = AlonzoUtxoPredFailure AlonzoEra

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure AlonzoEra

instance InjectRuleFailure "UTXO" ShelleyPpupPredFailure AlonzoEra where
  injectFailure = UtxosFailure . injectFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure AlonzoEra where
  injectFailure = allegraToAlonzoUtxoPredFailure . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" AllegraUtxoPredFailure AlonzoEra where
  injectFailure = allegraToAlonzoUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure AlonzoEra where
  injectFailure = UtxosFailure

deriving stock instance
  ( Era era
  , Show (Value era)
  , Show (TxOut era)
  , Show (TxBody era)
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Show (AlonzoUtxoPredFailure era)

deriving stock instance
  ( Era era
  , Eq (Value era)
  , Eq (TxOut era)
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Eq (AlonzoUtxoPredFailure era)

instance
  ( NoThunks (Value era)
  , NoThunks (UTxO era)
  , NoThunks (PredicateFailure (EraRule "UTXOS" era))
  , NoThunks (TxOut era)
  ) =>
  NoThunks (AlonzoUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (UTxO era)
  , NFData (PredicateFailure (EraRule "UTXOS" era))
  , NFData (TxOut era)
  ) =>
  NFData (AlonzoUtxoPredFailure era)

newtype AlonzoUtxoEvent era
  = UtxosEvent (Event (EraRule "UTXOS" era))
  deriving (Generic)

deriving instance Show (Event (EraRule "UTXOS" era)) => Show (AlonzoUtxoEvent era)

deriving instance Eq (Event (EraRule "UTXOS" era)) => Eq (AlonzoUtxoEvent era)

instance NFData (Event (EraRule "UTXOS" era)) => NFData (AlonzoUtxoEvent era)

-- | Returns true for VKey locked addresses, and false for any kind of
-- script-locked address.
isKeyHashAddr :: Addr -> Bool
isKeyHashAddr (AddrBootstrap _) = True
isKeyHashAddr (Addr _ (KeyHashObj _) _) = True
isKeyHashAddr _ = False

-- | This is equivalent to `isKeyHashAddr`, but for compacted version of an address.
isKeyHashCompactAddr :: CompactAddr -> Bool
isKeyHashCompactAddr cAddr =
  isBootstrapCompactAddr cAddr || not (isPayCredScriptCompactAddr cAddr)

vKeyLocked :: EraTxOut era => TxOut era -> Bool
vKeyLocked txOut =
  case txOut ^. addrEitherTxOutL of
    Left addr -> isKeyHashAddr addr
    Right cAddr -> isKeyHashCompactAddr cAddr

-- | feesOK is a predicate with several parts. Some parts only apply in special circumstances.
--   1) The fee paid is >= the minimum fee
--   2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
--   3) The collateral consists only of VKey addresses
--   4) The collateral is sufficient to cover the appropriate percentage of the
--      fee marked in the transaction
--   5) The collateral inputs do not contain any non-ADA part
--   6) There is at least one collateral input
--   As a TransitionRule it will return (), and produce a validation failure (rather than
--   return) if any of the required parts are False.
feesOK ::
  forall era.
  ( AlonzoEraTx era
  , EraUTxO era
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Test (AlonzoUtxoPredFailure era)
feesOK pp tx u@(UTxO utxo) =
  let txBody = tx ^. bodyTxL
      collateral = txBody ^. collateralInputsTxBodyL -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      utxoCollateral = eval (collateral ◁ utxo)
      theFee = txBody ^. feeTxBodyL
      minFee = getMinFeeTxUtxo pp tx u
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txb
          failureUnless
            (minFee <= theFee)
            (FeeTooSmallUTxO Mismatch {mismatchSupplied = theFee, mismatchExpected = minFee})
        , -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (null $ tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL) $
            validateCollateral pp txBody utxoCollateral
        ]

validateCollateral ::
  ( EraTxBody era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  TxBody era ->
  Map.Map TxIn (TxOut era) ->
  Test (AlonzoUtxoPredFailure era)
validateCollateral pp txb utxoCollateral =
  sequenceA_
    [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      validateScriptsNotPaidUTxO utxoCollateral
    , -- Part 4: balance ∗ 100 ≥ txfee txb ∗ (collateralPercent pp)
      validateInsufficientCollateral pp txb bal
    , -- Part 5: isAdaOnly balance
      validateCollateralContainsNonADA utxoCollateral
    , -- Part 6: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      failureIf (null utxoCollateral) NoCollateralInputs
    ]
  where
    bal = toDeltaCoin $ sumAllCoin utxoCollateral

-- > (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
validateScriptsNotPaidUTxO ::
  EraTxOut era =>
  Map.Map TxIn (TxOut era) ->
  Test (AlonzoUtxoPredFailure era)
validateScriptsNotPaidUTxO utxoCollateral =
  failureUnless (all vKeyLocked utxoCollateral) $
    ScriptsNotPaidUTxO (UTxO (Map.filter (not . vKeyLocked) utxoCollateral))

-- > balance ∗ 100 ≥ txfee txb ∗ (collateralPercent pp)
validateInsufficientCollateral ::
  ( EraTxBody era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  TxBody era ->
  DeltaCoin ->
  Test (AlonzoUtxoPredFailure era)
validateInsufficientCollateral pp txBody bal =
  failureUnless (Val.scale (100 :: Int) bal >= Val.scale collPerc (toDeltaCoin txfee)) $
    InsufficientCollateral bal $
      rationalToCoinViaCeiling $
        (fromIntegral collPerc * unCoin txfee) %. knownNonZero @100
  where
    txfee = txBody ^. feeTxBodyL -- Coin supplied to pay fees
    collPerc = pp ^. ppCollateralPercentageL

-- > isAdaOnly balance
validateCollateralContainsNonADA ::
  (Foldable f, EraTxOut era) =>
  f (TxOut era) ->
  Test (AlonzoUtxoPredFailure era)
validateCollateralContainsNonADA collateralTxOuts =
  failureUnless (areAllAdaOnly collateralTxOuts) $
    CollateralContainsNonADA $
      sumAllValue collateralTxOuts

-- | If tx has non-native scripts, end of validity interval must translate to time
--
-- > (_,i_f) := txvldt tx
-- > ◇ ∉ { txrdmrs tx, i_f } ⇒ epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇
validateOutsideForecast ::
  ( MaryEraTxBody era
  , AlonzoEraTxWits era
  , EraTx era
  ) =>
  EpochInfo (Either a) ->
  -- | Current slot number
  SlotNo ->
  SystemStart ->
  Tx era ->
  Test (AlonzoUtxoPredFailure era)
validateOutsideForecast ei slotNo sysSt tx =
  {-   (_,i_f) := txvldt tx   -}
  case tx ^. bodyTxL . vldtTxBodyL of
    ValidityInterval _ (SJust ifj)
      | not . null $ tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL ->
          let ei' = unsafeLinearExtendEpochInfo slotNo ei
           in -- ◇ ∉ { txrdmrs tx, i_f } ⇒
              failureUnless (isRight (epochInfoSlotToUTCTime ei' sysSt ifj)) $ OutsideForecast ifj
    _ -> pure ()

-- | Ensure that there are no `TxOut`s that have value less than the sized @coinsPerUTxOWord@
--
-- > ∀ txout ∈ txouts txb, getValue txout ≥ inject (utxoEntrySize txout ∗ coinsPerUTxOWord pp)
validateOutputTooSmallUTxO ::
  (AlonzoEraTxOut era, Foldable f) =>
  PParams era ->
  f (TxOut era) ->
  Test (AlonzoUtxoPredFailure era)
validateOutputTooSmallUTxO pp outputs =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    outputsTooSmall =
      filter
        ( \txOut ->
            let v = txOut ^. valueTxOutL
             in -- pointwise is used because non-ada amounts must be >= 0 too
                not $ Val.pointwise (>=) v (Val.inject $ getMinCoinTxOut pp txOut)
        )
        (toList outputs)

-- | Ensure that there are no `TxOut`s that have `Value` of size larger
-- than @MaxValSize@. We use serialized length of `Value` because this Value
-- size is being limited inside a serialized `Tx`.
--
-- > ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp
validateOutputTooBigUTxO ::
  ( EraTxOut era
  , AlonzoEraPParams era
  , Foldable f
  ) =>
  PParams era ->
  f (TxOut era) ->
  Test (AlonzoUtxoPredFailure era)
validateOutputTooBigUTxO pp outputs =
  failureUnless (null outputsTooBig) $ OutputTooBigUTxO outputsTooBig
  where
    maxValSize = pp ^. ppMaxValSizeL
    protVer = pp ^. ppProtocolVersionL
    outputsTooBig = F.foldl' accum [] outputs
    accum ans txOut =
      let v = txOut ^. valueTxOutL
          serSize = fromIntegral $ BSL.length $ serialize (pvMajor protVer) v
       in if serSize > maxValSize
            then (fromIntegral serSize, fromIntegral maxValSize, txOut) : ans
            else ans

-- | Ensure if NetworkId is present in the txbody it matches the global NetworkId
--
-- > (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇)
validateWrongNetworkInTxBody ::
  AlonzoEraTxBody era =>
  Network ->
  TxBody era ->
  Test (AlonzoUtxoPredFailure era)
validateWrongNetworkInTxBody netId txBody =
  case txBody ^. networkIdTxBodyL of
    SNothing -> pure ()
    SJust bid ->
      failureUnless (netId == bid) $
        WrongNetworkInTxBody Mismatch {mismatchSupplied = bid, mismatchExpected = netId}

-- | Ensure that execution units to not exceed the maximum allowed @maxTxExUnits@ parameter.
--
-- > totExunits tx ≤ maxTxExUnits pp
validateExUnitsTooBigUTxO ::
  ( AlonzoEraTxWits era
  , EraTx era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  Tx era ->
  Test (AlonzoUtxoPredFailure era)
validateExUnitsTooBigUTxO pp tx =
  failureUnless (pointWiseExUnits (<=) totalExUnits maxTxExUnits) $
    ExUnitsTooBigUTxO Mismatch {mismatchSupplied = totalExUnits, mismatchExpected = maxTxExUnits}
  where
    maxTxExUnits = pp ^. ppMaxTxExUnitsL
    -- This sums up the ExUnits for all embedded Plutus Scripts anywhere in the transaction:
    totalExUnits = totExUnits tx

-- | Ensure that number of collaterals does not exceed the allowed @maxCollInputs@ parameter.
--
-- > ‖collateral tx‖  ≤  maxCollInputs pp
validateTooManyCollateralInputs ::
  AlonzoEraTxBody era =>
  PParams era ->
  TxBody era ->
  Test (AlonzoUtxoPredFailure era)
validateTooManyCollateralInputs pp txBody =
  failureUnless (numColl <= maxColl) $
    TooManyCollateralInputs Mismatch {mismatchSupplied = numColl, mismatchExpected = maxColl}
  where
    maxColl, numColl :: Natural
    maxColl = pp ^. ppMaxCollateralInputsL
    numColl = fromIntegral . Set.size $ txBody ^. collateralInputsTxBodyL

-- ================================================================

-- | The UTxO transition rule for the Alonzo eras.
utxoTransition ::
  forall era.
  ( EraUTxO era
  , AlonzoEraTx era
  , ProtVerAtMost era 8
  , EraRule "UTXO" era ~ AlonzoUTXO era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , Embed (EraRule "UTXOS" era) (AlonzoUTXO era)
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , EraCertState era
  , SafeToHash (TxWits era)
  ) =>
  TransitionRule (EraRule "UTXO" era)
utxoTransition = do
  TRC (UtxoEnv slot pp dpstate, utxos, tx) <- judgmentContext
  let utxo = utxosUtxo utxos

  {-   txb := txbody tx   -}
  let txBody = tx ^. bodyTxL
      inputsAndCollateral =
        Set.union
          (txBody ^. inputsTxBodyL)
          (txBody ^. collateralInputsTxBodyL)

  {- ininterval slot (txvld txb) -}
  runTest $
    Allegra.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ validateOutsideForecast ei slot sysSt tx

  {-   txins txb ≠ ∅   -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txBody

  {-   feesOK pp tx utxo   -}
  runTest $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- inputsAndCollateral = txins txb ∪ collateral txb -}
  {- (txins txb) ∪ (collateral txb) ⊆ dom utxo   -}
  runTest $ Shelley.validateBadInputsUTxO utxo inputsAndCollateral

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ Shelley.validateValueNotConservedUTxO pp utxo dpstate txBody

  {- adaPolicy ∉ supp mint tx
     above check not needed because mint field of type MultiAsset cannot contain ada -}

  let outputs = txBody ^. outputsTxBodyL
  {-   ∀ txout ∈ txouts txb, getValuetxout ≥ inject (uxoEntrySizetxout ∗ coinsPerUTxOWord p) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {-   ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ validateOutputTooBigUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $ Shelley.validateOutputBootAddrAttrsTooBig outputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId outputs

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txBody

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ validateWrongNetworkInTxBody netId txBody

  {- txsize tx ≤ maxTxSize pp -}
  runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  {-   totExunits tx ≤ maxTxExUnits pp    -}
  runTest $ validateExUnitsTooBigUTxO pp tx

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}

  trans @(EraRule "UTXOS" era) =<< coerce <$> judgmentContext

--------------------------------------------------------------------------------
-- AlonzoUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraUTxO era
  , AlonzoEraTx era
  , Embed (EraRule "UTXOS" era) (AlonzoUTXO era)
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , EraRule "UTXO" era ~ AlonzoUTXO era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , ProtVerAtMost era 8
  , EraCertState era
  , SafeToHash (TxWits era)
  ) =>
  STS (AlonzoUTXO era)
  where
  type State (AlonzoUTXO era) = UTxOState era
  type Signal (AlonzoUTXO era) = Tx era
  type Environment (AlonzoUTXO era) = UtxoEnv era
  type BaseM (AlonzoUTXO era) = ShelleyBase
  type PredicateFailure (AlonzoUTXO era) = AlonzoUtxoPredFailure era
  type Event (AlonzoUTXO era) = AlonzoUtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]
  assertions = [Shelley.validSizeComputationCheck]

instance
  ( Era era
  , STS (AlonzoUTXOS era)
  , PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era
  , Event (EraRule "UTXOS" era) ~ Event (AlonzoUTXOS era)
  ) =>
  Embed (AlonzoUTXOS era) (AlonzoUTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  EncCBOR (AlonzoUtxoPredFailure era)
  where
  encCBOR x = encode (encFail x)

encFail ::
  forall era.
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  AlonzoUtxoPredFailure era ->
  Encode 'Open (AlonzoUtxoPredFailure era)
encFail (BadInputsUTxO ins) =
  Sum (BadInputsUTxO @era) 0 !> To ins
encFail (OutsideValidityIntervalUTxO a b) =
  Sum OutsideValidityIntervalUTxO 1 !> To a !> To b
encFail (MaxTxSizeUTxO m) =
  Sum MaxTxSizeUTxO 2 !> To m
encFail InputSetEmptyUTxO =
  Sum InputSetEmptyUTxO 3
encFail (FeeTooSmallUTxO m) =
  Sum FeeTooSmallUTxO 4 !> To m
encFail (ValueNotConservedUTxO m) =
  Sum (ValueNotConservedUTxO @era) 5 !> To m
encFail (OutputTooSmallUTxO outs) =
  Sum (OutputTooSmallUTxO @era) 6 !> To outs
encFail (UtxosFailure a) =
  Sum (UtxosFailure @era) 7 !> To a
encFail (WrongNetwork right wrongs) =
  Sum (WrongNetwork @era) 8 !> To right !> To wrongs
encFail (WrongNetworkWithdrawal right wrongs) =
  Sum (WrongNetworkWithdrawal @era) 9 !> To right !> To wrongs
encFail (OutputBootAddrAttrsTooBig outs) =
  Sum (OutputBootAddrAttrsTooBig @era) 10 !> To outs
encFail TriesToForgeADA =
  Sum TriesToForgeADA 11
encFail (OutputTooBigUTxO outs) =
  Sum (OutputTooBigUTxO @era) 12 !> To outs
encFail (InsufficientCollateral a b) =
  Sum InsufficientCollateral 13 !> To a !> To b
encFail (ScriptsNotPaidUTxO a) =
  Sum ScriptsNotPaidUTxO 14 !> To a
encFail (ExUnitsTooBigUTxO m) =
  Sum ExUnitsTooBigUTxO 15 !> To m
encFail (CollateralContainsNonADA a) =
  Sum CollateralContainsNonADA 16 !> To a
encFail (WrongNetworkInTxBody m) =
  Sum WrongNetworkInTxBody 17 !> To m
encFail (OutsideForecast a) =
  Sum OutsideForecast 18 !> To a
encFail (TooManyCollateralInputs m) =
  Sum TooManyCollateralInputs 19 !> To m
encFail NoCollateralInputs =
  Sum NoCollateralInputs 20

decFail ::
  ( Era era
  , DecCBOR (TxOut era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Word ->
  Decode 'Open (AlonzoUtxoPredFailure era)
decFail 0 = SumD BadInputsUTxO <! From
decFail 1 = SumD OutsideValidityIntervalUTxO <! From <! From
decFail 2 = SumD MaxTxSizeUTxO <! From
decFail 3 = SumD InputSetEmptyUTxO
decFail 4 = SumD FeeTooSmallUTxO <! From
decFail 5 = SumD ValueNotConservedUTxO <! From
decFail 6 = SumD OutputTooSmallUTxO <! From
decFail 7 = SumD UtxosFailure <! From
decFail 8 = SumD WrongNetwork <! From <! From
decFail 9 = SumD WrongNetworkWithdrawal <! From <! From
decFail 10 = SumD OutputBootAddrAttrsTooBig <! From
decFail 11 = SumD TriesToForgeADA
decFail 12 =
  let fromRestricted :: (Int, Int, TxOut era) -> (Integer, Integer, TxOut era)
      fromRestricted (sz, mv, txOut) = (toInteger sz, toInteger mv, txOut)
   in SumD OutputTooBigUTxO <! D (map fromRestricted <$> decCBOR)
decFail 13 = SumD InsufficientCollateral <! From <! From
decFail 14 = SumD ScriptsNotPaidUTxO <! D (UTxO <$> decCBOR)
decFail 15 = SumD ExUnitsTooBigUTxO <! From
decFail 16 = SumD CollateralContainsNonADA <! From
decFail 17 = SumD WrongNetworkInTxBody <! From
decFail 18 = SumD OutsideForecast <! From
decFail 19 = SumD TooManyCollateralInputs <! From
decFail 20 = SumD NoCollateralInputs
decFail n = Invalid n

instance
  ( Era era
  , DecCBOR (TxOut era)
  , DecCBOR (Value era)
  , EncCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (AlonzoUtxoPredFailure era)
  where
  decCBOR = decode (Summands "UtxoPredicateFailure" decFail)

-- =====================================================
-- Injecting from one PredicateFailure to another

allegraToAlonzoUtxoPredFailure ::
  forall t era.
  ( EraRuleFailure "PPUP" era ~ t era
  , InjectRuleFailure "UTXOS" t era
  ) =>
  AllegraUtxoPredFailure era ->
  AlonzoUtxoPredFailure era
allegraToAlonzoUtxoPredFailure = \case
  Allegra.BadInputsUTxO x -> BadInputsUTxO x
  Allegra.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Allegra.MaxTxSizeUTxO m -> MaxTxSizeUTxO m
  Allegra.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Allegra.FeeTooSmallUTxO m -> FeeTooSmallUTxO m
  Allegra.ValueNotConservedUTxO m -> ValueNotConservedUTxO m
  Allegra.WrongNetwork x y -> WrongNetwork x y
  Allegra.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Allegra.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Allegra.UpdateFailure x -> UtxosFailure (injectFailure @"UTXOS" @t x)
  Allegra.OutputBootAddrAttrsTooBig xs -> OutputTooBigUTxO (map (0,0,) xs)
  Allegra.TriesToForgeADA -> TriesToForgeADA
  Allegra.OutputTooBigUTxO xs -> OutputTooBigUTxO (map (0,0,) xs)
