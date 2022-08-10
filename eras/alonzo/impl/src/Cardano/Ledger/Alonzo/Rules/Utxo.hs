{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Utxo
  ( AlonzoUTXO,
    UtxoPredicateFailure (..),
    UtxoEvent (..),
    utxoPredFailMaToAlonzo,
    utxoPredFailShelleyToAlonzo,
    validateCollateralContainsNonADA,
    validateExUnitsTooBigUTxO,
    validateInsufficientCollateral,
    validateOutsideForecast,
    validateScriptsNotPaidUTxO,
    validateTooManyCollateralInputs,
    validateWrongNetworkInTxBody,
    utxoEntrySize,
    vKeyLocked,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize)
import Cardano.Ledger.Address (Addr (..), RewardAcnt)
import Cardano.Ledger.Alonzo.Data (dataHashSize)
import Cardano.Ledger.Alonzo.Era (AlonzoUTXO)
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUTXOS, UtxosPredicateFailure)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices, pointWiseExUnits)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), AlonzoTx (..), minfee, totExUnits)
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoEraTxBody (..),
    AlonzoEraTxOut (..),
    ShelleyMAEraTxBody (..),
  )
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq)
import Cardano.Ledger.Alonzo.TxWitness
  ( AlonzoEraWitnesses,
    TxWitness (txrdmrs'),
    nullRedeemers,
  )
import Cardano.Ledger.BaseTypes
  ( Network,
    ProtVer,
    ShelleyBase,
    StrictMaybe (..),
    epochInfo,
    networkId,
    systemStart,
  )
import Cardano.Ledger.Coin
import Cardano.Ledger.CompactAddress
  ( CompactAddr,
    isBootstrapCompactAddr,
    isPayCredScriptCompactAddr,
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Rules.ValidationMode
  ( Inject (..),
    InjectMaybe (..),
    Test,
    runTest,
    runTestOnSignal,
  )
import Cardano.Ledger.Shelley.HardForks (allowOutsideForecastTTL)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoEnv, ShelleyUtxoPredFailure)
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import Cardano.Ledger.Shelley.Tx (TxIn)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, txouts)
import qualified Cardano.Ledger.ShelleyMA.Rules as ShelleyMA
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Slotting.Time (SystemStart)
import Control.Monad (unless)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (◁))
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Coders
  ( Decode (..),
    Encode (..),
    Wrapped (Open),
    decode,
    decodeList,
    decodeMap,
    decodeSet,
    encode,
    encodeFoldable,
    (!>),
    (<!),
  )
import Data.Coerce (coerce)
import Data.Either (isRight)
import Data.Foldable (foldl', sequenceA_, toList)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import Lens.Micro
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Validation

-- | Compute an estimate of the size of storing one UTxO entry.
-- This function implements the UTxO entry size estimate done by scaledMinDeposit in the ShelleyMA era
utxoEntrySize :: AlonzoEraTxOut era => TxOut era -> Integer
utxoEntrySize txOut = utxoEntrySizeWithoutVal + Val.size v + dataHashSize dh
  where
    v = txOut ^. valueTxOutL
    dh = txOut ^. dataHashTxOutL
    -- lengths obtained from tracing on HeapWords of inputs and outputs
    -- obtained experimentally, and number used here
    -- units are Word64s

    -- size of UTxO entry excluding the Value part
    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 27 -- 6 + txoutLenNoVal [14] + txinLen [7]

-- ============================================

-- ==========================================================

data UtxoPredicateFailure era
  = -- | The bad transaction inputs
    BadInputsUTxO
      !(Set (TxIn (Crypto era)))
  | OutsideValidityIntervalUTxO
      !ValidityInterval
      -- ^ transaction's validity interval
      !SlotNo
      -- ^ current slot
  | MaxTxSizeUTxO
      !Integer
      -- ^ the actual transaction size
      !Integer
      -- ^ the max transaction size
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      !Coin
      -- ^ the minimum fee for this transaction
      !Coin
      -- ^ the fee supplied in this transaction
  | ValueNotConservedUTxO
      !(Value era)
      -- ^ the Coin consumed by this transaction
      !(Value era)
      -- ^ the Coin produced by this transaction
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      !Network
      -- ^ the expected network id
      !(Set (Addr (Crypto era)))
      -- ^ the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network
      -- ^ the expected network id
      !(Set (RewardAcnt (Crypto era)))
      -- ^ the set of reward addresses with incorrect network IDs
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO
      ![TxOut era]
  | -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      ![TxOut era]
  | TriesToForgeADA
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      ![(Integer, Integer, TxOut era)]
  | InsufficientCollateral
      !Coin
      -- ^ balance computed
      !Coin
      -- ^ the required collateral for the given fee
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO
      !(UTxO era)
  | ExUnitsTooBigUTxO
      !ExUnits
      -- ^ Max EXUnits from the protocol parameters
      !ExUnits
      -- ^ EXUnits supplied
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA !(Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      !Network
      -- ^ Actual Network ID
      !Network
      -- ^ Network ID in transaction body
  | -- | slot number outside consensus forecast range
    OutsideForecast
      !SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      !Natural
      -- ^ Max allowed collateral inputs
      !Natural
      -- ^ Number of collateral inputs
  | NoCollateralInputs
  deriving (Generic)

deriving stock instance
  ( Era era,
    Show (Value era),
    Show (TxOut era),
    Show (TxBody era),
    Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Eq (Value era),
    Eq (TxOut era),
    Eq (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Eq (UtxoPredicateFailure era)

instance
  ( NoThunks (Value era),
    NoThunks (UTxO era),
    NoThunks (PredicateFailure (EraRule "UTXOS" era)),
    NoThunks (TxOut era)
  ) =>
  NoThunks (UtxoPredicateFailure era)

newtype UtxoEvent era
  = UtxosEvent (Event (EraRule "UTXOS" era))

-- | Returns true for VKey locked addresses, and false for any kind of
-- script-locked address.
isKeyHashAddr :: Addr crypto -> Bool
isKeyHashAddr (AddrBootstrap _) = True
isKeyHashAddr (Addr _ (KeyHashObj _) _) = True
isKeyHashAddr _ = False

-- | This is equivalent to `isKeyHashAddr`, but for compacted version of an address.
isKeyHashCompactAddr :: CompactAddr crypto -> Bool
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
  ( AlonzoEraTx era,
    Tx era ~ AlonzoTx era,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_prices" (PParams era) Prices,
    HasField "_collateralPercentage" (PParams era) Natural
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
feesOK pp tx (UTxO utxo) =
  let txBody = tx ^. bodyTxL
      collateral = txBody ^. collateralInputsTxBodyL -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      utxoCollateral = eval (collateral ◁ utxo)
      bal = balance @era (UTxO utxoCollateral)
      theFee = txBody ^. feeTxBodyL
      minimumFee = minfee @era pp tx
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txb
          failureUnless (minimumFee <= theFee) (inject (FeeTooSmallUTxO @era minimumFee theFee)),
          -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers . txrdmrs' . wits $ tx) $
            validateCollateral pp txBody utxoCollateral bal
        ]

validateCollateral ::
  ( EraTxBody era,
    HasField "_collateralPercentage" (PParams era) Natural
  ) =>
  PParams era ->
  TxBody era ->
  Map.Map (TxIn (Crypto era)) (TxOut era) ->
  Value era ->
  Test (UtxoPredicateFailure era)
validateCollateral pp txb utxoCollateral bal =
  sequenceA_
    [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      validateScriptsNotPaidUTxO utxoCollateral,
      -- Part 4: balance ∗ 100 ≥ txfee txb ∗ (collateralPercent pp)
      validateInsufficientCollateral pp txb bal,
      -- Part 5: isAdaOnly balance
      validateCollateralContainsNonADA bal,
      -- Part 6: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      failureIf (null utxoCollateral) NoCollateralInputs
    ]

-- > (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
validateScriptsNotPaidUTxO ::
  EraTxOut era =>
  Map.Map (TxIn (Crypto era)) (TxOut era) ->
  Test (UtxoPredicateFailure era)
validateScriptsNotPaidUTxO utxoCollateral =
  failureUnless (all vKeyLocked utxoCollateral) $
    ScriptsNotPaidUTxO (UTxO (Map.filter (not . vKeyLocked) utxoCollateral))

-- > balance ∗ 100 ≥ txfee txb ∗ (collateralPercent pp)
validateInsufficientCollateral ::
  ( HasField "_collateralPercentage" (PParams era) Natural,
    EraTxBody era
  ) =>
  PParams era ->
  TxBody era ->
  Value era ->
  Test (UtxoPredicateFailure era)
validateInsufficientCollateral pp txBody bal =
  failureUnless (Val.scale (100 :: Int) (Val.coin bal) >= Val.scale collPerc txfee) $
    InsufficientCollateral
      (Val.coin bal)
      (rationalToCoinViaCeiling $ (fromIntegral collPerc * unCoin txfee) % 100)
  where
    txfee = txBody ^. feeTxBodyL -- Coin supplied to pay fees
    collPerc = getField @"_collateralPercentage" pp

-- > isAdaOnly balance
validateCollateralContainsNonADA ::
  Val.Val (Value era) =>
  Value era ->
  Test (UtxoPredicateFailure era)
validateCollateralContainsNonADA bal =
  failureUnless (Val.isAdaOnly bal) $ CollateralContainsNonADA bal

-- | If tx has non-native scripts, end of validity interval must translate to time
--
-- > (_,i_f) := txvldt tx
-- > ◇ ∉ { txrdmrs tx, i_f } ⇒ epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇
validateOutsideForecast ::
  ( ShelleyMAEraTxBody era,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  PParams era ->
  EpochInfo (Either a) ->
  -- | Current slot number
  SlotNo ->
  SystemStart ->
  AlonzoTx era ->
  Test (UtxoPredicateFailure era)
validateOutsideForecast pp ei slotNo sysSt tx =
  {-   (_,i_f) := txvldt tx   -}
  case body tx ^. vldtTxBodyL of
    ValidityInterval _ (SJust ifj)
      | not (nullRedeemers (txrdmrs' $ wits tx)) ->
          let ei' =
                if allowOutsideForecastTTL pp
                  then unsafeLinearExtendEpochInfo slotNo ei
                  else ei
           in -- ◇ ∉ { txrdmrs tx, i_f } ⇒
              failureUnless (isRight (epochInfoSlotToUTCTime ei' sysSt ifj)) $ OutsideForecast ifj
    _ -> pure ()

-- | Ensure that there are no `TxOut`s that have value less than the sized @coinsPerUTxOWord@
--
-- > ∀ txout ∈ txouts txb, getValue txout ≥ inject (utxoEntrySize txout ∗ coinsPerUTxOWord pp)
validateOutputTooSmallUTxO ::
  ( HasField "_coinsPerUTxOWord" (PParams era) Coin,
    AlonzoEraTxOut era
  ) =>
  PParams era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
validateOutputTooSmallUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    Coin coinsPerUTxOWord = getField @"_coinsPerUTxOWord" pp
    outputsTooSmall =
      filter
        ( \txOut ->
            let v = txOut ^. valueTxOutL
             in -- pointwise is used because non-ada amounts must be >= 0 too
                not $ Val.pointwise (>=) v (Val.inject $ Coin (utxoEntrySize txOut * coinsPerUTxOWord))
        )
        (Map.elems outputs)

-- | Ensure that there are no `TxOut`s that have `Value` of size larger
-- than @MaxValSize@. We use serialized length of `Value` because this Value
-- size is being limited inside a serialized `Tx`.
--
-- > ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp
validateOutputTooBigUTxO ::
  ( HasField "_maxValSize" (PParams era) Natural,
    EraTxOut era
  ) =>
  PParams era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
validateOutputTooBigUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooBig) $ OutputTooBigUTxO outputsTooBig
  where
    maxValSize = getField @"_maxValSize" pp
    outputsTooBig = foldl' accum [] $ Map.elems outputs
    accum ans txOut =
      let v = txOut ^. valueTxOutL
          serSize = fromIntegral $ BSL.length $ serialize v
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
  Test (UtxoPredicateFailure era)
validateWrongNetworkInTxBody netId txBody =
  case txBody ^. networkIdTxBodyL of
    SNothing -> pure ()
    SJust bid -> failureUnless (netId == bid) $ WrongNetworkInTxBody netId bid

-- | Ensure that execution units to not exceed the maximum allowed @maxTxExUnits@ parameter.
--
-- > totExunits tx ≤ maxTxExUnits pp
validateExUnitsTooBigUTxO ::
  ( HasField "_maxTxExUnits" (PParams era) ExUnits,
    AlonzoEraWitnesses era,
    EraTx era
  ) =>
  PParams era ->
  Tx era ->
  Test (UtxoPredicateFailure era)
validateExUnitsTooBigUTxO pp tx =
  failureUnless (pointWiseExUnits (<=) totalExUnits maxTxExUnits) $
    ExUnitsTooBigUTxO maxTxExUnits totalExUnits
  where
    maxTxExUnits = getField @"_maxTxExUnits" pp
    -- This sums up the ExUnits for all embedded Plutus Scripts anywhere in the transaction:
    totalExUnits = totExUnits tx

-- | Ensure that number of collaterals does not exceed the allowed @maxCollInputs@ parameter.
--
-- > ‖collateral tx‖  ≤  maxCollInputs pp
validateTooManyCollateralInputs ::
  ( HasField "_maxCollateralInputs" (PParams era) Natural,
    AlonzoEraTxBody era
  ) =>
  PParams era ->
  TxBody era ->
  Test (UtxoPredicateFailure era)
validateTooManyCollateralInputs pp txBody =
  failureUnless (numColl <= maxColl) $ TooManyCollateralInputs maxColl numColl
  where
    maxColl, numColl :: Natural
    maxColl = getField @"_maxCollateralInputs" pp
    numColl = fromIntegral . Set.size $ txBody ^. collateralInputsTxBodyL

-- ================================================================

-- | The UTxO transition rule for the Alonzo eras.
utxoTransition ::
  forall era.
  ( AlonzoEraTx era,
    Tx era ~ AlonzoTx era,
    STS (AlonzoUTXO era),
    -- instructions for calling UTXOS from AlonzoUTXO
    Embed (EraRule "UTXOS" era) (AlonzoUTXO era),
    Environment (EraRule "UTXOS" era) ~ ShelleyUtxoEnv era,
    State (EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (EraRule "UTXOS" era) ~ Tx era,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_maxValSize" (PParams era) Natural,
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_coinsPerUTxOWord" (PParams era) Coin,
    HasField "_maxCollateralInputs" (PParams era) Natural,
    HasField "_collateralPercentage" (PParams era) Natural,
    HasField "_prices" (PParams era) Prices,
    Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  TransitionRule (AlonzoUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _deposits _fees _ppup _ = u

  {-   txb := txbody tx   -}
  let txBody = body tx
      inputsAndCollateral =
        Set.union
          (txBody ^. inputsTxBodyL)
          (txBody ^. collateralInputsTxBodyL)

  {- ininterval slot (txvld txb) -}
  runTest $
    ShelleyMA.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ validateOutsideForecast pp ei slot sysSt tx

  {-   txins txb ≠ ∅   -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txBody

  {-   feesOK pp tx utxo   -}
  runTest $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- inputsAndCollateral = txins txb ∪ collateral txb -}
  {- (txins txb) ∪ (collateral txb) ⊆ dom utxo   -}
  runTest $
    Shelley.validateBadInputsUTxO utxo inputsAndCollateral

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $
    ShelleyMA.validateValueNotConservedUTxO pp utxo stakepools txBody

  {-   adaID ∉ supp mint tx   -}
  runTestOnSignal $
    ShelleyMA.validateTriesToForgeADA txBody

  let outputs = txouts txBody
  {-   ∀ txout ∈ txouts txb, getValuetxout ≥ inject (uxoEntrySizetxout ∗ coinsPerUTxOWord p) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {-   ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ validateOutputTooBigUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $
    Shelley.validateOutputBootAddrAttrsTooBig outputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId . toList $ txBody ^. outputsTxBodyL

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txBody

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ validateWrongNetworkInTxBody netId txBody

  {- txsize tx ≤ maxTxSize pp -}
  runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  {-   totExunits tx ≤ maxTxExUnits pp    -}
  runTest $ validateExUnitsTooBigUTxO pp tx

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}
  runTest $ validateTooManyCollateralInputs pp txBody

  trans @(EraRule "UTXOS" era) =<< coerce <$> judgmentContext

--------------------------------------------------------------------------------
-- AlonzoUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( AlonzoEraTx era,
    Tx era ~ AlonzoTx era,
    TxSeq era ~ AlonzoTxSeq era,
    Show (TxOut era),
    Show (TxBody era),
    Embed (EraRule "UTXOS" era) (AlonzoUTXO era),
    Environment (EraRule "UTXOS" era) ~ ShelleyUtxoEnv era,
    State (EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (EraRule "UTXOS" era) ~ AlonzoTx era,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_maxValSize" (PParams era) Natural,
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    HasField "_coinsPerUTxOWord" (PParams era) Coin,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_maxCollateralInputs" (PParams era) Natural,
    HasField "_collateralPercentage" (PParams era) Natural,
    HasField "_prices" (PParams era) Prices,
    Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (AlonzoUTXO era)
  where
  type State (AlonzoUTXO era) = Shelley.UTxOState era
  type Signal (AlonzoUTXO era) = AlonzoTx era
  type Environment (AlonzoUTXO era) = ShelleyUtxoEnv era
  type BaseM (AlonzoUTXO era) = ShelleyBase
  type PredicateFailure (AlonzoUTXO era) = UtxoPredicateFailure era
  type Event (AlonzoUTXO era) = UtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era,
    STS (AlonzoUTXOS era),
    PredicateFailure (EraRule "UTXOS" era) ~ UtxosPredicateFailure era,
    Event (EraRule "UTXOS" era) ~ Event (AlonzoUTXOS era)
  ) =>
  Embed (AlonzoUTXOS era) (AlonzoUTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Typeable era,
    Era era,
    ToCBOR (TxOut era),
    ToCBOR (Value era),
    ToCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  ToCBOR (UtxoPredicateFailure era)
  where
  toCBOR x = encode (encFail x)

encFail ::
  forall era.
  ( Era era,
    ToCBOR (TxOut era),
    ToCBOR (Value era),
    ToCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  UtxoPredicateFailure era ->
  Encode 'Open (UtxoPredicateFailure era)
encFail (BadInputsUTxO ins) =
  Sum (BadInputsUTxO @era) 0 !> E encodeFoldable ins
encFail (OutsideValidityIntervalUTxO a b) =
  Sum OutsideValidityIntervalUTxO 1 !> To a !> To b
encFail (MaxTxSizeUTxO a b) =
  Sum MaxTxSizeUTxO 2 !> To a !> To b
encFail InputSetEmptyUTxO =
  Sum InputSetEmptyUTxO 3
encFail (FeeTooSmallUTxO a b) =
  Sum FeeTooSmallUTxO 4 !> To a !> To b
encFail (ValueNotConservedUTxO a b) =
  Sum (ValueNotConservedUTxO @era) 5 !> To a !> To b
encFail (OutputTooSmallUTxO outs) =
  Sum (OutputTooSmallUTxO @era) 6 !> E encodeFoldable outs
encFail (UtxosFailure a) =
  Sum (UtxosFailure @era) 7 !> To a
encFail (WrongNetwork right wrongs) =
  Sum (WrongNetwork @era) 8 !> To right !> E encodeFoldable wrongs
encFail (WrongNetworkWithdrawal right wrongs) =
  Sum (WrongNetworkWithdrawal @era) 9 !> To right !> E encodeFoldable wrongs
encFail (OutputBootAddrAttrsTooBig outs) =
  Sum (OutputBootAddrAttrsTooBig @era) 10 !> E encodeFoldable outs
encFail TriesToForgeADA =
  Sum TriesToForgeADA 11
encFail (OutputTooBigUTxO outs) =
  Sum (OutputTooBigUTxO @era) 12 !> E encodeFoldable outs
encFail (InsufficientCollateral a b) =
  Sum InsufficientCollateral 13 !> To a !> To b
encFail (ScriptsNotPaidUTxO a) =
  Sum ScriptsNotPaidUTxO 14 !> To a
encFail (ExUnitsTooBigUTxO a b) =
  Sum ExUnitsTooBigUTxO 15 !> To a !> To b
encFail (CollateralContainsNonADA a) =
  Sum CollateralContainsNonADA 16 !> To a
encFail (WrongNetworkInTxBody a b) =
  Sum WrongNetworkInTxBody 17 !> To a !> To b
encFail (OutsideForecast a) =
  Sum OutsideForecast 18 !> To a
encFail (TooManyCollateralInputs a b) =
  Sum TooManyCollateralInputs 19 !> To a !> To b
encFail NoCollateralInputs =
  Sum NoCollateralInputs 20

decFail ::
  ( Era era,
    FromCBOR (TxOut era),
    FromCBOR (Value era),
    FromCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Word ->
  Decode 'Open (UtxoPredicateFailure era)
decFail 0 = SumD BadInputsUTxO <! D (decodeSet fromCBOR)
decFail 1 = SumD OutsideValidityIntervalUTxO <! From <! From
decFail 2 = SumD MaxTxSizeUTxO <! From <! From
decFail 3 = SumD InputSetEmptyUTxO
decFail 4 = SumD FeeTooSmallUTxO <! From <! From
decFail 5 = SumD ValueNotConservedUTxO <! From <! From
decFail 6 = SumD OutputTooSmallUTxO <! D (decodeList fromCBOR)
decFail 7 = SumD UtxosFailure <! From
decFail 8 = SumD WrongNetwork <! From <! D (decodeSet fromCBOR)
decFail 9 = SumD WrongNetworkWithdrawal <! From <! D (decodeSet fromCBOR)
decFail 10 = SumD OutputBootAddrAttrsTooBig <! D (decodeList fromCBOR)
decFail 11 = SumD TriesToForgeADA
decFail 12 =
  let fromRestricted :: (Int, Int, TxOut era) -> (Integer, Integer, TxOut era)
      fromRestricted (sz, mv, txOut) = (toInteger sz, toInteger mv, txOut)
   in SumD OutputTooBigUTxO <! D (map fromRestricted <$> decodeList fromCBOR)
decFail 13 = SumD InsufficientCollateral <! From <! From
decFail 14 =
  SumD ScriptsNotPaidUTxO
    <! D (UTxO <$> decodeMap fromCBOR fromCBOR)
decFail 15 = SumD ExUnitsTooBigUTxO <! From <! From
decFail 16 = SumD CollateralContainsNonADA <! From
decFail 17 = SumD WrongNetworkInTxBody <! From <! From
decFail 18 = SumD OutsideForecast <! From
decFail 19 = SumD TooManyCollateralInputs <! From <! From
decFail 20 = SumD NoCollateralInputs
decFail n = Invalid n

instance
  ( Era era,
    FromCBOR (TxOut era),
    FromCBOR (Value era),
    FromCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  FromCBOR (UtxoPredicateFailure era)
  where
  fromCBOR = decode (Summands "UtxoPredicateFailure" decFail)

-- =====================================================
-- Injecting from one PredicateFailure to another

fromShelleyFailure :: ShelleyUtxoPredFailure era -> Maybe (UtxoPredicateFailure era)
fromShelleyFailure = \case
  Shelley.BadInputsUTxO ins -> Just $ BadInputsUTxO ins
  Shelley.ExpiredUTxO {} -> Nothing -- Replaced with `OutsideValidityIntervalUTxO` in ShelleyMA
  Shelley.MaxTxSizeUTxO a m -> Just $ MaxTxSizeUTxO a m
  Shelley.InputSetEmptyUTxO -> Just InputSetEmptyUTxO
  Shelley.FeeTooSmallUTxO mf af -> Just $ FeeTooSmallUTxO mf af
  Shelley.ValueNotConservedUTxO {} -> Nothing -- Updated in ShelleyMA
  Shelley.WrongNetwork n as -> Just $ WrongNetwork n as
  Shelley.WrongNetworkWithdrawal n as -> Just $ WrongNetworkWithdrawal n as
  Shelley.OutputTooSmallUTxO {} -> Nothing -- Updated in ShelleyMA
  Shelley.UpdateFailure {} -> Nothing -- Removed
  Shelley.OutputBootAddrAttrsTooBig outs -> Just $ OutputBootAddrAttrsTooBig outs

fromShelleyMAFailure :: ShelleyMA.UtxoPredicateFailure era -> Maybe (UtxoPredicateFailure era)
fromShelleyMAFailure = \case
  ShelleyMA.BadInputsUTxO {} -> Nothing -- Inherited from Shelley
  ShelleyMA.OutsideValidityIntervalUTxO vi slotNo -> Just $ OutsideValidityIntervalUTxO vi slotNo
  ShelleyMA.MaxTxSizeUTxO {} -> Nothing -- Inherited from Shelley
  ShelleyMA.InputSetEmptyUTxO -> Nothing -- Inherited from Shelley
  ShelleyMA.FeeTooSmallUTxO {} -> Nothing -- Inherited from Shelley
  ShelleyMA.ValueNotConservedUTxO vc vp -> Just $ ValueNotConservedUTxO vc vp
  ShelleyMA.WrongNetwork {} -> Nothing -- Inherited from Shelley
  ShelleyMA.WrongNetworkWithdrawal {} -> Nothing -- Inherited from Shelley
  ShelleyMA.OutputTooSmallUTxO {} -> Nothing -- Updated
  ShelleyMA.UpdateFailure {} -> Nothing -- Removed
  ShelleyMA.OutputBootAddrAttrsTooBig {} -> Nothing -- Inherited from Shelley
  ShelleyMA.TriesToForgeADA -> Just TriesToForgeADA
  ShelleyMA.OutputTooBigUTxO {} -> Nothing -- Updated error reporting

instance Inject (UtxoPredicateFailure era) (UtxoPredicateFailure era) where
  inject = id

instance
  PredicateFailure (EraRule "UTXOS" era) ~ UtxosPredicateFailure era =>
  Inject (UtxosPredicateFailure era) (UtxoPredicateFailure era)
  where
  inject = UtxosFailure

instance
  Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)) =>
  Inject (ShelleyMA.UtxoPredicateFailure era) (UtxoPredicateFailure era)
  where
  inject = utxoPredFailMaToAlonzo

utxoPredFailMaToAlonzo ::
  Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)) =>
  ShelleyMA.UtxoPredicateFailure era ->
  UtxoPredicateFailure era
utxoPredFailMaToAlonzo (ShelleyMA.BadInputsUTxO x) = BadInputsUTxO x
utxoPredFailMaToAlonzo (ShelleyMA.OutsideValidityIntervalUTxO vi slotNo) =
  OutsideValidityIntervalUTxO vi slotNo
utxoPredFailMaToAlonzo (ShelleyMA.MaxTxSizeUTxO x y) = MaxTxSizeUTxO x y
utxoPredFailMaToAlonzo ShelleyMA.InputSetEmptyUTxO = InputSetEmptyUTxO
utxoPredFailMaToAlonzo (ShelleyMA.FeeTooSmallUTxO c1 c2) = FeeTooSmallUTxO c1 c2
utxoPredFailMaToAlonzo (ShelleyMA.ValueNotConservedUTxO vc vp) = ValueNotConservedUTxO vc vp
utxoPredFailMaToAlonzo (ShelleyMA.WrongNetwork x y) = WrongNetwork x y
utxoPredFailMaToAlonzo (ShelleyMA.WrongNetworkWithdrawal x y) = WrongNetworkWithdrawal x y
utxoPredFailMaToAlonzo (ShelleyMA.OutputTooSmallUTxO x) = OutputTooSmallUTxO x
utxoPredFailMaToAlonzo (ShelleyMA.UpdateFailure x) = UtxosFailure (inject x)
utxoPredFailMaToAlonzo (ShelleyMA.OutputBootAddrAttrsTooBig xs) =
  OutputTooBigUTxO (map (\x -> (0, 0, x)) xs)
utxoPredFailMaToAlonzo ShelleyMA.TriesToForgeADA = TriesToForgeADA
utxoPredFailMaToAlonzo (ShelleyMA.OutputTooBigUTxO xs) = OutputTooBigUTxO (map (\x -> (0, 0, x)) xs)

instance
  Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)) =>
  Inject (ShelleyUtxoPredFailure era) (UtxoPredicateFailure era)
  where
  inject = utxoPredFailShelleyToAlonzo

utxoPredFailShelleyToAlonzo ::
  Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)) =>
  ShelleyUtxoPredFailure era ->
  UtxoPredicateFailure era
utxoPredFailShelleyToAlonzo (Shelley.BadInputsUTxO ins) = BadInputsUTxO ins
utxoPredFailShelleyToAlonzo (Shelley.ExpiredUTxO ttl current) =
  OutsideValidityIntervalUTxO (ValidityInterval SNothing (SJust ttl)) current
utxoPredFailShelleyToAlonzo (Shelley.MaxTxSizeUTxO a m) = MaxTxSizeUTxO a m
utxoPredFailShelleyToAlonzo Shelley.InputSetEmptyUTxO = InputSetEmptyUTxO
utxoPredFailShelleyToAlonzo (Shelley.FeeTooSmallUTxO mf af) = FeeTooSmallUTxO mf af
utxoPredFailShelleyToAlonzo (Shelley.ValueNotConservedUTxO vc vp) = ValueNotConservedUTxO vc vp
utxoPredFailShelleyToAlonzo (Shelley.WrongNetwork n as) = WrongNetwork n as
utxoPredFailShelleyToAlonzo (Shelley.WrongNetworkWithdrawal n as) = WrongNetworkWithdrawal n as
utxoPredFailShelleyToAlonzo (Shelley.OutputTooSmallUTxO x) = OutputTooSmallUTxO x
utxoPredFailShelleyToAlonzo (Shelley.UpdateFailure x) = UtxosFailure (inject x)
utxoPredFailShelleyToAlonzo (Shelley.OutputBootAddrAttrsTooBig outs) =
  OutputTooBigUTxO (map (\x -> (0, 0, x)) outs)

instance InjectMaybe (ShelleyUtxoPredFailure era) (UtxoPredicateFailure era) where
  injectMaybe = fromShelleyFailure

instance InjectMaybe (ShelleyMA.UtxoPredicateFailure era) (UtxoPredicateFailure era) where
  injectMaybe = fromShelleyMAFailure
