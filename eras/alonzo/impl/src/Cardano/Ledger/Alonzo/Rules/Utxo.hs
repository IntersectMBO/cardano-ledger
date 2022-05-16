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

module Cardano.Ledger.Alonzo.Rules.Utxo where

-- Inject instances
import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize)
import Cardano.Ledger.Address (Addr (..), RewardAcnt)
-- we need the fields
import Cardano.Ledger.Alonzo.Data (DataHash, dataHashSize)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (ConcreteAlonzo, UTXOS, UtxosPredicateFailure)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), pointWiseExUnits)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..), totExUnits)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (ValidatedTx)
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo (TxSeq)
import Cardano.Ledger.Alonzo.TxWitness (Redeemers, TxWitness (txrdmrs'), nullRedeemers)
import Cardano.Ledger.BaseTypes
  ( Network,
    ProtVer,
    ShelleyBase,
    StrictMaybe (..),
    epochInfoWithErr,
    networkId,
    systemStart,
  )
import Cardano.Ledger.Coin
import Cardano.Ledger.CompactAddress
  ( CompactAddr,
    isBootstrapCompactAddr,
    isPayCredScriptCompactAddr,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Rules.ValidationMode
  ( Inject (..),
    InjectMaybe (..),
    Test,
    mapMaybeValidation,
    runTest,
    runTestOnSignal,
  )
import Cardano.Ledger.Shelley.HardForks (allowOutsideForecastTTL)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import Cardano.Ledger.Shelley.Tx (TxIn)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, txouts)
import Cardano.Ledger.ShelleyMA.Rules.Utxo ()
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as ShelleyMA
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API (EpochInfo, epochInfoSlotToUTCTime)
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Slotting.Time (SystemStart)
import Control.Monad (unless)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Coders
  ( Decode (..),
    Encode (..),
    Wrapped (Open),
    decode,
    decodeList,
    decodeSet,
    decodeSplitMap,
    encode,
    encodeFoldable,
    (!>),
    (<!),
  )
import Data.Coerce (coerce)
import qualified Data.Compact.SplitMap as SplitMap
import Data.Either (isRight)
import Data.Foldable (foldl', sequenceA_, toList)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Validation

-- | Compute an estimate of the size of storing one UTxO entry.
-- This function implements the UTxO entry size estimate done by scaledMinDeposit in the ShelleyMA era
utxoEntrySize ::
  ( Era era,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash c))
  ) =>
  Core.TxOut era ->
  Integer
utxoEntrySize txout = utxoEntrySizeWithoutVal + Val.size v + dataHashSize dh
  where
    v = getField @"value" txout
    dh = getField @"datahash" txout
    -- lengths obtained from tracing on HeapWords of inputs and outputs
    -- obtained experimentally, and number used here
    -- units are Word64s

    -- size of UTxO entry excluding the Value part
    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 27 -- 6 + txoutLenNoVal [14] + txinLen [7]

-- ============================================

-- | The uninhabited type that marks the Alonzo UTxO rule
data AlonzoUTXO era

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
      !(Core.Value era)
      -- ^ the Coin consumed by this transaction
      !(Core.Value era)
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
      ![Core.TxOut era]
  | -- | Subtransition Failures
    UtxosFailure (PredicateFailure (Core.EraRule "UTXOS" era))
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      ![Core.TxOut era]
  | TriesToForgeADA
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      ![(Integer, Integer, Core.TxOut era)]
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
    CollateralContainsNonADA !(Core.Value era)
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
    Show (Core.Value era),
    Show (Core.TxOut era),
    Show (Core.TxBody era),
    Show (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Eq (Core.Value era),
    Eq (Core.TxOut era),
    Eq (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Eq (UtxoPredicateFailure era)

instance
  ( Era era,
    ToCBOR (Core.Value era),
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.TxBody era),
    NoThunks (Core.Value era),
    NoThunks (Core.TxOut era),
    NoThunks (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  NoThunks (UtxoPredicateFailure era)

newtype UtxoEvent era
  = UtxosEvent (Event (Core.EraRule "UTXOS" era))

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

vKeyLocked :: Era era => Core.TxOut era -> Bool
vKeyLocked txOut =
  case getTxOutEitherAddr txOut of
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
  ( Era era,
    Core.Tx era ~ Alonzo.ValidatedTx era,
    -- "collateral" to get inputs to pay the fees
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_collateralPercentage" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
feesOK pp tx (UTxO utxo) =
  let txb = getField @"body" tx
      collateral = getField @"collateral" txb -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      -- (collateral ◁ utxo)
      utxoCollateral = collateral SplitMap.◁ utxo
      bal = balance @era (UTxO utxoCollateral)
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txb
          mapMaybeValidation fromShelleyFailure $ Shelley.validateFeeTooSmallUTxO pp tx,
          -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers . txrdmrs' . wits $ tx) $
            validateCollateral pp txb utxoCollateral bal
        ]

validateCollateral ::
  ( Era era,
    HasField "_collateralPercentage" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Core.TxBody era ->
  SplitMap.SplitMap (TxIn (Crypto era)) (Core.TxOut era) ->
  Core.Value era ->
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
  Era era =>
  SplitMap.SplitMap (TxIn (Crypto era)) (Core.TxOut era) ->
  Test (UtxoPredicateFailure era)
validateScriptsNotPaidUTxO utxoCollateral =
  failureUnless (all vKeyLocked utxoCollateral) $
    ScriptsNotPaidUTxO (UTxO (SplitMap.filter (not . vKeyLocked) utxoCollateral))

-- > balance ∗ 100 ≥ txfee txb ∗ (collateralPercent pp)
validateInsufficientCollateral ::
  ( HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "txfee" (Core.TxBody era) Coin,
    Val.Val (Core.Value era)
  ) =>
  Core.PParams era ->
  Core.TxBody era ->
  Core.Value era ->
  Test (UtxoPredicateFailure era)
validateInsufficientCollateral pp txb bal =
  failureUnless (Val.scale (100 :: Int) (Val.coin bal) >= Val.scale collPerc txfee) $
    InsufficientCollateral
      (Val.coin bal)
      (rationalToCoinViaCeiling $ (fromIntegral collPerc * unCoin txfee) % 100)
  where
    txfee = getField @"txfee" txb -- Coin supplied to pay fees
    collPerc = getField @"_collateralPercentage" pp

-- > isAdaOnly balance
validateCollateralContainsNonADA ::
  Val.Val (Core.Value era) =>
  Core.Value era ->
  Test (UtxoPredicateFailure era)
validateCollateralContainsNonADA bal =
  failureUnless (Val.isAdaOnly bal) $ CollateralContainsNonADA bal

-- | If tx has non-native scripts, end of validity interval must translate to time
--
-- > (_,i_f) := txvldt tx
-- > ◇ ∉ { txrdmrs tx, i_f } ⇒ epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇
validateOutsideForecast ::
  ( HasField "vldt" (Core.TxBody era) ValidityInterval,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  Core.PParams era ->
  EpochInfo (Either a) ->
  -- | Current slot number
  SlotNo ->
  SystemStart ->
  ValidatedTx era ->
  Test (UtxoPredicateFailure era)
validateOutsideForecast pp ei slotNo sysSt tx =
  {-   (_,i_f) := txvldt tx   -}
  case getField @"vldt" (body tx) of
    ValidityInterval _ (SJust ifj)
      | not (nullRedeemers (txrdmrs' $ wits tx)) ->
          let ei' =
                if allowOutsideForecastTTL pp
                  then unsafeLinearExtendEpochInfo slotNo ei
                  else ei
           in -- ◇ ∉ { txrdmrs tx, i_f } ⇒
              failureUnless (isRight (epochInfoSlotToUTCTime ei' sysSt ifj)) $ OutsideForecast ifj
    _ -> pure ()

-- | Ensure that there are no `Core.TxOut`s that have value less than the sized @coinsPerUTxOWord@
--
-- > ∀ txout ∈ txouts txb, getValue txout ≥ inject (utxoEntrySize txout ∗ coinsPerUTxOWord pp)
validateOutputTooSmallUTxO ::
  ( HasField "_coinsPerUTxOWord" (Core.PParams era) Coin,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash c)),
    Era era
  ) =>
  Core.PParams era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
validateOutputTooSmallUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    Coin coinsPerUTxOWord = getField @"_coinsPerUTxOWord" pp
    outputsTooSmall =
      filter
        ( \out ->
            let v = getField @"value" out
             in -- pointwise is used because non-ada amounts must be >= 0 too
                not $ Val.pointwise (>=) v (Val.inject $ Coin (utxoEntrySize out * coinsPerUTxOWord))
        )
        (SplitMap.elems outputs)

-- | Ensure that there are no `Core.TxOut`s that have `Value` of size larger
-- than @MaxValSize@. We use serialized length of `Core.Value` because this Value
-- size is being limited inside a serialized `Core.Tx`.
--
-- > ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp
validateOutputTooBigUTxO ::
  ( HasField "_maxValSize" (Core.PParams era) Natural,
    HasField "value" (Core.TxOut era) (Core.Value era),
    ToCBOR (Core.Value era)
  ) =>
  Core.PParams era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
validateOutputTooBigUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooBig) $ OutputTooBigUTxO outputsTooBig
  where
    maxValSize = getField @"_maxValSize" pp
    outputsTooBig = foldl' accum [] $ SplitMap.elems outputs
    accum ans out =
      let v = getField @"value" out
          serSize = fromIntegral $ BSL.length $ serialize v
       in if serSize > maxValSize
            then (fromIntegral serSize, fromIntegral maxValSize, out) : ans
            else ans

-- | Ensure if NetworkId is present in the txbody it matches the global NetworkId
--
-- > (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇)
validateWrongNetworkInTxBody ::
  HasField "txnetworkid" (Core.TxBody era) (StrictMaybe Network) =>
  Network ->
  Core.TxBody era ->
  Test (UtxoPredicateFailure era)
validateWrongNetworkInTxBody netId txb =
  case getField @"txnetworkid" txb of
    SNothing -> pure ()
    SJust bid -> failureUnless (netId == bid) $ WrongNetworkInTxBody netId bid

-- | Ensure that execution units to not exceed the maximum allowed @maxTxExUnits@ parameter.
--
-- > totExunits tx ≤ maxTxExUnits pp
validateExUnitsTooBigUTxO ::
  ( HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "wits" (Core.Tx era) (Core.Witnesses era)
  ) =>
  Core.PParams era ->
  Core.Tx era ->
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
  ( HasField "_maxCollateralInputs" (Core.PParams era) Natural,
    HasField "collateral" (Core.TxBody era) (Set a)
  ) =>
  Core.PParams era ->
  Core.TxBody era ->
  Test (UtxoPredicateFailure era)
validateTooManyCollateralInputs pp txb =
  failureUnless (numColl <= maxColl) $ TooManyCollateralInputs maxColl numColl
  where
    maxColl, numColl :: Natural
    maxColl = getField @"_maxCollateralInputs" pp
    numColl = fromIntegral . Set.size $ getField @"collateral" txb

-- ================================================================

-- | The UTxO transition rule for the Alonzo eras.
utxoTransition ::
  forall era.
  ( Era era,
    ValidateScript era,
    ConcreteAlonzo era, -- Unlike the Tests, we are ony going to use this once, so we fix the Core.XX types
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    STS (AlonzoUTXO era),
    -- instructions for calling UTXOS from AlonzoUTXO
    Embed (Core.EraRule "UTXOS" era) (AlonzoUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  TransitionRule (AlonzoUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _deposits _fees _ppup _ = u

  {-   txb := txbody tx   -}
  let txb = body tx
      inputsAndCollateral =
        Set.union
          (getField @"inputs" txb)
          (getField @"collateral" txb)

  {- ininterval slot (txvld txb) -}
  runTest $
    ShelleyMA.validateOutsideValidityIntervalUTxO slot txb

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfoWithErr

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ validateOutsideForecast pp ei slot sysSt tx

  {-   txins txb ≠ ∅   -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txb

  {-   feesOK pp tx utxo   -}
  runTest $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- inputsAndCollateral = txins txb ∪ collateral txb -}
  {- (txins txb) ∪ (collateral txb) ⊆ dom utxo   -}
  runTest $
    Shelley.validateBadInputsUTxO utxo inputsAndCollateral

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $
    ShelleyMA.validateValueNotConservedUTxO pp utxo stakepools txb

  {-   adaID ∉ supp mint tx   -}
  runTestOnSignal $
    ShelleyMA.validateTriesToForgeADA txb

  let outputs = txouts txb
  {-   ∀ txout ∈ txouts txb, getValuetxout ≥ inject (uxoEntrySizetxout ∗ coinsPerUTxOWord p) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {-   ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ validateOutputTooBigUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $
    Shelley.validateOutputBootAddrAttrsTooBig outputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId . toList $ getField @"outputs" txb

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txb

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ validateWrongNetworkInTxBody netId txb

  {- txsize tx ≤ maxTxSize pp -}
  runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  {-   totExunits tx ≤ maxTxExUnits pp    -}
  runTest $ validateExUnitsTooBigUTxO pp tx

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}
  runTest $ validateTooManyCollateralInputs pp txb

  trans @(Core.EraRule "UTXOS" era) =<< coerce <$> judgmentContext

--------------------------------------------------------------------------------
-- AlonzoUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( ValidateScript era,
    ConcreteAlonzo era, -- Unlike the Tests, we are only going to use this once, so we fix the Core.XX types
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    Embed (Core.EraRule "UTXOS" era) (AlonzoUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ ValidatedTx era,
    Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)),
    Era.TxSeq era ~ Alonzo.TxSeq era
  ) =>
  STS (AlonzoUTXO era)
  where
  type State (AlonzoUTXO era) = Shelley.UTxOState era
  type Signal (AlonzoUTXO era) = ValidatedTx era
  type Environment (AlonzoUTXO era) = Shelley.UtxoEnv era
  type BaseM (AlonzoUTXO era) = ShelleyBase
  type PredicateFailure (AlonzoUTXO era) = UtxoPredicateFailure era
  type Event (AlonzoUTXO era) = UtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era,
    STS (UTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ UtxosPredicateFailure era,
    Event (Core.EraRule "UTXOS" era) ~ Event (UTXOS era)
  ) =>
  Embed (UTXOS era) (AlonzoUTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Typeable era,
    Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  ToCBOR (UtxoPredicateFailure era)
  where
  toCBOR x = encode (encFail x)

encFail ::
  forall era.
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
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
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
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
  let fromRestricted :: (Int, Int, Core.TxOut era) -> (Integer, Integer, Core.TxOut era)
      fromRestricted (sz, mv, txOut) = (toInteger sz, toInteger mv, txOut)
   in SumD OutputTooBigUTxO <! D (map fromRestricted <$> decodeList fromCBOR)
decFail 13 = SumD InsufficientCollateral <! From <! From
decFail 14 =
  SumD ScriptsNotPaidUTxO
    <! D (UTxO <$> decodeSplitMap fromCBOR fromCBOR)
decFail 15 = SumD ExUnitsTooBigUTxO <! From <! From
decFail 16 = SumD CollateralContainsNonADA <! From
decFail 17 = SumD WrongNetworkInTxBody <! From <! From
decFail 18 = SumD OutsideForecast <! From
decFail 19 = SumD TooManyCollateralInputs <! From <! From
decFail 20 = SumD NoCollateralInputs
decFail n = Invalid n

instance
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  FromCBOR (UtxoPredicateFailure era)
  where
  fromCBOR = decode (Summands "UtxoPredicateFailure" decFail)

-- =====================================================
-- Injecting from one PredicateFailure to another

fromShelleyFailure :: Shelley.UtxoPredicateFailure era -> Maybe (UtxoPredicateFailure era)
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
  PredicateFailure (Core.EraRule "UTXOS" era) ~ UtxosPredicateFailure era =>
  Inject (UtxosPredicateFailure era) (UtxoPredicateFailure era)
  where
  inject = UtxosFailure

instance
  Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)) =>
  Inject (ShelleyMA.UtxoPredicateFailure era) (UtxoPredicateFailure era)
  where
  inject = utxoPredFailMaToAlonzo

utxoPredFailMaToAlonzo ::
  Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)) =>
  (ShelleyMA.UtxoPredicateFailure era) ->
  (UtxoPredicateFailure era)
utxoPredFailMaToAlonzo (ShelleyMA.BadInputsUTxO x) = BadInputsUTxO x
utxoPredFailMaToAlonzo (ShelleyMA.OutsideValidityIntervalUTxO vi slotNo) = OutsideValidityIntervalUTxO vi slotNo
utxoPredFailMaToAlonzo (ShelleyMA.MaxTxSizeUTxO x y) = MaxTxSizeUTxO x y
utxoPredFailMaToAlonzo (ShelleyMA.InputSetEmptyUTxO) = InputSetEmptyUTxO
utxoPredFailMaToAlonzo (ShelleyMA.FeeTooSmallUTxO c1 c2) = FeeTooSmallUTxO c1 c2
utxoPredFailMaToAlonzo (ShelleyMA.ValueNotConservedUTxO vc vp) = ValueNotConservedUTxO vc vp
utxoPredFailMaToAlonzo (ShelleyMA.WrongNetwork x y) = WrongNetwork x y
utxoPredFailMaToAlonzo (ShelleyMA.WrongNetworkWithdrawal x y) = WrongNetworkWithdrawal x y
utxoPredFailMaToAlonzo (ShelleyMA.OutputTooSmallUTxO x) = OutputTooSmallUTxO x
utxoPredFailMaToAlonzo (ShelleyMA.UpdateFailure x) = UtxosFailure (inject x)
utxoPredFailMaToAlonzo (ShelleyMA.OutputBootAddrAttrsTooBig xs) = OutputTooBigUTxO (map (\x -> (0, 0, x)) xs)
utxoPredFailMaToAlonzo (ShelleyMA.TriesToForgeADA) = TriesToForgeADA
utxoPredFailMaToAlonzo (ShelleyMA.OutputTooBigUTxO xs) = OutputTooBigUTxO (map (\x -> (0, 0, x)) xs)

instance
  Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)) =>
  Inject (Shelley.UtxoPredicateFailure era) (UtxoPredicateFailure era)
  where
  inject = utxoPredFailShelleyToAlonzo

utxoPredFailShelleyToAlonzo ::
  Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)) =>
  (Shelley.UtxoPredicateFailure era) ->
  (UtxoPredicateFailure era)
utxoPredFailShelleyToAlonzo (Shelley.BadInputsUTxO ins) = BadInputsUTxO ins
utxoPredFailShelleyToAlonzo (Shelley.ExpiredUTxO ttl current) = OutsideValidityIntervalUTxO (ValidityInterval SNothing (SJust ttl)) current
utxoPredFailShelleyToAlonzo (Shelley.MaxTxSizeUTxO a m) = MaxTxSizeUTxO a m
utxoPredFailShelleyToAlonzo (Shelley.InputSetEmptyUTxO) = InputSetEmptyUTxO
utxoPredFailShelleyToAlonzo (Shelley.FeeTooSmallUTxO mf af) = FeeTooSmallUTxO mf af
utxoPredFailShelleyToAlonzo (Shelley.ValueNotConservedUTxO vc vp) = ValueNotConservedUTxO vc vp
utxoPredFailShelleyToAlonzo (Shelley.WrongNetwork n as) = WrongNetwork n as
utxoPredFailShelleyToAlonzo (Shelley.WrongNetworkWithdrawal n as) = WrongNetworkWithdrawal n as
utxoPredFailShelleyToAlonzo (Shelley.OutputTooSmallUTxO x) = OutputTooSmallUTxO x
utxoPredFailShelleyToAlonzo (Shelley.UpdateFailure x) = UtxosFailure (inject x)
utxoPredFailShelleyToAlonzo (Shelley.OutputBootAddrAttrsTooBig outs) = OutputTooBigUTxO (map (\x -> (0, 0, x)) outs)

instance InjectMaybe (Shelley.UtxoPredicateFailure era) (UtxoPredicateFailure era) where
  injectMaybe = fromShelleyFailure

instance InjectMaybe (ShelleyMA.UtxoPredicateFailure era) (UtxoPredicateFailure era) where
  injectMaybe = fromShelleyMAFailure
