{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Utxo (
  UTXO,
  BabbageUtxoPredFailure (..),
  babbageUtxoValidation,
  utxoTransition,
  feesOK,
  validateTotalCollateral,
  validateCollateralEqBalance,
  validateOutputTooSmallUTxO,
  disjointRefInputs,
  updateUTxOStateByTxValidity,
) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra, UTXO)
import Cardano.Ledger.Babbage.Rules.Ppup ()
import Cardano.Ledger.Babbage.Rules.Utxos (UTXOS)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  ProtVer (..),
  ShelleyBase,
  epochInfo,
  networkId,
  systemStart,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), Sized (..), natVersion)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), toDeltaCoin)
import Cardano.Ledger.Rules.ValidationMode (
  Test,
  runTest,
  runTestOnSignal,
 )
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val (inject, isAdaOnly, pointwise)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Bifunctor (first)
import Data.Foldable (sequenceA_, toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import Validation (Validation, failureIf, failureUnless)

-- ======================================================

-- | Predicate failure for the Babbage Era
data BabbageUtxoPredFailure era
  = AlonzoInBabbageUtxoPredFailure (Alonzo.AlonzoUtxoPredFailure era) -- Inherited from Alonzo
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      -- | collateral provided
      DeltaCoin
      -- | collateral amount declared in transaction body
      Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO
      (NonEmpty (TxOut era, Coin))
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs
      (NonEmpty TxIn)
  deriving (Generic)

type instance EraRuleFailure "UTXO" BabbageEra = BabbageUtxoPredFailure BabbageEra

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure BabbageEra

instance InjectRuleFailure "UTXO" Alonzo.AlonzoUtxoPredFailure BabbageEra where
  injectFailure = AlonzoInBabbageUtxoPredFailure

instance InjectRuleFailure "UTXO" Shelley.ShelleyPpupPredFailure BabbageEra where
  injectFailure = AlonzoInBabbageUtxoPredFailure . Alonzo.UtxosFailure . injectFailure

instance InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure BabbageEra where
  injectFailure =
    AlonzoInBabbageUtxoPredFailure
      . Alonzo.allegraToAlonzoUtxoPredFailure
      . Allegra.shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure BabbageEra where
  injectFailure = AlonzoInBabbageUtxoPredFailure . Alonzo.allegraToAlonzoUtxoPredFailure

instance InjectRuleFailure "UTXO" Alonzo.AlonzoUtxosPredFailure BabbageEra where
  injectFailure = AlonzoInBabbageUtxoPredFailure . Alonzo.UtxosFailure

deriving instance
  ( Era era
  , Show (Alonzo.AlonzoUtxoPredFailure era)
  , Show (PredicateFailure (EraRule "UTXO" era))
  , Show (TxOut era)
  , Show (Script era)
  , Show TxIn
  ) =>
  Show (BabbageUtxoPredFailure era)

deriving instance
  ( Era era
  , Eq (Alonzo.AlonzoUtxoPredFailure era)
  , Eq (PredicateFailure (EraRule "UTXO" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq TxIn
  ) =>
  Eq (BabbageUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  NFData (BabbageUtxoPredFailure era)

-- =======================================================

-- | feesOK is a predicate with several parts. Some parts only apply in special circumstances.
--   1) The fee paid is >= the minimum fee
--   2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
--   3) The collateral consists only of VKey addresses
--   4) The collateral inputs do not contain any non-ADA part
--   5) The collateral is sufficient to cover the appropriate percentage of the
--      fee marked in the transaction
--   6) The collateral is equivalent to total collateral asserted by the transaction
--   7) There is at least one collateral input
--
--   feesOK can differ from Era to Era, as new notions of fees arise. This is the Babbage version
--   See: Figure 2: Functions related to fees and collateral, in the Babbage specification
--   In the spec feesOK is a boolean function. Because wee need to handle predicate failures
--   in the implementaion, it is coded as a Test. Which is a validation.
--   This version is generic in that it can be lifted to any PredicateFailure type that
--   embeds BabbageUtxoPred era. This makes it possibly useful in future Eras.
feesOK ::
  forall era rule.
  ( EraUTxO era
  , BabbageEraTxBody era
  , AlonzoEraTxWits era
  , InjectRuleFailure rule Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure rule BabbageUtxoPredFailure era
  ) =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  Test (EraRuleFailure rule era)
feesOK pp tx u@(UTxO utxo) =
  let txBody = tx ^. bodyTxL
      collateral = txBody ^. collateralInputsTxBodyL -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      utxoCollateral = Map.restrictKeys utxo collateral
      theFee = txBody ^. feeTxBodyL -- Coin supplied to pay fees
      minFee = getMinFeeTxUtxo pp tx u
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txBody
          failureUnless
            (minFee <= theFee)
            ( injectFailure $
                Alonzo.FeeTooSmallUTxO Mismatch {mismatchSupplied = theFee, mismatchExpected = minFee}
            )
        , -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (null $ tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL) $
            validateTotalCollateral pp txBody utxoCollateral
        ]

-- | Test that inputs and refInpts are disjoint, in Conway and later Eras.
disjointRefInputs ::
  forall era.
  EraPParams era =>
  PParams era ->
  Set TxIn ->
  Set TxIn ->
  Test (BabbageUtxoPredFailure era)
disjointRefInputs pp inputs refInputs =
  when
    ( pvMajor (pp ^. ppProtocolVersionL) > eraProtVerHigh @BabbageEra
        && pvMajor (pp ^. ppProtocolVersionL) < natVersion @11
    )
    (failureOnNonEmpty common BabbageNonDisjointRefInputs)
  where
    common = inputs `Set.intersection` refInputs

validateTotalCollateral ::
  forall era rule.
  ( BabbageEraTxBody era
  , InjectRuleFailure rule Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure rule BabbageUtxoPredFailure era
  ) =>
  PParams era ->
  TxBody TopTx era ->
  Map.Map TxIn (TxOut era) ->
  Test (EraRuleFailure rule era)
validateTotalCollateral pp txBody utxoCollateral =
  sequenceA_
    [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      fromAlonzoValidation $ Alonzo.validateScriptsNotPaidUTxO utxoCollateral
    , -- Part 4: isAdaOnly balance
      fromAlonzoValidation $
        validateCollateralContainsNonADA txBody utxoCollateral
    , -- Part 5: balance ≥ ⌈txfee txb ∗ (collateralPercent pp) / 100⌉
      fromAlonzoValidation $ Alonzo.validateInsufficientCollateral pp txBody bal
    , -- Part 6: (txcoll tx ≠ ◇) ⇒ balance = txcoll tx
      first (fmap injectFailure) $ validateCollateralEqBalance bal (txBody ^. totalCollateralTxBodyL)
    , -- Part 7: collInputs tx ≠ ∅
      fromAlonzoValidation $ failureIf (null utxoCollateral) (Alonzo.NoCollateralInputs @era)
    ]
  where
    bal = collAdaBalance txBody utxoCollateral
    fromAlonzoValidation = first (fmap injectFailure)

-- | This validation produces the same failure as in Alonzo, but is slightly different
-- then the corresponding one in Alonzo, due to addition of the collateral return output:
--
-- 1. Collateral amount can be specified exactly, thus protecting user against unnecessary
-- loss.
--
-- 2. Collateral inputs can contain multi-assets, as long all of them are returned to the
-- `collateralReturnTxBodyL`. This design decision was also intentional, in order to
-- simplify utxo selection for collateral.
validateCollateralContainsNonADA ::
  forall era.
  BabbageEraTxBody era =>
  TxBody TopTx era ->
  Map.Map TxIn (TxOut era) ->
  Test (Alonzo.AlonzoUtxoPredFailure era)
validateCollateralContainsNonADA txBody utxoCollateral =
  failureUnless onlyAdaInCollateral $ Alonzo.CollateralContainsNonADA valueWithNonAda
  where
    onlyAdaInCollateral =
      utxoCollateralAndReturnHaveOnlyAda || allNonAdaIsConsumedByReturn
    -- When we do not have any non-ada TxOuts we can short-circuit the more expensive
    -- validation of NonAda being fully consumed by the return output, which requires
    -- computation of the full balance on the Value, not just the Coin.
    utxoCollateralAndReturnHaveOnlyAda =
      utxoCollateralHasOnlyAda && areAllAdaOnly (txBody ^. collateralReturnTxBodyL)
    utxoCollateralHasOnlyAda = areAllAdaOnly utxoCollateral
    -- Whenever return TxOut consumes all of the NonAda value then the total collateral
    -- balance is considered valid.
    allNonAdaIsConsumedByReturn = Val.isAdaOnly totalCollateralBalance
    -- When reporting failure the NonAda value can be present in either:
    -- - Only in collateral inputs.
    -- - Only in return output, thus we report the return output value, rather than utxo.
    -- - Both inputs and return address, but does not balance out. In which case
    --   we report utxo balance
    valueWithNonAda =
      case txBody ^. collateralReturnTxBodyL of
        SNothing -> collateralBalance
        SJust retTxOut ->
          if utxoCollateralHasOnlyAda
            then retTxOut ^. valueTxOutL
            else collateralBalance
    -- This is the balance that is provided by the collateral inputs
    collateralBalance = sumAllValue utxoCollateral
    -- This is the total amount that will be spent as collateral. This is where we account
    -- for the fact that we can remove Non-Ada assets from collateral inputs, by directing
    -- them to the return TxOut.
    totalCollateralBalance = case txBody ^. collateralReturnTxBodyL of
      SNothing -> collateralBalance
      SJust retTxOut -> collateralBalance <-> (retTxOut ^. valueTxOutL @era)

-- > (txcoll tx ≠ ◇) => balance == txcoll tx
validateCollateralEqBalance ::
  DeltaCoin -> StrictMaybe Coin -> Validation (NonEmpty (BabbageUtxoPredFailure era)) ()
validateCollateralEqBalance bal txcoll =
  case txcoll of
    SNothing -> pure ()
    SJust tc -> failureUnless (bal == toDeltaCoin tc) (IncorrectTotalCollateralField bal tc)

-- > getValue txout ≥ inject ( serSize txout ∗ coinsPerUTxOByte pp )
validateOutputTooSmallUTxO ::
  (EraTxOut era, Foldable f) =>
  PParams era ->
  f (Sized (TxOut era)) ->
  Test (BabbageUtxoPredFailure era)
validateOutputTooSmallUTxO pp outs =
  failureOnNonEmpty outputsTooSmall BabbageOutputTooSmallUTxO
  where
    outs' = map (\out -> (sizedValue out, getMinCoinSizedTxOut pp out)) (toList outs)
    outputsTooSmall =
      filter
        ( \(out, minSize) ->
            let v = out ^. valueTxOutL
             in -- pointwise is used because non-ada amounts must be >= 0 too
                not $
                  Val.pointwise
                    (>=)
                    v
                    (Val.inject minSize)
        )
        outs'

babbageUtxoValidation ::
  forall era.
  ( EraUTxO era
  , BabbageEraTxBody era
  , AlonzoEraTxWits era
  , InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ StAnnTx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , EraCertState era
  ) =>
  Rule (EraRule "UTXO" era) 'Transition ()
babbageUtxoValidation = do
  TRC (Shelley.UtxoEnv slot pp certState, utxos, stAnnTx) <- judgmentContext
  let tx = stAnnTx ^. txStAnnTxG
      utxo = utxosUtxo utxos

  {-   txb := txbody tx   -}
  let txBody = tx ^. bodyTxL
      allInputs = txBody ^. allInputsTxBodyF
      refInputs :: Set TxIn
      refInputs = txBody ^. referenceInputsTxBodyL
      inputs :: Set TxIn
      inputs = txBody ^. inputsTxBodyL

  {- inputs ∩ refInputs = ∅ -}
  runTest $ disjointRefInputs pp inputs refInputs

  {- ininterval slot (txvld txb) -}
  runTest $ Allegra.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ Alonzo.validateOutsideForecast ei slot sysSt tx

  {-   txins txb ≠ ∅   -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txBody

  {-   feesOK pp tx utxo   -}
  validate $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb -}
  {- (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo   -}
  runTest $ Shelley.validateBadInputsUTxO utxo allInputs

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ Shelley.validateValueNotConservedUTxO pp utxo certState txBody

  {-   adaID ∉ supp mint tx - check not needed because mint field of type MultiAsset
   cannot contain ada -}

  {-   ∀ txout ∈ allOuts txb, getValue txout ≥ inject (serSize txout ∗ coinsPerUTxOByte pp) -}
  let allSizedOutputs = txBody ^. allSizedOutputsTxBodyF
  runTest $ validateOutputTooSmallUTxO pp allSizedOutputs

  let allOutputs = fmap sizedValue allSizedOutputs
  {-   ∀ txout ∈ allOuts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ Alonzo.validateOutputTooBigUTxO pp allOutputs

  {- ∀ ( _ ↦ (a,_)) ∈ allOuts txb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $ Shelley.validateOutputBootAddrAttrsTooBig allOutputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ allOuts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId allOutputs

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txBody

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ Alonzo.validateWrongNetworkInTxBody netId txBody

  {- txsize tx ≤ maxTxSize pp -}
  runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  {-   totExunits tx ≤ maxTxExUnits pp    -}
  runTest $ Alonzo.validateExUnitsTooBigUTxO pp tx

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}
  runTest $ Alonzo.validateTooManyCollateralInputs pp txBody

-- | The UTxO transition rule for the Babbage eras.
utxoTransition ::
  forall era.
  ( EraUTxO era
  , BabbageEraTxBody era
  , AlonzoEraTx era
  , EraCertState era
  , EraStake era
  , GovState era ~ ShelleyGovState era
  , InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ StAnnTx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , Event (EraRule "UTXO" era) ~ Alonzo.AlonzoUtxoEvent era
  , STS (EraRule "UTXO" era)
  , -- In this function we call the UTXOS rule, so we need some assumptions
    Embed (EraRule "UTXOS" era) (EraRule "UTXO" era)
  , Environment (EraRule "UTXOS" era) ~ Alonzo.UtxosEnv era
  , State (EraRule "UTXOS" era) ~ ShelleyGovState era
  , Signal (EraRule "UTXOS" era) ~ StAnnTx TopTx era
  ) =>
  TransitionRule (EraRule "UTXO" era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp certState, utxos, stAnnTx) <- judgmentContext
  let tx = stAnnTx ^. txStAnnTxG
  babbageUtxoValidation
  updatedGovState <-
    trans @(EraRule "UTXOS" era) $
      TRC (Alonzo.UtxosEnv slot pp certState, utxosGovState utxos, stAnnTx)
  updateUTxOStateByTxValidity pp certState tx (utxos & utxosGovStateL .~ updatedGovState)

updateUTxOStateByTxValidity ::
  forall era.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , EraStake era
  , EraCertState era
  , Event (EraRule "UTXO" era) ~ Alonzo.AlonzoUtxoEvent era
  ) =>
  PParams era ->
  CertState era ->
  Tx TopTx era ->
  UTxOState era ->
  Rule (EraRule "UTXO" era) 'Transition (UTxOState era)
updateUTxOStateByTxValidity pp certState tx utxoState =
  let txBody = tx ^. bodyTxL
      utxo = utxosUtxo utxoState
   in case tx ^. isValidTxL of
        IsValid True ->
          Shelley.updateUTxOState
            pp
            txBody
            certState
            (tellEvent . Alonzo.TotalDeposits (hashAnnotated txBody))
            (\a b -> tellEvent $ Alonzo.TxUTxODiff a b)
            utxoState
        IsValid False ->
          {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
          {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
          let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
              UTxO collouts = collOuts txBody
              DeltaCoin collateralFees = collAdaBalance txBody utxoDel -- NEW to Babbage
           in pure $!
                utxoState {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
                  { utxosUtxo = UTxO (Map.union utxoKeep collouts) -- NEW to Babbage
                  {- fees + collateralFees -}
                  , utxosFees = utxosFees utxoState <> Coin collateralFees -- NEW to Babbage
                  , utxosInstantStake =
                      deleteInstantStake (UTxO utxoDel) (addInstantStake (UTxO collouts) (utxoState ^. instantStakeL))
                  }

--------------------------------------------------------------------------------
-- UTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , AlonzoEraTx era
  , AlonzoEraTxWits era
  , EraCertState era
  , EraStake era
  , GovState era ~ ShelleyGovState era
  , EraRule "UTXO" era ~ UTXO era
  , InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , -- instructions for calling UTXOS from UTXO
    Embed (EraRule "UTXOS" era) (UTXO era)
  , Environment (EraRule "UTXOS" era) ~ Alonzo.UtxosEnv era
  , State (EraRule "UTXOS" era) ~ ShelleyGovState era
  , Signal (EraRule "UTXOS" era) ~ StAnnTx TopTx era
  , SafeToHash (TxWits era)
  ) =>
  STS (UTXO era)
  where
  type State (UTXO era) = UTxOState era
  type Signal (UTXO era) = StAnnTx TopTx era
  type Environment (UTXO era) = Shelley.UtxoEnv era
  type BaseM (UTXO era) = ShelleyBase
  type PredicateFailure (UTXO era) = BabbageUtxoPredFailure era
  type Event (UTXO era) = Alonzo.AlonzoUtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition @era]
  assertions = [Shelley.validSizeComputationCheck]

instance
  ( Era era
  , STS (UTXOS era)
  , PredicateFailure (EraRule "UTXOS" era) ~ Alonzo.AlonzoUtxosPredFailure era
  , Event (EraRule "UTXOS" era) ~ Event (UTXOS era)
  ) =>
  Embed (UTXOS era) (UTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . Alonzo.UtxosFailure
  wrapEvent = Alonzo.UtxosEvent

-- ============================================
-- CBOR for Predicate faiure type

instance
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  , EncCBOR TxIn
  ) =>
  EncCBOR (BabbageUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      AlonzoInBabbageUtxoPredFailure x -> Sum AlonzoInBabbageUtxoPredFailure 1 !> To x
      IncorrectTotalCollateralField c1 c2 -> Sum IncorrectTotalCollateralField 2 !> To c1 !> To c2
      BabbageOutputTooSmallUTxO x -> Sum BabbageOutputTooSmallUTxO 3 !> To x
      BabbageNonDisjointRefInputs x -> Sum BabbageNonDisjointRefInputs 4 !> To x

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (Script era)
  , Typeable (TxAuxData era)
  ) =>
  DecCBOR (BabbageUtxoPredFailure era)
  where
  decCBOR = decode $ Summands "BabbageUtxoPred" $ \case
    1 -> SumD AlonzoInBabbageUtxoPredFailure <! From
    2 -> SumD IncorrectTotalCollateralField <! From <! From
    3 -> SumD BabbageOutputTooSmallUTxO <! From
    4 -> SumD BabbageNonDisjointRefInputs <! From
    n -> Invalid n
