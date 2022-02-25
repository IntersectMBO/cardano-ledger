{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

module Cardano.Ledger.Babbage.Rules.Utxo where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Data (DataHash)
import Cardano.Ledger.Alonzo.Rules.Utxo
  ( UtxoEvent (..),
    UtxoPredicateFailure (..),
    utxoPredFailMaToAlonzo,
    utxoPredFailShelleyToAlonzo,
    vKeyLocked,
    validateExUnitsTooBigUTxO,
    validateOutputTooBigUTxO,
    validateOutputTooSmallUTxO,
    validateOutsideForecast,
    validateTooManyCollateralInputs,
    validateWrongNetworkInTxBody,
  )
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (WrappedShelleyEraFailure))
import Cardano.Ledger.Alonzo.Scripts (Prices)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..), minfee, txouts)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..), nullRedeemers)
import Cardano.Ledger.Babbage.Collateral (collBalance, minCollateral)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Utxos (BabbageUTXOS, ConcreteBabbage)
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    collateralInputs',
    collateralReturn',
    totalCollateral',
    txfee',
  )
import Cardano.Ledger.BaseTypes
  ( ShelleyBase,
    epochInfoWithErr,
    networkId,
    systemStart,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Rules.ValidationMode
  ( Inject (..),
    Test,
    runTest,
    runTestOnSignal,
  )
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure)
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as ShelleyMA
  ( UtxoPredicateFailure,
    validateOutsideValidityIntervalUTxO,
    validateTriesToForgeADA,
    validateValueNotConservedUTxO,
  )
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Coders
import Data.Coerce (coerce)
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (sequenceA_)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Natural (Natural)
import GHC.Records (HasField (getField))
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation

-- ======================================================

data BabbageUTXO era

-- | Predicate failure for the Babbage Era
data BabbageUtxoPred era
  = FromAlonzoUtxoFail !(UtxoPredicateFailure era) -- Inherited from Alonzo
  | FromAlonzoUtxowFail !(UtxowPredicateFail era)
  | UnequalCollateralReturn !Coin !Coin
  | UnknownDataHash !(Set.Set (DataHash (Crypto era)))
  | DanglingWitnessDataHash !(Set.Set (DataHash (Crypto era)))

deriving instance
  ( Era era,
    Show (UtxoPredicateFailure era),
    Show (PredicateFailure (Core.EraRule "UTXO" era)),
    Show (Core.Script era)
  ) =>
  Show (BabbageUtxoPred era)

deriving instance
  ( Era era,
    Eq (UtxoPredicateFailure era),
    Eq (PredicateFailure (Core.EraRule "UTXO" era)),
    Eq (Core.Script era)
  ) =>
  Eq (BabbageUtxoPred era)

-- ===============================================
-- Inject instances

instance Inject (UtxoPredicateFailure era) (BabbageUtxoPred era) where
  inject = FromAlonzoUtxoFail

instance Inject (UtxowPredicateFail era) (BabbageUtxoPred era) where
  inject = FromAlonzoUtxowFail

instance Inject (BabbageUtxoPred era) (BabbageUtxoPred era) where
  inject = id

instance
  Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)) =>
  Inject (ShelleyMA.UtxoPredicateFailure era) (BabbageUtxoPred era)
  where
  inject = FromAlonzoUtxoFail . utxoPredFailMaToAlonzo

instance
  Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)) =>
  Inject (Shelley.UtxoPredicateFailure era) (BabbageUtxoPred era)
  where
  inject = FromAlonzoUtxoFail . utxoPredFailShelleyToAlonzo

instance Inject (UtxowPredicateFailure era) (BabbageUtxoPred era) where
  inject = FromAlonzoUtxowFail . WrappedShelleyEraFailure

-- =======================================================

-- | feesOK can differ from Era to Era, as new notions of fees arise. This is the Babbage version
--   See: Figure 2: Functions related to fees and collateral, in the Babbage specification
--   In the spec feesOK is a boolean function. Because wee need to handle predicate failures
--   in the implementaion, it is coded as a Test. Which is a validation.
--   This version is generic in that it can be lifted to any PredicateFailure type that
--   embeds BabbageUtxoPred era. This makes it possibly useful in future Eras.
feesOK ::
  forall era.
  ( ValidateScript era, -- isTwoPhaseScriptAddress
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    Core.TxBody era ~ TxBody era,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "_collateralPercentage" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Test (BabbageUtxoPred era)
feesOK pp tx (UTxO utxo) = do
  let txb = getField @"body" tx
      theFee = txfee' txb -- Coin supplied to pay fees
      minimumFee = minfee @era pp tx
  -- Part 1  (minfee pp tx ≤ txfee tx )
  sequenceA_
    [ failureUnless (minimumFee <= theFee) (inject (FeeTooSmallUTxO @era minimumFee theFee)),
      -- Part 2 (txrdmrs tx /= ◇ ⇒ ... )
      if (nullRedeemers . txrdmrs' . wits $ tx)
        then pure ()
        else
          let valbalance = collBalance txb (UTxO utxo) -- Collateral as Value
              coinbalance = Val.coin valbalance -- Collateral as a Coin
              collat = collateralInputs' txb SplitMap.◁ utxo
           in -- collat is the UTxO restricted to the inputs allocated to pay the Fee
              sequenceA_
                [ -- Part 3 ((∀(a, , ) ∈ range (collInputs txb ◁ utxo), paymentHK a ∈ Addr^{vkey})
                  failureUnless
                    (all vKeyLocked collat)
                    (inject (ScriptsNotPaidUTxO (UTxO (SplitMap.filter (not . vKeyLocked) collat)))),
                  -- Part 4 (adaOnly balance)
                  failureUnless (Val.adaOnly valbalance) (inject (CollateralContainsNonADA @era valbalance)),
                  -- Part 5 (balance ≥ minCollateral tx pp)
                  failureUnless
                    (coinbalance >= minCollateral txb pp)
                    (inject (InsufficientCollateral @era coinbalance (minCollateral txb pp))),
                  -- Part 6 ((txcoll tx 6 = 3) ⇒ balance = txcoll tx)
                  case collateralReturn' txb of
                    SNothing -> pure ()
                    SJust _txout -> failureUnless (coinbalance == total) (inject (UnequalCollateralReturn @era coinbalance total))
                      where
                        total = totalCollateral' txb,
                  -- Part 7 (collInputs tx 6 = ∅)
                  failureUnless (not (Set.null (collateralInputs' txb))) (inject (NoCollateralInputs @era))
                ]
    ]

-- ========================================================

-- | The UTxO transition rule for the Babbage eras.
utxoTransition ::
  forall era.
  ( Era era,
    ValidateScript era,
    ConcreteBabbage era, -- Unlike the Tests, we are only going to use this once, so we fix the Core.XX types
    STS (BabbageUTXO era),
    -- In this function we we call the UTXOS rule, so we need some assumptions
    Embed (Core.EraRule "UTXOS" era) (BabbageUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  TransitionRule (BabbageUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _deposits _fees _ppup _ = u

  {-   txb := txbody tx   -}
  let txb = body tx
      allInputs =
        Set.unions
          [ getField @"inputs" txb,
            getField @"collateral" txb,
            getField @"referenceInputs" txb -- NEW TO Babbage UTXO rule
          ]

  {- ininterval slot (txvld txb) -}
  runTest $
    ShelleyMA.validateOutsideValidityIntervalUTxO slot txb

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfoWithErr

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ validateOutsideForecast ei sysSt tx

  {-   txins txb ≠ ∅   -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txb

  {-   feesOK pp tx utxo   -}
  runTest $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb -}
  {- (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo   -}
  runTest $
    Shelley.validateBadInputsUTxO utxo allInputs

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $
    ShelleyMA.validateValueNotConservedUTxO pp utxo stakepools txb

  {-   adaID ∉ supp mint tx   -}
  runTestOnSignal $
    ShelleyMA.validateTriesToForgeADA txb

  let outs = txouts txb
  {-   ∀ txout ∈ txouts txb, getValuetxout ≥ inject (uxoEntrySizetxout ∗ coinsPerUTxOWord p) -}
  runTest $ validateOutputTooSmallUTxO pp outs

  {-   ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ validateOutputTooBigUTxO pp outs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $
    Shelley.validateOutputBootAddrAttrsTooBig outs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId txb

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
-- BabbageUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( ValidateScript era,
    Era era,
    ValidateScript era,
    ConcreteBabbage era, -- Unlike the Tests, we are only going to use this once, so we fix the Core.XX types
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    -- instructions for calling UTXOS from BabbageUTXO
    Embed (Core.EraRule "UTXOS" era) (BabbageUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)),
    PredicateFailure (Core.EraRule "UTXO" era) ~ BabbageUtxoPred era
  ) =>
  STS (BabbageUTXO era)
  where
  type State (BabbageUTXO era) = Shelley.UTxOState era
  type Signal (BabbageUTXO era) = ValidatedTx era
  type Environment (BabbageUTXO era) = Shelley.UtxoEnv era
  type BaseM (BabbageUTXO era) = ShelleyBase
  type PredicateFailure (BabbageUTXO era) = BabbageUtxoPred era
  type Event (BabbageUTXO era) = UtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era,
    STS (BabbageUTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ UtxosPredicateFailure era,
    Event (Core.EraRule "UTXOS" era) ~ Event (BabbageUTXOS era)
  ) =>
  Embed (BabbageUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = FromAlonzoUtxoFail . UtxosFailure
  wrapEvent = UtxosEvent

-- ============================================
-- CBOR for Predicate faiure type

instance
  ( Era era,
    Typeable era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era)),
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    ToCBOR (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  ToCBOR (BabbageUtxoPred era)
  where
  toCBOR pf = encode (work pf)
    where
      work (FromAlonzoUtxoFail x) = Sum FromAlonzoUtxoFail 1 !> To x
      work (FromAlonzoUtxowFail x) = Sum FromAlonzoUtxowFail 2 !> To x
      work (UnequalCollateralReturn c1 c2) = Sum UnequalCollateralReturn 3 !> To c1 !> To c2
      work (UnknownDataHash x) = Sum UnknownDataHash 4 !> To x
      work (DanglingWitnessDataHash x) = Sum DanglingWitnessDataHash 5 !> To x

instance
  ( Era era,
    Typeable era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era)),
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  FromCBOR (BabbageUtxoPred era)
  where
  fromCBOR = decode (Summands "BabbageUtxoPred" work)
    where
      work 1 = SumD FromAlonzoUtxoFail <! From
      work 2 = SumD FromAlonzoUtxowFail <! From
      work 3 = SumD UnequalCollateralReturn <! From <! From
      work 4 = SumD UnknownDataHash <! From
      work 5 = SumD DanglingWitnessDataHash <! From
      work n = Invalid n

deriving via InspectHeapNamed "BabbageUtxoPred" (BabbageUtxoPred era) instance NoThunks (BabbageUtxoPred era)
