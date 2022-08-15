{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Utxo
  ( BabbageUTXO,
    BabbageUtxoPredFailure (..),
    utxoTransition,
    feesOK,
    validateTotalCollateral,
    validateCollateralEqBalance,
    babbageMinUTxOValue,
    validateOutputTooSmallUTxO,
    validateOutputTooBigUTxO,
    validateOutputBootAddrAttrsTooBig,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize)
import Cardano.Ledger.Address (bootstrapAddressAttrsSize)
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxoEvent (..),
    AlonzoUtxoPredFailure (..),
    AlonzoUtxosPredFailure (..),
    utxoPredFailMaToAlonzo,
    utxoPredFailShelleyToAlonzo,
    validateCollateralContainsNonADA,
    validateExUnitsTooBigUTxO,
    validateInsufficientCollateral,
    validateOutsideForecast,
    validateScriptsNotPaidUTxO,
    validateTooManyCollateralInputs,
    validateWrongNetworkInTxBody,
  )
import Cardano.Ledger.Alonzo.Scripts (ExUnits, Prices)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), minfee)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (collateralInputsTxBodyL))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (allOuts, allSizedOuts))
import Cardano.Ledger.Alonzo.TxWitness (AlonzoEraWitnesses, TxWitness (..), nullRedeemers)
import Cardano.Ledger.Babbage.Collateral (collBalance)
import Cardano.Ledger.Babbage.Era (BabbageUTXO)
import Cardano.Ledger.Babbage.Rules.Utxos (BabbageUTXOS)
import Cardano.Ledger.Babbage.TxBody
  ( BabbageEraTxBody (..),
    BabbageTxBody (..),
    BabbageTxOut,
    txfee',
  )
import Cardano.Ledger.BaseTypes
  ( ProtVer,
    ShelleyBase,
    epochInfo,
    networkId,
    systemStart,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode
  ( Inject (..),
    Test,
    runTest,
    runTestOnSignal,
  )
import Cardano.Ledger.Serialization (Sized (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoEnv, ShelleyUtxoPredFailure)
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Rules (ShelleyMAUtxoPredFailure)
import qualified Cardano.Ledger.ShelleyMA.Rules as ShelleyMA
  ( validateOutsideValidityIntervalUTxO,
    validateTriesToForgeADA,
    validateValueNotConservedUTxO,
  )
import Cardano.Ledger.TxIn (TxIn)
import qualified Cardano.Ledger.Val as Val
import Control.Monad (unless)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (◁))
import Control.State.Transition.Extended
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Coders
import Data.Coerce (coerce)
import Data.Foldable (Foldable (foldl'), sequenceA_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Typeable (Typeable)
import GHC.Natural (Natural)
import GHC.Records (HasField (getField))
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation (Validation, failureIf, failureUnless)

-- ======================================================

-- | Predicate failure for the Babbage Era
data BabbageUtxoPredFailure era
  = AlonzoInBabbageUtxoPredFailure !(AlonzoUtxoPredFailure era) -- Inherited from Alonzo
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      !Coin
      -- ^ collateral provided
      !Coin
      -- ^ collateral amount declared in transaction body
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO
      ![(TxOut era, Coin)]

deriving instance
  ( Era era,
    Show (AlonzoUtxoPredFailure era),
    Show (PredicateFailure (EraRule "UTXO" era)),
    Show (TxOut era),
    Show (Script era)
  ) =>
  Show (BabbageUtxoPredFailure era)

deriving instance
  ( Era era,
    Eq (AlonzoUtxoPredFailure era),
    Eq (PredicateFailure (EraRule "UTXO" era)),
    Eq (TxOut era),
    Eq (Script era)
  ) =>
  Eq (BabbageUtxoPredFailure era)

-- ===============================================
-- Inject instances

instance Inject (AlonzoUtxoPredFailure era) (BabbageUtxoPredFailure era) where
  inject = AlonzoInBabbageUtxoPredFailure

instance Inject (BabbageUtxoPredFailure era) (BabbageUtxoPredFailure era) where
  inject = id

instance
  Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)) =>
  Inject (ShelleyMAUtxoPredFailure era) (BabbageUtxoPredFailure era)
  where
  inject = AlonzoInBabbageUtxoPredFailure . utxoPredFailMaToAlonzo

instance
  Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)) =>
  Inject (ShelleyUtxoPredFailure era) (BabbageUtxoPredFailure era)
  where
  inject = AlonzoInBabbageUtxoPredFailure . utxoPredFailShelleyToAlonzo

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
  forall era.
  ( EraTx era,
    BabbageEraTxBody era,
    AlonzoEraWitnesses era,
    Tx era ~ AlonzoTx era,
    TxBody era ~ BabbageTxBody era,
    TxOut era ~ BabbageTxOut era,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_collateralPercentage" (PParams era) Natural,
    HasField "_prices" (PParams era) Prices
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Test (BabbageUtxoPredFailure era)
feesOK pp tx u@(UTxO utxo) =
  let txBody = tx ^. bodyTxL
      collateral' = txBody ^. collateralInputsTxBodyL -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      utxoCollateral = eval (collateral' ◁ utxo)
      bal = collBalance txBody u
      theFee = txfee' txBody -- Coin supplied to pay fees
      minimumFee = minfee @era pp tx
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txBody
          failureUnless (minimumFee <= theFee) (inject (FeeTooSmallUTxO @era minimumFee theFee)),
          -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers . txrdmrs' . wits $ tx) $
            validateTotalCollateral pp txBody utxoCollateral bal
        ]

validateTotalCollateral ::
  forall era.
  ( EraTxOut era,
    BabbageEraTxBody era,
    HasField "_collateralPercentage" (PParams era) Natural
  ) =>
  PParams era ->
  TxBody era ->
  Map.Map (TxIn (Crypto era)) (TxOut era) ->
  Value era ->
  Test (BabbageUtxoPredFailure era)
validateTotalCollateral pp txBody utxoCollateral bal =
  sequenceA_
    [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      fromAlonzoValidation $ validateScriptsNotPaidUTxO utxoCollateral,
      -- Part 4: isAdaOnly balance
      fromAlonzoValidation $ validateCollateralContainsNonADA @era bal,
      -- Part 5: balance ≥ ⌈txfee txb ∗ (collateralPercent pp) / 100⌉
      fromAlonzoValidation $ validateInsufficientCollateral pp txBody bal,
      -- Part 6: (txcoll tx ≠ ◇) ⇒ balance = txcoll tx
      validateCollateralEqBalance (Val.coin bal) (txBody ^. totalCollateralTxBodyL),
      -- Part 7: collInputs tx ≠ ∅
      fromAlonzoValidation $ failureIf (null utxoCollateral) (NoCollateralInputs @era)
    ]
  where
    fromAlonzoValidation x = first (fmap inject) x

-- > (txcoll tx ≠ ◇) => balance == txcoll tx
validateCollateralEqBalance :: Coin -> StrictMaybe Coin -> Validation (NonEmpty (BabbageUtxoPredFailure era)) ()
validateCollateralEqBalance bal txcoll =
  case txcoll of
    SNothing -> pure ()
    SJust tc -> failureUnless (bal == tc) (IncorrectTotalCollateralField bal tc)

babbageMinUTxOValue ::
  HasField "_coinsPerUTxOByte" (PParams era) Coin =>
  PParams era ->
  Sized (TxOut era) ->
  Coin
babbageMinUTxOValue pp sizedOut =
  Coin $
    fromIntegral (constantOverhead + sizedSize sizedOut) * unCoin (getField @"_coinsPerUTxOByte" pp)
  where
    -- This constant is an approximation of the memory overhead that comes
    -- from TxIn and an entry in the Map data structure:
    --
    -- 160 = 20 words * 8bytes
    --
    -- This means that if:
    --
    --  * 'coinsPerUTxOByte' = 4310
    --  * A simple TxOut with staking and payment credentials with ADA only
    --    amount of 978370 lovelace
    --
    -- we get the size of TxOut to be 67 bytes and the minimum value will come
    -- out to be 978597 lovelace. Also the absolute minimum value will be
    -- 857690, because TxOut without staking address can't be less than 39 bytes
    constantOverhead = 160

-- > getValue txout ≥ inject ( serSize txout ∗ coinsPerUTxOByte pp )
validateOutputTooSmallUTxO ::
  ( EraTxOut era,
    TxOut era ~ BabbageTxOut era,
    HasField "_coinsPerUTxOByte" (PParams era) Coin
  ) =>
  PParams era ->
  [Sized (TxOut era)] ->
  Test (BabbageUtxoPredFailure era)
validateOutputTooSmallUTxO pp outs =
  failureUnless (null outputsTooSmall) $ BabbageOutputTooSmallUTxO outputsTooSmall
  where
    outs' = map (\out -> (sizedValue out, babbageMinUTxOValue pp out)) outs
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

-- > serSize (getValue txout) ≤ maxValSize pp
validateOutputTooBigUTxO ::
  ( HasField "_maxValSize" (PParams era) Natural,
    EraTxOut era
  ) =>
  PParams era ->
  [TxOut era] ->
  Test (AlonzoUtxoPredFailure era)
validateOutputTooBigUTxO pp outs =
  failureUnless (null outputsTooBig) $ OutputTooBigUTxO outputsTooBig
  where
    maxValSize = getField @"_maxValSize" pp
    outputsTooBig = foldl' accum [] outs
    accum ans txOut =
      let v = txOut ^. valueTxOutL
          serSize = fromIntegral $ BSL.length $ serialize v
       in if serSize > maxValSize
            then (fromIntegral serSize, fromIntegral maxValSize, txOut) : ans
            else ans

-- > a ∈ Addr_bootstrap ⇒ bootstrapAttrsSize a ≤ 64
validateOutputBootAddrAttrsTooBig ::
  EraTxOut era =>
  [TxOut era] ->
  Test (AlonzoUtxoPredFailure era)
validateOutputBootAddrAttrsTooBig outs =
  failureUnless (null outputsAttrsTooBig) $ OutputBootAddrAttrsTooBig outputsAttrsTooBig
  where
    outputsAttrsTooBig =
      filter
        ( \txOut ->
            case txOut ^. bootAddrTxOutF of
              Just addr -> bootstrapAddressAttrsSize addr > 64
              _ -> False
        )
        outs

-- | The UTxO transition rule for the Babbage eras.
utxoTransition ::
  forall era.
  ( EraTx era,
    BabbageEraTxBody era,
    AlonzoEraWitnesses era,
    Tx era ~ AlonzoTx era,
    TxBody era ~ BabbageTxBody era,
    TxOut era ~ BabbageTxOut era,
    STS (BabbageUTXO era),
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_maxValSize" (PParams era) Natural,
    HasField "_maxCollateralInputs" (PParams era) Natural,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_collateralPercentage" (PParams era) Natural,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_coinsPerUTxOByte" (PParams era) Coin,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_prices" (PParams era) Prices,
    -- In this function we we call the UTXOS rule, so we need some assumptions
    Embed (EraRule "UTXOS" era) (BabbageUTXO era),
    Environment (EraRule "UTXOS" era) ~ ShelleyUtxoEnv era,
    State (EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (EraRule "UTXOS" era) ~ Tx era,
    Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)),
    ExtendedUTxO era
  ) =>
  TransitionRule (BabbageUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _deposits _fees _ppup _ = u

  {-   txb := txbody tx   -}
  let txBody = body tx
      allInputs = txBody ^. allInputsTxBodyF

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

  {- allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb -}
  {- (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo   -}
  runTest $
    Shelley.validateBadInputsUTxO utxo allInputs

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $
    ShelleyMA.validateValueNotConservedUTxO pp utxo stakepools txBody

  {-   adaID ∉ supp mint tx   -}
  runTestOnSignal $
    ShelleyMA.validateTriesToForgeADA txBody

  {-   ∀ txout ∈ allOuts txb, getValue txout ≥ inject (serSize txout ∗ coinsPerUTxOByte pp) -}
  runTest $ validateOutputTooSmallUTxO pp $ allSizedOuts txBody

  let outs = allOuts txBody
  {-   ∀ txout ∈ allOuts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ validateOutputTooBigUTxO pp outs

  {- ∀ ( _ ↦ (a,_)) ∈ allOuts txb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $
    validateOutputBootAddrAttrsTooBig outs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ allOuts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId outs

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
-- BabbageUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraTx era,
    BabbageEraTxBody era,
    AlonzoEraWitnesses era,
    Tx era ~ AlonzoTx era,
    TxOut era ~ BabbageTxOut era,
    TxBody era ~ BabbageTxBody era,
    TxWits era ~ TxWitness era,
    Show (TxBody era),
    Show (TxOut era),
    Show (Script era),
    Eq (Script era),
    HasField "_maxCollateralInputs" (PParams era) Natural,
    HasField "_coinsPerUTxOByte" (PParams era) Coin,
    HasField "_collateralPercentage" (PParams era) Natural,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_maxValSize" (PParams era) Natural,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_prices" (PParams era) Prices,
    HasField "_protocolVersion" (PParams era) ProtVer,
    -- instructions for calling UTXOS from BabbageUTXO
    Embed (EraRule "UTXOS" era) (BabbageUTXO era),
    Environment (EraRule "UTXOS" era) ~ ShelleyUtxoEnv era,
    State (EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (EraRule "UTXOS" era) ~ Tx era,
    Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era)),
    PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era,
    ExtendedUTxO era
  ) =>
  STS (BabbageUTXO era)
  where
  type State (BabbageUTXO era) = Shelley.UTxOState era
  type Signal (BabbageUTXO era) = AlonzoTx era
  type Environment (BabbageUTXO era) = ShelleyUtxoEnv era
  type BaseM (BabbageUTXO era) = ShelleyBase
  type PredicateFailure (BabbageUTXO era) = BabbageUtxoPredFailure era
  type Event (BabbageUTXO era) = AlonzoUtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era,
    STS (BabbageUTXOS era),
    PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era,
    Event (EraRule "UTXOS" era) ~ Event (BabbageUTXOS era)
  ) =>
  Embed (BabbageUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . UtxosFailure
  wrapEvent = UtxosEvent

-- ============================================
-- CBOR for Predicate faiure type

instance
  ( Era era,
    Typeable era,
    ToCBOR (TxOut era),
    ToCBOR (Value era),
    ToCBOR (PredicateFailure (EraRule "UTXOS" era)),
    ToCBOR (PredicateFailure (EraRule "UTXO" era)),
    ToCBOR (Script era),
    Typeable (AuxiliaryData era)
  ) =>
  ToCBOR (BabbageUtxoPredFailure era)
  where
  toCBOR pf = encode (work pf)
    where
      work (AlonzoInBabbageUtxoPredFailure x) = Sum AlonzoInBabbageUtxoPredFailure 1 !> To x
      work (IncorrectTotalCollateralField c1 c2) = Sum IncorrectTotalCollateralField 2 !> To c1 !> To c2
      work (BabbageOutputTooSmallUTxO x) = Sum BabbageOutputTooSmallUTxO 3 !> To x

instance
  ( Era era,
    Typeable era,
    FromCBOR (TxOut era),
    FromCBOR (Value era),
    FromCBOR (PredicateFailure (EraRule "UTXOS" era)),
    FromCBOR (PredicateFailure (EraRule "UTXO" era)),
    Typeable (Script era),
    Typeable (AuxiliaryData era)
  ) =>
  FromCBOR (BabbageUtxoPredFailure era)
  where
  fromCBOR = decode (Summands "BabbageUtxoPred" work)
    where
      work 1 = SumD AlonzoInBabbageUtxoPredFailure <! From
      work 2 = SumD IncorrectTotalCollateralField <! From <! From
      work 3 = SumD BabbageOutputTooSmallUTxO <! From
      work n = Invalid n

deriving via
  InspectHeapNamed "BabbageUtxoPred" (BabbageUtxoPredFailure era)
  instance
    NoThunks (BabbageUtxoPredFailure era)
