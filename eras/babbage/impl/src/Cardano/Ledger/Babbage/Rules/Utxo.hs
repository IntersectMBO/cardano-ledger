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

module Cardano.Ledger.Babbage.Rules.Utxo where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize)
import Cardano.Ledger.Address (bootstrapAddressAttrsSize)
import Cardano.Ledger.Alonzo.Rules.Utxo
  ( UtxoEvent (..),
    UtxoPredicateFailure (..),
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
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Scripts (Prices)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..), minfee)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (allOuts, allSizedOuts))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers, TxWitness (..), nullRedeemers)
import Cardano.Ledger.Babbage.Collateral
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Utxos (BabbageUTXOS, ConcreteBabbage)
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    TxOut,
    txfee',
  )
import Cardano.Ledger.BaseTypes
  ( ShelleyBase,
    epochInfo,
    networkId,
    systemStart,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..), ValidateScript (..), getTxOutBootstrapAddress)
import Cardano.Ledger.Rules.ValidationMode
  ( Inject (..),
    Test,
    runTest,
    runTestOnSignal,
  )
import Cardano.Ledger.Serialization (Sized (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as ShelleyMA
  ( UtxoPredicateFailure,
    validateOutsideValidityIntervalUTxO,
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
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Natural (Natural)
import GHC.Records (HasField (getField))
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation (Validation, failureIf, failureUnless)

-- ======================================================

data BabbageUTXO era

-- | Predicate failure for the Babbage Era
data BabbageUtxoPred era
  = FromAlonzoUtxoFail !(UtxoPredicateFailure era) -- Inherited from Alonzo
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      !Coin
      -- ^ collateral provided
      !Coin
      -- ^ collateral amount declared in transaction body
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO
      ![(Core.TxOut era, Coin)]

deriving instance
  ( Era era,
    Show (UtxoPredicateFailure era),
    Show (PredicateFailure (Core.EraRule "UTXO" era)),
    Show (Core.TxOut era),
    Show (Core.Script era)
  ) =>
  Show (BabbageUtxoPred era)

deriving instance
  ( Era era,
    Eq (UtxoPredicateFailure era),
    Eq (PredicateFailure (Core.EraRule "UTXO" era)),
    Eq (Core.TxOut era),
    Eq (Core.Script era)
  ) =>
  Eq (BabbageUtxoPred era)

-- ===============================================
-- Inject instances

instance Inject (UtxoPredicateFailure era) (BabbageUtxoPred era) where
  inject = FromAlonzoUtxoFail

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
  ( Era era,
    Core.Tx era ~ ValidatedTx era,
    Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    -- "collateral" to get inputs to pay the fees
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "collateralReturn" (Core.TxBody era) (StrictMaybe (TxOut era)),
    HasField "_prices" (Core.PParams era) Prices,
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "totalCollateral" (Core.TxBody era) (StrictMaybe Coin)
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Test (BabbageUtxoPred era)
feesOK pp tx u@(UTxO utxo) =
  let txb = getField @"body" tx
      collateral' = getField @"collateral" txb -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      utxoCollateral = eval (collateral' ◁ utxo)
      bal = collBalance txb u
      theFee = txfee' txb -- Coin supplied to pay fees
      minimumFee = minfee @era pp tx
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txb
          failureUnless (minimumFee <= theFee) (inject (FeeTooSmallUTxO @era minimumFee theFee)),
          -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers . txrdmrs' . wits $ tx) $
            validateTotalCollateral pp txb utxoCollateral bal
        ]

validateTotalCollateral ::
  forall era.
  ( Era era,
    HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "totalCollateral" (Core.TxBody era) (StrictMaybe Coin)
  ) =>
  Core.PParams era ->
  Core.TxBody era ->
  Map.Map (TxIn (Crypto era)) (Core.TxOut era) ->
  Core.Value era ->
  Test (BabbageUtxoPred era)
validateTotalCollateral pp txb utxoCollateral bal =
  sequenceA_
    [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      fromAlonzoValidation $ validateScriptsNotPaidUTxO utxoCollateral,
      -- Part 4: isAdaOnly balance
      fromAlonzoValidation $ validateCollateralContainsNonADA @era bal,
      -- Part 5: balance ≥ ⌈txfee txb ∗ (collateralPercent pp) / 100⌉
      fromAlonzoValidation $ validateInsufficientCollateral pp txb bal,
      -- Part 6: (txcoll tx ≠ ◇) ⇒ balance = txcoll tx
      validateCollateralEqBalance (Val.coin bal) (getField @"totalCollateral" txb),
      -- Part 7: collInputs tx ≠ ∅
      fromAlonzoValidation $ failureIf (null utxoCollateral) (NoCollateralInputs @era)
    ]
  where
    fromAlonzoValidation x = first (fmap inject) x

-- > (txcoll tx ≠ ◇) => balance == txcoll tx
validateCollateralEqBalance :: Coin -> StrictMaybe Coin -> Validation (NonEmpty (BabbageUtxoPred era)) ()
validateCollateralEqBalance bal txcoll =
  case txcoll of
    SNothing -> pure ()
    SJust tc -> failureUnless (bal == tc) (IncorrectTotalCollateralField bal tc)

babbageMinUTxOValue ::
  HasField "_coinsPerUTxOByte" (Core.PParams era) Coin =>
  Core.PParams era ->
  Sized (Core.TxOut era) ->
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
  ( Era era,
    HasField "_coinsPerUTxOByte" (Core.PParams era) Coin
  ) =>
  Core.PParams era ->
  [Sized (Core.TxOut era)] ->
  Test (BabbageUtxoPred era)
validateOutputTooSmallUTxO pp outs =
  failureUnless (null outputsTooSmall) $ BabbageOutputTooSmallUTxO outputsTooSmall
  where
    outs' = map (\out -> (sizedValue out, babbageMinUTxOValue pp out)) outs
    outputsTooSmall =
      filter
        ( \(out, minSize) ->
            let v = getField @"value" out
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
  ( HasField "_maxValSize" (Core.PParams era) Natural,
    HasField "value" (Core.TxOut era) (Core.Value era),
    ToCBOR (Core.Value era)
  ) =>
  Core.PParams era ->
  [Core.TxOut era] ->
  Test (UtxoPredicateFailure era)
validateOutputTooBigUTxO pp outs =
  failureUnless (null outputsTooBig) $ OutputTooBigUTxO outputsTooBig
  where
    maxValSize = getField @"_maxValSize" pp
    outputsTooBig = foldl' accum [] outs
    accum ans out =
      let v = getField @"value" out
          serSize = fromIntegral $ BSL.length $ serialize v
       in if serSize > maxValSize
            then (fromIntegral serSize, fromIntegral maxValSize, out) : ans
            else ans

-- > a ∈ Addr_bootstrap ⇒ bootstrapAttrsSize a ≤ 64
validateOutputBootAddrAttrsTooBig ::
  Era era =>
  [Core.TxOut era] ->
  Test (UtxoPredicateFailure era)
validateOutputBootAddrAttrsTooBig outs =
  failureUnless (null outputsAttrsTooBig) $ OutputBootAddrAttrsTooBig outputsAttrsTooBig
  where
    outputsAttrsTooBig =
      filter
        ( \txOut ->
            case getTxOutBootstrapAddress txOut of
              Just addr -> bootstrapAddressAttrsSize addr > 64
              _ -> False
        )
        outs

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
    Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)),
    ExtendedUTxO era
  ) =>
  TransitionRule (BabbageUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _deposits _fees _ppup _ = u

  {-   txb := txbody tx   -}
  let txb = body tx
      allInputs = getAllTxInputs txb

  {- ininterval slot (txvld txb) -}
  runTest $
    ShelleyMA.validateOutsideValidityIntervalUTxO slot txb

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ validateOutsideForecast pp ei slot sysSt tx

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

  {-   ∀ txout ∈ allOuts txb, getValue txout ≥ inject (serSize txout ∗ coinsPerUTxOByte pp) -}
  runTest $ validateOutputTooSmallUTxO pp $ allSizedOuts txb

  let outs = allOuts txb
  {-   ∀ txout ∈ allOuts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ validateOutputTooBigUTxO pp outs

  {- ∀ ( _ ↦ (a,_)) ∈ allOuts txb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $
    validateOutputBootAddrAttrsTooBig outs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ allOuts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId outs

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
    ConcreteBabbage era, -- Unlike the Tests, we are only going to use this once,
    -- so we fix the Core.XX types
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    -- instructions for calling UTXOS from BabbageUTXO
    Embed (Core.EraRule "UTXOS" era) (BabbageUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    Inject (PredicateFailure (Core.EraRule "PPUP" era)) (PredicateFailure (Core.EraRule "UTXOS" era)),
    PredicateFailure (Core.EraRule "UTXO" era) ~ BabbageUtxoPred era,
    ExtendedUTxO era
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
      work (IncorrectTotalCollateralField c1 c2) = Sum IncorrectTotalCollateralField 2 !> To c1 !> To c2
      work (BabbageOutputTooSmallUTxO x) = Sum BabbageOutputTooSmallUTxO 3 !> To x

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
      work 2 = SumD IncorrectTotalCollateralField <! From <! From
      work 3 = SumD BabbageOutputTooSmallUTxO <! From
      work n = Invalid n

deriving via
  InspectHeapNamed "BabbageUtxoPred" (BabbageUtxoPred era)
  instance
    NoThunks (BabbageUtxoPred era)
