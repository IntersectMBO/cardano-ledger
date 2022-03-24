{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Figure 2: Functions related to fees and collateral
--   Babbage Specification
module Cardano.Ledger.Babbage.Collateral where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (NoCollateralInputs, TotalCollateralInequality), fromShelleyFailure, validateCollateralContainsNonADA, validateInsufficientCollateral, validateScriptsNotPaidUTxO)
import Cardano.Ledger.Alonzo.Tx (isTwoPhaseScriptAddressFromMap, wits)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (txscripts))
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (txrdmrs'), nullRedeemers)
import Cardano.Ledger.Babbage.Tx (ValidatedTx)
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    TxOut (..),
    collateralReturn',
    outputs',
    txfee',
  )
import Cardano.Ledger.BaseTypes (txIxFromIntegral)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto), ValidateScript (..))
import Cardano.Ledger.Rules.ValidationMode (Test, mapMaybeValidation)
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Control.Monad (unless)
import Data.Compact.SplitMap ((◁))
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (sequenceA_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import GHC.Records (HasField (..))
import Numeric.Natural (Natural)
import Validation (Validation, failureIf, failureUnless)

-- ============================================================

isTwoPhaseScriptAddress ::
  forall era.
  ( ValidateScript era,
    ExtendedUTxO era
  ) =>
  Core.Tx era ->
  UTxO era ->
  Addr (Crypto era) ->
  Bool
isTwoPhaseScriptAddress tx utxo addr = isTwoPhaseScriptAddressFromMap @era (txscripts utxo tx) addr

minCollateral ::
  HasField "_collateralPercentage" (Core.PParams era) Natural =>
  TxBody era ->
  Core.PParams era ->
  Coin
minCollateral txb pp = Coin ((fee * percent) `divideCeiling` 100)
  where
    fee = unCoin (txfee' txb)
    percent = fromIntegral (getField @"_collateralPercentage" pp)
    divideCeiling x y = if _rem == 0 then n else n + 1 -- Works when both x and y are positive
      where
        (n, _rem) = x `divMod` y

collBalance ::
  forall era.
  ( Era era,
    HasField "collateralReturn" (Core.TxBody era) (StrictMaybe (TxOut era)),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  Core.TxBody era ->
  UTxO era ->
  Core.Value era
collBalance txb (UTxO m) =
  case getField @"collateralReturn" txb of
    SNothing -> colbal
    SJust (TxOut _ retval _ _) -> colbal <-> retval
  where
    col = UTxO (getField @"collateral" txb ◁ m)
    colbal = balance @era col

collOuts ::
  ( Era era,
    Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era
  ) =>
  TxBody era ->
  UTxO era
collOuts txb =
  case collateralReturn' txb of
    SNothing -> UTxO SplitMap.empty
    SJust txout -> UTxO (SplitMap.singleton (TxIn (txid txb) index) txout)
      where
        index = case txIxFromIntegral (length (outputs' txb)) of
          Just i -> i
          Nothing -> error ("length outputs, should always fit in a TxIx")

-- | feesOK is a predicate with several parts. Some parts only apply in special circumstances.
--   1) The fee paid is >= the minimum fee
--   2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
--   3) The collateral consists only of VKey addresses
--   4) The collateral inputs do not contain any non-ADA part
--   5) The collateral is sufficient to cover the appropriate percentage of the
--      fee marked in the transaction
--   6) The collateral is equivalent to total collateral asserted by the transaction
--   7) There is at least one collateral input
--   As a TransitionRule it will return (), and produce a validation failure (rather than
--   return) if any of the required parts are False.
feesOK ::
  forall era.
  ( Era era,
    Core.Tx era ~ ValidatedTx era,
    -- "collateral" to get inputs to pay the fees
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "collateralReturn" (Core.TxBody era) (StrictMaybe (TxOut era)),
    HasField "totalCollateral" (Core.TxBody era) (Core.Value era)
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
feesOK pp tx u@(UTxO utxo) =
  let txb = getField @"body" tx
      collateral' = getField @"collateral" txb -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      -- (collateral ◁ utxo)
      utxoCollateral = collateral' SplitMap.◁ utxo
      bal = collBalance txb u
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txb
          mapMaybeValidation fromShelleyFailure $ Shelley.validateFeeTooSmallUTxO pp tx,
          -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers . txrdmrs' . wits $ tx) $
            validateTotalCollateral pp txb utxoCollateral bal
        ]

validateTotalCollateral ::
  ( Era era,
    HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "totalCollateral" (Core.TxBody era) (Core.Value era)
  ) =>
  Core.PParams era ->
  Core.TxBody era ->
  SplitMap.SplitMap (TxIn (Crypto era)) (Core.TxOut era) ->
  Core.Value era ->
  Test (UtxoPredicateFailure era)
validateTotalCollateral pp txb utxoCollateral bal =
  sequenceA_
    [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      validateScriptsNotPaidUTxO utxoCollateral,
      -- Part 4: isAdaOnly balance
      validateCollateralContainsNonADA bal,
      -- Part 5: balance ≥ ⌈txfee txb ∗ (collateralPercent pp) / 100⌉
      validateInsufficientCollateral pp txb bal,
      -- Part 6: (txcoll tx ≠ ◇) ⇒ balance = txcoll tx
      unless (totalCollateral' == mempty) $ validateCollateralEqBalance bal totalCollateral',
      -- Part 7: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      failureIf (null utxoCollateral) NoCollateralInputs
    ]
  where
    totalCollateral' = getField @"totalCollateral" txb

-- > (txcoll tx ≠ ◇) => balance == txcoll tx
validateCollateralEqBalance :: Val.Val t => t -> t -> Validation (NonEmpty (UtxoPredicateFailure era)) ()
validateCollateralEqBalance bal txcoll =
  failureUnless (bal == txcoll) $
    TotalCollateralInequality
      (Val.coin bal)
      (Val.coin txcoll)
