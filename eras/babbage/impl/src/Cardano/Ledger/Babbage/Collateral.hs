{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Figure 2: Functions related to fees and collateral
--   Babbage Specification
module Cardano.Ledger.Babbage.Collateral where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Tx (isTwoPhaseScriptAddressFromMap)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (txscripts))
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (..), BabbageTxBody, BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (TxIx (..), txIxFromIntegral)
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<->))
import Control.SetAlgebra (eval, (◁))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word16, Word64)
import Lens.Micro

-- ============================================================

isTwoPhaseScriptAddress ::
  forall era.
  (ExtendedUTxO era, EraScript era) =>
  Tx era ->
  UTxO era ->
  Addr (Crypto era) ->
  Bool
isTwoPhaseScriptAddress tx utxo = isTwoPhaseScriptAddressFromMap @era (txscripts utxo tx)

collBalance ::
  forall era.
  (BabbageEraTxBody era, TxOut era ~ BabbageTxOut era) =>
  TxBody era ->
  UTxO era ->
  Value era
collBalance txBody (UTxO m) =
  case txBody ^. collateralReturnTxBodyL of
    SNothing -> colbal
    SJust txOut -> colbal <-> (txOut ^. valueTxOutL @era)
  where
    col = UTxO (eval ((txBody ^. collateralInputsTxBodyL) ◁ m))
    colbal = balance @era col

collOuts ::
  ( BabbageEraTxBody era,
    TxBody era ~ BabbageTxBody era,
    TxOut era ~ BabbageTxOut era
  ) =>
  TxBody era ->
  UTxO era
collOuts txBody =
  case txBody ^. collateralReturnTxBodyL of
    SNothing -> UTxO Map.empty
    SJust txOut -> UTxO (Map.singleton (TxIn (txid txBody) index) txOut)
      where
        index = case txIxFromIntegral (length (txBody ^. outputsTxBodyL)) of
          Just i -> i
          -- In the impossible event that there are more transaction outputs
          -- in the transaction than will fit into a Word16 (which backs the TxIx),
          -- we give the collateral return output an index of maxBound.
          Nothing -> TxIx ((fromIntegral :: Word16 -> Word64) (maxBound :: Word16))
