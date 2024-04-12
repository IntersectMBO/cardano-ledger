{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Figure 2: Functions related to fees and collateral
--   Babbage Specification
module Cardano.Ledger.Babbage.Collateral (
  collAdaBalance,
  collOuts,
)
where

import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (..))
import Cardano.Ledger.BaseTypes (TxIx (..), txIxFromIntegral)
import Cardano.Ledger.Coin (DeltaCoin, toDeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..), coinBalance)
import Cardano.Ledger.Val ((<->))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word16, Word64)
import Lens.Micro

-- ============================================================

collAdaBalance ::
  forall era.
  BabbageEraTxBody era =>
  TxBody era ->
  Map.Map (TxIn (EraCrypto era)) (TxOut era) ->
  DeltaCoin
collAdaBalance txBody utxoCollateral = toDeltaCoin $
  case txBody ^. collateralReturnTxBodyL of
    SNothing -> colbal
    SJust txOut -> colbal <-> (txOut ^. coinTxOutL @era)
  where
    colbal = coinBalance $ UTxO utxoCollateral

collOuts ::
  BabbageEraTxBody era =>
  TxBody era ->
  UTxO era
collOuts txBody =
  case txBody ^. collateralReturnTxBodyL of
    SNothing -> UTxO Map.empty
    SJust txOut -> UTxO (Map.singleton (TxIn (txIdTxBody txBody) index) txOut)
      where
        index = case txIxFromIntegral (length (txBody ^. outputsTxBodyL)) of
          Just i -> i
          -- In the impossible event that there are more transaction outputs
          -- in the transaction than will fit into a Word16 (which backs the TxIx),
          -- we give the collateral return output an index of maxBound.
          Nothing -> TxIx ((fromIntegral :: Word16 -> Word64) (maxBound :: Word16))
