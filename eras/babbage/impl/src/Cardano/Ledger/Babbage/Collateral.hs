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
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (txscripts))
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    TxOut (..),
    collateralReturn',
    outputs',
  )
import Cardano.Ledger.BaseTypes (TxIx (..), txIxFromIntegral)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto), ValidateScript (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Cardano.Ledger.Val ((<->))
import Control.SetAlgebra (eval, (◁))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import Data.Word (Word16, Word64)
import GHC.Records (HasField (..))

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
isTwoPhaseScriptAddress tx utxo = isTwoPhaseScriptAddressFromMap @era (txscripts utxo tx)

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
    col = UTxO (eval (getField @"collateral" txb ◁ m))
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
    SNothing -> UTxO Map.empty
    SJust txout -> UTxO (Map.singleton (TxIn (txid txb) index) txout)
      where
        index = case txIxFromIntegral (length (outputs' txb)) of
          Just i -> i
          -- In the impossible event that there are more transaction outputs
          -- in the transaction than will fit into a Word16 (which backs the TxIx),
          -- we give the collateral return output an index of maxBound.
          Nothing -> TxIx ((fromIntegral :: Word16 -> Word64) (maxBound :: Word16))
