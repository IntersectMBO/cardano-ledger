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
import Cardano.Ledger.Alonzo.Tx (isTwoPhaseScriptAddressFromMap)
import Cardano.Ledger.Babbage.Scripts (txscripts)
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    TxOut (..),
    collateralInputs',
    collateralReturn',
    outputs',
    txfee',
  )
import Cardano.Ledger.BaseTypes (txIxFromIntegral)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto), ValidateScript (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Cardano.Ledger.Val ((<->))
import Data.Compact.SplitMap ((◁))
import qualified Data.Compact.SplitMap as SplitMap
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Records (HasField (..))
import Numeric.Natural (Natural)

-- ============================================================

isTwoPhaseScriptAddress ::
  forall era.
  ( Core.TxOut era ~ TxOut era,
    ValidateScript era,
    Core.TxBody era ~ TxBody era
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

collBalance :: forall era. Era era => TxBody era -> UTxO era -> Core.Value era
collBalance txb (UTxO m) =
  case collateralReturn' txb of
    SNothing -> colbal
    SJust (TxOut _ retval _ _) -> colbal <-> retval
  where
    col = UTxO (collateralInputs' @era txb ◁ m)
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

feesOK :: Core.PParams era -> Core.Tx era -> UTxO era -> Bool
feesOK _pparams _tx _utxo = undefined
