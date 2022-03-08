{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Figure 3: Functions related to scripts
--   Babbage Specification
module Cardano.Ledger.Babbage.Scripts where

import Cardano.Ledger.Alonzo.Data (BinaryData, dataToBinaryData)
import Cardano.Ledger.Alonzo.Tx
  ( ScriptPurpose (..),
    ValidatedTx (..),
    isTwoPhaseScriptAddressFromMap,
    txdats',
  )
import Cardano.Ledger.Alonzo.TxWitness (TxWitness, unTxDats)
import Cardano.Ledger.Babbage.TxBody
  ( Datum (..),
    TxBody (..),
    TxOut (..),
    referenceInputs',
    spendInputs',
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (hashScript))
import Cardano.Ledger.Hashes (DataHash)
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))

-- ====================================================================

getTxIn :: ScriptPurpose crypto -> Maybe (TxIn crypto)
getTxIn (Spending txin) = Just txin
-- Only the Spending ScriptPurpose contains TxIn
getTxIn (Minting _policyid) = Nothing
getTxIn (Rewarding _rewaccnt) = Nothing
getTxIn (Certifying _dcert) = Nothing

-- | Extract binary data either directly from the `Core.Tx` as an "inline datum"
-- or it up in the witnesses by the hash.
getDatum ::
  ( Era era,
    Core.TxOut era ~ TxOut era,
    Core.Witnesses era ~ TxWitness era
  ) =>
  Core.Tx era ->
  UTxO era ->
  ScriptPurpose (Crypto era) ->
  Maybe (BinaryData era)
getDatum tx (UTxO m) sp = do
  txin <- getTxIn sp
  TxOut _ _ datum _refScript <- SplitMap.lookup txin m
  case datum of
    NoDatum -> Nothing
    Datum d -> Just d
    DatumHash hash ->
      dataToBinaryData <$> Map.lookup hash (unTxDats $ txdats' (getField @"wits" tx))

{- txscripts tx utxo = txwitscripts tx ∪ {hash s ↦ s | ( , , , s) ∈ utxo (spendInputs tx ∪ refInputs tx)} -}

-- | Compute a Map of all the Scripts in a ValidatedTx.
babbageTxScripts ::
  forall era.
  ( Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    ValidateScript era
  ) =>
  UTxO era ->
  Core.Tx era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era)
babbageTxScripts (UTxO mp) tx = SplitMap.foldl' accum (getField @"scriptWits" tx) smallUtxo
  where
    txbody = getField @"body" tx
    scriptinputs = referenceInputs' txbody `Set.union` spendInputs' txbody
    smallUtxo = scriptinputs SplitMap.◁ mp
    accum ans txout =
      case txout of
        (TxOut _ _ _ SNothing) -> ans
        (TxOut _ _ _ (SJust script)) -> Map.insert (hashScript @era script) script ans

-- Compute two sets for all TwoPhase scripts in a Tx.
-- set 1) DataHashes for each Two phase Script in a TxIn that has a DataHash
-- set 2) TxIns that are TwoPhase scripts, and should have a DataHash but don't.
--        in Babbage, a TxOut with an inline Datum, does not need DataHash, so
--        it should not be added to set of Bad TxIn.
{- { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isNonNativeScriptAddress tx a} -}
babbageInputDataHashes ::
  forall era.
  ( HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    ValidateScript era,
    Core.TxOut era ~ TxOut era
  ) =>
  Map.Map (ScriptHash (Crypto era)) (Core.Script era) ->
  ValidatedTx era ->
  UTxO era ->
  (Set (DataHash (Crypto era)), Set (TxIn (Crypto era)))
babbageInputDataHashes hashScriptMap tx (UTxO mp) =
  SplitMap.foldlWithKey' accum (Set.empty, Set.empty) smallUtxo
  where
    txbody = body tx
    spendinputs = getField @"inputs" txbody :: (Set (TxIn (Crypto era)))
    smallUtxo = spendinputs SplitMap.◁ mp
    accum ans@(!hashSet, !inputSet) txin txout =
      case txout of
        TxOut addr _ NoDatum _ ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (hashSet, Set.insert txin inputSet)
            else ans
        TxOut addr _ (DatumHash dhash) _ ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (Set.insert dhash hashSet, inputSet)
            else ans
        -- Though it is somewhat odd to allow non-two-phase-scripts to include a datum,
        -- the Alonzo era already set the precedent with datum hashes, and several dapp
        -- developers see this as a helpful feature.
        TxOut _ _ (Datum _) _ -> ans
