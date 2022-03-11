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

-- import Debug.Trace(trace)
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

-- | Compute a Map of (ScriptHash -> Script) for all Scripts
--   found in a ValidatedTx. Note we are interested in the actual scripts.
--   There are two places to look
--   1) the Script part of the Witnesses
--   2) the reference scripts found in the TxOuts, pointed to by the inputs
--   The flip side is 'ScriptsNeeded' computes the ScriptHash of every Credential.
--   We maintain the invariant that every Script Credential points to some actual script.
babbageTxScripts ::
  forall era.
  ( Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    ValidateScript era
  ) =>
  UTxO era ->
  Core.Tx era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era)
babbageTxScripts utxo tx = ans
  where
    txbody = getField @"body" tx
    ins = referenceInputs' txbody `Set.union` spendInputs' txbody
    ans = Map.union (refScripts ins utxo) (getField @"scriptWits" tx)

plistf :: Monoid x => (t -> x) -> x -> [t] -> x -> x -> x
plistf f open xs sep close = open <> loop xs <> close
  where
    loop [] = mempty
    loop [y] = f y
    loop (y : ys) = f y <> sep <> loop ys

-- | Collect all the reference scripts found in the TxOuts, pointed to by some input.
refScripts ::
  forall era.
  ( Core.TxOut era ~ TxOut era,
    ValidateScript era
  ) =>
  Set (TxIn (Crypto era)) ->
  UTxO era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era)
refScripts ins (UTxO mp) = SplitMap.foldl' accum Map.empty (ins SplitMap.◁ mp)
  where
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
    accum ans@(hashSet, inputSet) txin txout =
      case txout of
        (TxOut addr _ NoDatum _) ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (hashSet, Set.insert txin inputSet)
            else ans
        (TxOut addr _ (DatumHash dhash) _) ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (Set.insert dhash hashSet, inputSet)
            else ans
        (TxOut addr _ (Datum _) _) ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then ans -- An a TwoPhaseScript with Explict Datum and does not need a DataHash
            else (hashSet, Set.insert txin inputSet)

-- FIXME -- An onePhase script with an unneeded Explict Datum, is that an error?
