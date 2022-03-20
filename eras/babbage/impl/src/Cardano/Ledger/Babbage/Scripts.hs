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
    TxOut (..),
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

-- Figure 3 of the Specification
{- txscripts tx utxo = txwitscripts tx ∪ {hash s ↦ s | ( , , , s) ∈ utxo (spendInputs tx ∪ refInputs tx)} -}

-- Uses of inputs in ‘txscripts’ and ‘neededScripts’
-- There are currently 3 sets of inputs (spending, collateral, reference). A particular TxInput
-- can appear in more than one of the sets. Even in all three at the same, but that may not be
-- a really useful case.
--
-- 1) Collateral inputs are only spent if phase two fails. Their corresponding TxOut can only have
--    Key (not Script) Pay credentials, so ‘neededScripts’ does not look there.
-- 2) Reference inputs are not spent in the current Tx, unless that same input also appears in one
--    of the other sets. If that is not the case, their credentials are never needed, so anyone can
--    access the inline datums and scripts in their corresponding TxOut, without needing any
--    authorizing credentials. So ‘neededScripts’ does not look there.
-- 3) Spending inputs are always spent. So their Pay credentials are always needed.
--
-- Compute a Map of (ScriptHash -> Script) for all Scripts found in a ValidatedTx.
-- Note we are interested in the actual scripts that might be run during the Utxow
-- rule. There are two places to look:
-- 1) The Script part of the Witnesses
-- 2) The reference scripts found in the TxOuts, pointed to by the spending and reference inputs
--    of the Tx.  Given such a TxOut, we look in the Pay credentials of the Addr of that TxOut.
--      A. We only look in the Pay credential of the TxOut, because the Stake credential plays
--         no role in the Utxow rule.
--      B. We don’t use the collateral inputs, because they only have key-locked Pay credentials
-- 3) Note that 'txscripts' includes both Plutus and Non-Plutus scripts
--
-- The flip side is 'ScriptsNeeded' which computes the ScriptHash of every Pay Credential
-- in spending and collateral inputs. Since reference inputs do not need to be authorized,
-- 'scriptsNeeded' does not look there.
-- It is an invariant that every such Credential points to some actual script found here.

babbageTxScripts ::
  forall era.
  (ValidateScript era, HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era)), HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))), HasField "referenceInputs" (Core.TxBody era) (Set (TxIn (Crypto era)))) =>
  UTxO era ->
  Core.Tx era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era)
babbageTxScripts utxo tx = ans
  where
    txbody = getField @"body" tx
    ins = getField @"referenceInputs" txbody `Set.union` getField @"inputs" txbody
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
  (ValidateScript era, HasField "referenceScript" (Core.TxOut era) (StrictMaybe (Core.Script era))) =>
  Set (TxIn (Crypto era)) ->
  UTxO era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era)
refScripts ins (UTxO mp) = SplitMap.foldl' accum Map.empty (ins SplitMap.◁ mp)
  where
    accum ans txout =
      case getField @"referenceScript" txout of
        SNothing -> ans
        (SJust script) -> Map.insert (hashScript @era script) script ans

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
