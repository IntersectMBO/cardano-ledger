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
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (language)
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..), txdats')
import Cardano.Ledger.Alonzo.TxWitness (TxWitness, txscripts', unTxDats)
import Cardano.Ledger.Babbage.TxBody
  ( Datum (..),
    TxBody (..),
    TxOut (..),
    referenceInputs',
    spendingInputs',
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (isNativeScript))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))

-- ====================================================================

hasTxIn :: ScriptPurpose crypto -> Maybe (TxIn crypto)
hasTxIn (Spending txin) = Just txin
-- Only the Spending ScriptPurpose contains TxIn
hasTxIn (Minting _policyid) = Nothing
hasTxIn (Rewarding _rewaccnt) = Nothing
hasTxIn (Certifying _dcert) = Nothing

getDatum ::
  ( Era era,
    Core.TxOut era ~ TxOut era,
    Core.Witnesses era ~ TxWitness era
  ) =>
  Core.Tx era ->
  UTxO era ->
  ScriptPurpose (Crypto era) ->
  [BinaryData era]
getDatum tx (UTxO m) sp = case hasTxIn sp of
  Nothing -> []
  Just txin ->
    case SplitMap.lookup txin m of
      Nothing -> []
      Just (TxOut _ _ datum) ->
        case datum of
          NoDatum -> []
          Datum d -> [d]
          DatumHash hash ->
            case Map.lookup hash (unTxDats $ txdats' (getField @"wits" tx)) of
              Nothing -> []
              Just dat -> [dataToBinaryData dat]

extractScript :: Core.TxOut era -> Maybe (Core.Script era)
extractScript _txout = undefined

txscripts ::
  ( Era era,
    Core.TxBody era ~ TxBody era,
    Core.Witnesses era ~ TxWitness era,
    Ord (Core.Script era)
  ) =>
  Core.Tx era ->
  UTxO era ->
  Set (Core.Script era)
txscripts tx (UTxO m) = witnessScripts `Set.union` referenceScripts
  where
    txbody = getField @"body" tx
    allInputs = Set.union (spendingInputs' txbody) (referenceInputs' txbody)
    witnessset = getField @"wits" tx
    witnessScripts = Set.fromList (Map.elems (txscripts' witnessset))
    referenceScripts = Set.foldl' accum Set.empty allInputs
    getScript txin = do txout <- SplitMap.lookup txin m; extractScript txout
    accum ans txin = maybe ans (`Set.insert` ans) (getScript txin)

languages ::
  forall era.
  ( ValidateScript era,
    Core.TxBody era ~ TxBody era,
    Core.Witnesses era ~ TxWitness era,
    Core.Script era ~ Script era -- THE ATERNATIVE TO THIS IS TO ADD 'language' to the ValidateScript class
  ) =>
  Core.Tx era ->
  UTxO era ->
  Set Language
languages tx utxo =
  Set.fromList
    [ lang
      | script <- Set.toList (txscripts tx utxo),
        (not . isNativeScript @era) script,
        Just lang <- [language @era script]
    ]
