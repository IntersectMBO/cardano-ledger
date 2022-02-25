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
    spendInputs',
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (hashScript, isNativeScript))
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (SJust))
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

txscripts ::
  forall era.
  ( Era era,
    ValidateScript era,
    Core.TxBody era ~ TxBody era,
    Core.Witnesses era ~ TxWitness era,
    Core.TxOut era ~ TxOut era
  ) =>
  UTxO era ->
  Core.Tx era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era)
txscripts (UTxO m) tx = witnessScripts `Map.union` referenceScripts
  where
    txbody = getField @"body" tx
    allInputs = Set.union (spendInputs' txbody) (referenceInputs' txbody)
    witnessset = getField @"wits" tx
    witnessScripts = txscripts' witnessset
    referenceScripts = Set.foldl' accum Map.empty allInputs
    accum ans txin =
      case getScript txin of
        Nothing -> ans
        Just script -> Map.insert (hashScript @era script) script ans
    getScript txin = SplitMap.lookup txin m >>= extractScript
    extractScript (TxOut _ _ _ (SJust s)) = Just s
    extractScript _ = Nothing

languages ::
  forall era.
  ( ValidateScript era,
    Core.TxBody era ~ TxBody era,
    Core.Witnesses era ~ TxWitness era,
    Core.Script era ~ Script era, -- THE ATERNATIVE TO THIS IS TO ADD 'language' to the ValidateScript class
    Core.TxOut era ~ TxOut era
  ) =>
  UTxO era ->
  Core.Tx era ->
  Set Language
languages utxo tx =
  Set.fromList
    [ lang
      | (_, script) <- Map.toList (txscripts utxo tx),
        (not . isNativeScript @era) script,
        Just lang <- [language @era script]
    ]
