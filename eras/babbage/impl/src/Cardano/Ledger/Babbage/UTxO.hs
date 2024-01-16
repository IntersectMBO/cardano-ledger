{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.UTxO (
  getBabbageSupplementalDataHashes,
  getBabbageSpendingDatum,
  getBabbageScriptsProvided,
  getReferenceScripts,
) where

import Cardano.Ledger.Alonzo.TxWits (unTxDats)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded,
  getAlonzoScriptsHashesNeeded,
  getAlonzoScriptsNeeded,
  getAlonzoWitsVKeyNeeded,
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Shelley.UTxO (shelleyProducedValue)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Control.Applicative
import Control.SetAlgebra (eval, (◁))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro

instance Crypto c => EraUTxO (BabbageEra c) where
  type ScriptsNeeded (BabbageEra c) = AlonzoScriptsNeeded (BabbageEra c)

  getConsumedValue = getConsumedMaryValue

  getProducedValue = shelleyProducedValue

  getScriptsProvided = getBabbageScriptsProvided

  getScriptsNeeded = getAlonzoScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded = getAlonzoWitsVKeyNeeded

instance Crypto c => AlonzoEraUTxO (BabbageEra c) where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum

getBabbageSupplementalDataHashes ::
  BabbageEraTxBody era =>
  UTxO era ->
  TxBody era ->
  Set.Set (DataHash (EraCrypto era))
getBabbageSupplementalDataHashes (UTxO utxo) txBody =
  Set.fromList [dh | txOut <- outs, SJust dh <- [txOut ^. dataHashTxOutL]]
  where
    newOuts = map sizedValue $ toList $ txBody ^. allSizedOutputsTxBodyF
    referencedOuts = Map.elems $ Map.restrictKeys utxo (txBody ^. referenceInputsTxBodyL)
    outs = newOuts <> referencedOuts

-- | Extract binary data either directly from the `Tx` as an "inline datum"
-- or look it up in the witnesses by the hash.
getBabbageSpendingDatum ::
  ( AlonzoEraTx era
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  Tx era ->
  PlutusPurpose AsItem era ->
  Maybe (Data era)
getBabbageSpendingDatum (UTxO utxo) tx sp = do
  txIn <- plutusPurposeSpendingTxIn sp
  txOut <- Map.lookup txIn utxo
  let txOutDataFromWits = do
        dataHash <- strictMaybeToMaybe (txOut ^. dataHashTxOutL)
        Map.lookup dataHash (unTxDats (tx ^. witsTxL . datsTxWitsL))
  strictMaybeToMaybe (txOut ^. dataTxOutL) <|> txOutDataFromWits

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
-- Compute a Map of (ScriptHash -> Script) for all Scripts found in a AlonzoTx.
-- Note we are interested in the actual scripts that might be run during the Utxow
-- rule. There are two places to look:
-- 1) The Script part of the TxWits
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

getBabbageScriptsProvided ::
  ( EraTx era
  , BabbageEraTxBody era
  ) =>
  UTxO era ->
  Tx era ->
  ScriptsProvided era
getBabbageScriptsProvided utxo tx = ScriptsProvided ans
  where
    txBody = tx ^. bodyTxL
    ins = (txBody ^. referenceInputsTxBodyL) `Set.union` (txBody ^. inputsTxBodyL)
    ans = getReferenceScripts utxo ins `Map.union` (tx ^. witsTxL . scriptTxWitsL)

-- | Collect all the reference scripts found in the TxOuts, pointed to by some input.
getReferenceScripts ::
  BabbageEraTxOut era =>
  UTxO era ->
  Set (TxIn (EraCrypto era)) ->
  Map.Map (ScriptHash (EraCrypto era)) (Script era)
getReferenceScripts (UTxO mp) inputs = Map.foldl' accum Map.empty (eval (inputs ◁ mp))
  where
    accum ans txOut =
      case txOut ^. referenceScriptTxOutL of
        SNothing -> ans
        SJust script -> Map.insert (hashScript script) script ans
