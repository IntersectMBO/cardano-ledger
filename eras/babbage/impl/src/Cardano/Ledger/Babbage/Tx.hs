{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Tx (
  AlonzoTx (..),
  BabbageTxBody (..),
  module X,
  getDatumBabbage,
  babbageTxScripts,
  refScripts,
)
where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.PlutusScriptApi (getSpendingTxIn)
import Cardano.Ledger.Alonzo.Tx as X
import Cardano.Ledger.Alonzo.TxSeq (
  AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
  hashAlonzoTxSeq,
 )
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  unTxDats,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxBody (
  BabbageEraTxBody (..),
  BabbageEraTxOut (..),
  BabbageTxBody (..),
  dataHashTxOutL,
 )
import Cardano.Ledger.Babbage.TxWits ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Control.Applicative ((<|>))
import Control.SetAlgebra (eval, (◁))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing), strictMaybeToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro

instance CC.Crypto c => EraTx (BabbageEra c) where
  {-# SPECIALIZE instance EraTx (BabbageEra CC.StandardCrypto) #-}

  type Tx (BabbageEra c) = AlonzoTx (BabbageEra c)

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script script) tx = validateTimelock @(BabbageEra c) script tx
  {-# INLINE validateScript #-}

  getMinFeeTx = alonzoMinFeeTx

instance CC.Crypto c => AlonzoEraTx (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTx (BabbageEra CC.StandardCrypto) #-}

  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance CC.Crypto c => EraSegWits (BabbageEra c) where
  type TxSeq (BabbageEra c) = AlonzoTxSeq (BabbageEra c)
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4

-- | Extract binary data either directly from the `Tx` as an "inline datum"
-- or look it up in the witnesses by the hash.
getDatumBabbage ::
  ( AlonzoEraTx era
  , BabbageEraTxOut era
  ) =>
  Tx era ->
  UTxO era ->
  ScriptPurpose era ->
  Maybe (Data era)
getDatumBabbage tx (UTxO m) sp = do
  txIn <- getSpendingTxIn sp
  txOut <- Map.lookup txIn m
  let txOutDataFromWits = do
        hash <- strictMaybeToMaybe (txOut ^. dataHashTxOutL)
        Map.lookup hash (unTxDats (tx ^. witsTxL . datsTxWitsL))
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

babbageTxScripts ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  ) =>
  UTxO era ->
  Tx era ->
  Map.Map (ScriptHash (EraCrypto era)) (Script era)
babbageTxScripts utxo tx = ans
  where
    txBody = tx ^. bodyTxL
    ins = (txBody ^. referenceInputsTxBodyL) `Set.union` (txBody ^. inputsTxBodyL)
    ans = refScripts ins utxo `Map.union` (tx ^. witsTxL . scriptTxWitsL)

-- | Collect all the reference scripts found in the TxOuts, pointed to by some input.
refScripts ::
  forall era.
  (BabbageEraTxOut era) =>
  Set (TxIn (EraCrypto era)) ->
  UTxO era ->
  Map.Map (ScriptHash (EraCrypto era)) (Script era)
refScripts ins (UTxO mp) = Map.foldl' accum Map.empty (eval (ins ◁ mp))
  where
    accum ans txOut =
      case txOut ^. referenceScriptTxOutL of
        SNothing -> ans
        SJust script -> Map.insert (hashScript @era script) script ans
