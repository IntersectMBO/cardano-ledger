{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.UTxO
  ( AlonzoScriptsNeeded (..),
    getAlonzoScriptsNeeded,
    getAlonzoScriptsHashesNeeded,
    getInputDataHashesTxBody,
  )
where

import Cardano.Ledger.Alonzo.Data (Datum (..))
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..), isTwoPhaseScriptAddressFromMap)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxOut (..), MaryEraTxBody (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (ScriptHashObj))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..), DelegCert (..), Delegation (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (..), getRwdCred)
import Cardano.Ledger.Shelley.UTxO (scriptCred)
import Cardano.Ledger.TxIn
import Cardano.Ledger.UTxO
  ( EraUTxO (..),
    UTxO (..),
    getScriptHash,
  )
import Control.SetAlgebra (eval, (◁))
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Extras (view)

-- | Alonzo era style scritps needed require also a `ScriptPurpose`, not only the `ScriptHash`
newtype AlonzoScriptsNeeded era
  = AlonzoScriptsNeeded [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
  deriving (Eq, Show)

instance Crypto c => EraUTxO (AlonzoEra c) where
  type ScriptsNeeded (AlonzoEra c) = AlonzoScriptsNeeded (AlonzoEra c)

  getConsumedValue = getConsumedMaryValue

  getScriptsNeeded = getAlonzoScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

getAlonzoScriptsHashesNeeded :: AlonzoScriptsNeeded era -> Set.Set (ScriptHash (EraCrypto era))
getAlonzoScriptsHashesNeeded (AlonzoScriptsNeeded sn) = Set.fromList (map snd sn)

-- | Compute two sets for all TwoPhase scripts in a Tx.
--
--   1) DataHashes for each Two phase Script in a TxIn that has a DataHash
--   2) TxIns that are TwoPhase scripts, and should have a DataHash but don't.
--
-- @{ h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isNonNativeScriptAddress tx a}@
getInputDataHashesTxBody ::
  (EraTxBody era, AlonzoEraTxOut era, EraScript era) =>
  UTxO era ->
  TxBody era ->
  Map.Map (ScriptHash (EraCrypto era)) (Script era) ->
  (Set.Set (DataHash (EraCrypto era)), Set.Set (TxIn (EraCrypto era)))
getInputDataHashesTxBody (UTxO mp) txBody hashScriptMap =
  Map.foldlWithKey' accum (Set.empty, Set.empty) smallUtxo
  where
    spendInputs = txBody ^. inputsTxBodyL
    smallUtxo = eval (spendInputs ◁ mp)
    accum ans@(!hashSet, !inputSet) txIn txOut =
      let addr = txOut ^. addrTxOutL
       in case txOut ^. datumTxOutF of
            NoDatum
              | isTwoPhaseScriptAddressFromMap hashScriptMap addr ->
                  (hashSet, Set.insert txIn inputSet)
            DatumHash dataHash
              | isTwoPhaseScriptAddressFromMap hashScriptMap addr ->
                  (Set.insert dataHash hashSet, inputSet)
            -- Though it is somewhat odd to allow non-two-phase-scripts to include a datum,
            -- the Alonzo era already set the precedent with datum hashes, and several dapp
            -- developers see this as a helpful feature.
            _ -> ans

-- |
-- Uses of inputs in ‘txscripts’ and ‘neededScripts’
-- There are currently 3 sets of inputs (spending, collateral, reference). A particular TxInput
-- can appear in more than one of the sets. Even in all three at the same, but that may not be
-- a really useful case. Inputs are where you find scripts with the 'Spending' purpose.
--
-- 1) Collateral inputs are only spent if phase two fails. Their corresponding TxOut can only have
--    Key (not Script) Pay credentials, so ‘neededScripts’ does not look there.
-- 2) Reference inputs are not spent in the current Tx, unless that same input also appears in one
--    of the other sets. If that is not the case, their credentials are never needed, so anyone can
--    access the inline datums and scripts in their corresponding TxOut, without needing any
--    authorizing credentials. So ‘neededScripts’ does not look there.
-- 3) Spending inputs are always spent. So their Pay credentials are always needed.
--
-- Collect information (purpose and ScriptHash) about all the Credentials that refer to scripts
-- that will be needed to run in a TxBody in the Utxow rule. Note there may be credentials that
-- cannot be run, so are not collected. In Babbage, reference inputs, fit that description.
-- Purposes include
-- 1) Spending (payment script credentials, but NOT staking scripts) in the Addr of a TxOut, pointed
--    to by some input that needs authorization. Be sure (getField @"inputs" txb) gets all such inputs.
--    In some Eras there may be multiple sets of inputs, which ones should be included? Currently that
--    is only the spending inputs. Because collateral inputs can only have key-locked credentials,
--    and reference inputs are never authorized. That might not always be the case.
-- 2) Rewarding (Withdrawals),
-- 3) Minting (minted field), and
-- 4) Certifying (Delegating) scripts.
--
-- 'getAlonzoScriptsNeeded' is an aggregation of the needed Credentials referring to
-- Scripts used in Utxow rule.  The flip side of 'getAlonzoScriptsNeeded' (which collects
-- script hashes) is 'txscripts' which finds the actual scripts. We maintain an invariant
-- that every script credential refers to some actual script.  This is tested in the test
-- function 'validateMissingScripts' in the Utxow rule.
getAlonzoScriptsNeeded ::
  forall era.
  MaryEraTxBody era =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getAlonzoScriptsNeeded (UTxO u) txBody =
  AlonzoScriptsNeeded (spend ++ reward ++ cert ++ minted)
  where
    collect ::
      TxIn (EraCrypto era) ->
      Maybe (ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))
    collect !i = do
      addr <- view addrTxOutL <$> Map.lookup i u
      hash <- getScriptHash addr
      return (Spending i, hash)

    !spend = mapMaybe collect (Set.toList $ txBody ^. inputsTxBodyL)

    !reward = mapMaybe fromRwd (Map.keys withdrawals)
      where
        withdrawals = unWdrl $ txBody ^. wdrlsTxBodyL
        fromRwd accnt = do
          hash <- scriptCred $ getRwdCred accnt
          return (Rewarding accnt, hash)

    !cert = foldl' addOnlyCwitness [] (txBody ^. certsTxBodyL)

    !minted = map (\hash -> (Minting (PolicyID hash), hash)) $ Set.toList $ txBody ^. mintedTxBodyF

-- | We only find certificate witnesses in Delegating and Deregistration DCerts that have
-- ScriptHashObj credentials.
addOnlyCwitness ::
  [(ScriptPurpose c, ScriptHash c)] ->
  DCert c ->
  [(ScriptPurpose c, ScriptHash c)]
addOnlyCwitness !ans (DCertDeleg c@(DeRegKey (ScriptHashObj hk))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans (DCertDeleg c@(Delegate (Delegation (ScriptHashObj hk) _dpool))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans _ = ans
