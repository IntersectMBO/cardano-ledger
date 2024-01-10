{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  getAlonzoSpendingDatum,
  getAlonzoSpendingTxIn,
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsNeeded,
  getAlonzoScriptsHashesNeeded,
  getInputDataHashesTxBody,
  getAlonzoWitsVKeyNeeded,
)
where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..), isTwoPhaseScriptAddressFromMap)
import Cardano.Ledger.Alonzo.TxWits (unTxDats)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Credential (credScriptHash)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.Plutus.Data (Data, Datum (..))
import Cardano.Ledger.Shelley.TxBody (getRwdCred)
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeeded, shelleyProducedValue)
import Cardano.Ledger.TxIn
import Cardano.Ledger.UTxO (
  EraUTxO (..),
  ScriptsProvided (..),
  UTxO (..),
  getScriptHash,
 )
import Control.SetAlgebra (eval, (◁))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Extras (view)

-- | Alonzo era style scripts needed require also a `ScriptPurpose`, not only the `ScriptHash`
newtype AlonzoScriptsNeeded era
  = AlonzoScriptsNeeded [(ScriptPurpose era, ScriptHash (EraCrypto era))]
  deriving (Semigroup, Monoid)

deriving instance (Era era, Eq (TxCert era)) => Eq (AlonzoScriptsNeeded era)
deriving instance (Era era, Show (TxCert era)) => Show (AlonzoScriptsNeeded era)

instance Crypto c => EraUTxO (AlonzoEra c) where
  type ScriptsNeeded (AlonzoEra c) = AlonzoScriptsNeeded (AlonzoEra c)

  getConsumedValue = getConsumedMaryValue

  getProducedValue = shelleyProducedValue

  getScriptsProvided _ tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsNeeded = getAlonzoScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded = getAlonzoWitsVKeyNeeded

class EraUTxO era => AlonzoEraUTxO era where
  -- | Get data hashes for a transaction that are not required. Such datums are optional,
  -- but they can be added to the witness set. In a broaded terms datums corresponding to
  -- the inputs that might be spent are the required datums and the datums corresponding
  -- to the outputs and reference inputs are the supplemental datums.
  getSupplementalDataHashes ::
    UTxO era ->
    TxBody era ->
    Set.Set (DataHash (EraCrypto era))

  -- | Lookup the TxIn from the `Spending` ScriptPurpose and find the datum needed for
  -- spending that input. This function will return `Nothing` for all script purposes,
  -- except spending, because only spending scripts require an extra datum.
  --
  -- This is the same function as in the spec:
  --
  -- @
  --   getDatum :: Tx era -> UTxO era -> ScriptPurpose era -> [Data era]
  -- @
  getSpendingDatum ::
    UTxO era ->
    Tx era ->
    ScriptPurpose era ->
    Maybe (Data era)

instance Crypto c => AlonzoEraUTxO (AlonzoEra c) where
  getSupplementalDataHashes _ = getAlonzoSupplementalDataHashes

  getSpendingDatum = getAlonzoSpendingDatum

getAlonzoSupplementalDataHashes ::
  (EraTxBody era, AlonzoEraTxOut era) =>
  TxBody era ->
  Set.Set (DataHash (EraCrypto era))
getAlonzoSupplementalDataHashes txBody =
  Set.fromList
    [ dh
    | txOut <- toList $ txBody ^. outputsTxBodyL
    , SJust dh <- [txOut ^. dataHashTxOutL]
    ]

-- | Get the Data associated with a ScriptPurpose. Only the Spending ScriptPurpose
--  contains Data. Nothing is returned for the other kinds.
getAlonzoSpendingDatum ::
  (AlonzoEraTxWits era, AlonzoEraTxOut era, EraTx era) =>
  UTxO era ->
  Tx era ->
  ScriptPurpose era ->
  Maybe (Data era)
getAlonzoSpendingDatum (UTxO m) tx sp = do
  txIn <- getAlonzoSpendingTxIn sp
  txOut <- Map.lookup txIn m
  SJust hash <- Just $ txOut ^. dataHashTxOutL
  Map.lookup hash (unTxDats $ tx ^. witsTxL . datsTxWitsL)

-- | Only the Spending ScriptPurpose contains TxIn
getAlonzoSpendingTxIn :: ScriptPurpose era -> Maybe (TxIn (EraCrypto era))
getAlonzoSpendingTxIn = \case
  Spending txin -> Just txin
  Minting _policyId -> Nothing
  Rewarding _rewardAccount -> Nothing
  Certifying _txCert -> Nothing

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
  ScriptsProvided era ->
  (Set.Set (DataHash (EraCrypto era)), Set.Set (TxIn (EraCrypto era)))
getInputDataHashesTxBody (UTxO mp) txBody (ScriptsProvided scriptsProvided) =
  Map.foldlWithKey' accum (Set.empty, Set.empty) spendUTxO
  where
    spendInputs = txBody ^. inputsTxBodyL
    spendUTxO = eval (spendInputs ◁ mp)
    accum ans@(!hashSet, !inputSet) txIn txOut =
      let addr = txOut ^. addrTxOutL
          isTwoPhaseScriptAddress = isTwoPhaseScriptAddressFromMap scriptsProvided addr
       in case txOut ^. datumTxOutF of
            NoDatum
              | isTwoPhaseScriptAddress -> (hashSet, Set.insert txIn inputSet)
            DatumHash dataHash
              | isTwoPhaseScriptAddress -> (Set.insert dataHash hashSet, inputSet)
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
--    to by some input that needs authorization. Be sure (txBody ^. inputsTxBodyL) gets all such inputs.
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
      Maybe (ScriptPurpose era, ScriptHash (EraCrypto era))
    collect !i = do
      addr <- view addrTxOutL <$> Map.lookup i u
      hash <- getScriptHash addr
      return (Spending i, hash)

    !spend = mapMaybe collect (Set.toList $ txBody ^. inputsTxBodyL)

    !reward = mapMaybe fromRwd (Map.keys withdrawals)
      where
        withdrawals = unWithdrawals $ txBody ^. withdrawalsTxBodyL
        fromRwd accnt = do
          hash <- credScriptHash $ getRwdCred accnt
          return (Rewarding accnt, hash)

    !cert =
      mapMaybe (\cred -> (Certifying cred,) <$> getScriptWitnessTxCert cred) $
        toList (txBody ^. certsTxBodyL)

    !minted = map (\pId@(PolicyID hash) -> (Minting pId, hash)) $ Set.toList $ txBody ^. mintedTxBodyF

-- | Just like `getShelleyWitsVKeyNeeded`, but also requires `reqSignerHashesTxBodyL`.
getAlonzoWitsVKeyNeeded ::
  forall era.
  (EraTx era, AlonzoEraTxBody era, ShelleyEraTxBody era) =>
  CertState era ->
  UTxO era ->
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
getAlonzoWitsVKeyNeeded certState utxo txBody =
  getShelleyWitsVKeyNeeded certState utxo txBody
    `Set.union` (txBody ^. reqSignerHashesTxBodyL)
