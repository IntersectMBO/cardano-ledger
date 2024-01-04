{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.UTxO (
  conwayProducedValue,
  getConwayWitsVKeyNeeded,
) where

import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsHashesNeeded,
  getAlonzoScriptsNeeded,
 )
import Cardano.Ledger.Babbage.UTxO (
  getBabbageScriptsProvided,
  getBabbageSpendingDatum,
  getBabbageSupplementalDataHashes,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance.Procedures (
  Voter (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Credential (credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeededNoGov, shelleyProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO)
import Cardano.Ledger.Val (Val (..), inject)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Lens.Micro ((^.))

getConwayScriptsNeeded ::
  ConwayEraTxBody era =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getConwayScriptsNeeded utxo txb = getAlonzoScriptsNeeded utxo txb <> voterScripts
  where
    getMaybeScriptHash voter =
      (,) (error "TODO: Implement VoterScriptPurpose")
        <$> case voter of
          CommitteeVoter cred -> credScriptHash cred
          DRepVoter cred -> credScriptHash cred
          StakePoolVoter _ -> Nothing

    voterScripts =
      AlonzoScriptsNeeded . mapMaybe getMaybeScriptHash . Map.keys . unVotingProcedures $
        txb ^. votingProceduresTxBodyL

conwayProducedValue ::
  ConwayEraTxBody era =>
  PParams era ->
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  TxBody era ->
  Value era
conwayProducedValue pp isStakePool txBody =
  shelleyProducedValue pp isStakePool txBody
    <+> inject (txBody ^. treasuryDonationTxBodyL)

instance Crypto c => EraUTxO (ConwayEra c) where
  type ScriptsNeeded (ConwayEra c) = AlonzoScriptsNeeded (ConwayEra c)

  getConsumedValue = getConsumedMaryValue

  getProducedValue = conwayProducedValue

  getScriptsProvided = getBabbageScriptsProvided

  getScriptsNeeded = getConwayScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded -- TODO: This also changes for Conway

  getWitsVKeyNeeded _ = getConwayWitsVKeyNeeded

instance Crypto c => AlonzoEraUTxO (ConwayEra c) where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum

getConwayWitsVKeyNeeded ::
  (EraTx era, ConwayEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
getConwayWitsVKeyNeeded utxo txBody =
  getShelleyWitsVKeyNeededNoGov utxo txBody
    `Set.union` (txBody ^. reqSignerHashesTxBodyL)
    `Set.union` voterWitnesses txBody

voterWitnesses ::
  ConwayEraTxBody era =>
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
voterWitnesses txb =
  Map.foldrWithKey' accum mempty (unVotingProcedures (txb ^. votingProceduresTxBodyL))
  where
    accum voter _ khs =
      maybe khs (`Set.insert` khs) $
        case voter of
          CommitteeVoter cred -> credKeyHashWitness cred
          DRepVoter cred -> credKeyHashWitness cred
          StakePoolVoter poolId -> Just $ asWitness poolId
