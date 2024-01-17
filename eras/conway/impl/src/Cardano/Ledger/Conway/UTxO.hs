{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
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
  getMintingScriptsNeeded,
  getRewardingScriptsNeeded,
  getSpendingScriptsNeeded,
  zipAsIxItem,
 )
import Cardano.Ledger.Babbage.UTxO (
  getBabbageScriptsProvided,
  getBabbageSpendingDatum,
  getBabbageSupplementalDataHashes,
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance.Procedures (
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  unVotingProcedures,
 )
import Cardano.Ledger.Credential (credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeededNoGov, shelleyProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Ledger.Val (Val (..), inject)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Lens.Micro ((^.))

getConwayScriptsNeeded ::
  ConwayEraTxBody era =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getConwayScriptsNeeded utxo txBody =
  getSpendingScriptsNeeded utxo txBody
    <> getRewardingScriptsNeeded txBody
    <> certifyingScriptsNeeded
    <> getMintingScriptsNeeded txBody
    <> votingScriptsNeeded
    <> proposingScriptsNeeded
  where
    certifyingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (txBody ^. certsTxBodyL) $
            \asIxItem@(AsIxItem _ txCert) ->
              (CertifyingPurpose asIxItem,) <$> getScriptWitnessTxCert txCert

    votingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (Map.keys (unVotingProcedures (txBody ^. votingProceduresTxBodyL))) $
            \asIxItem@(AsIxItem _ voter) ->
              (VotingPurpose asIxItem,) <$> getVoterScriptHash voter
      where
        getVoterScriptHash = \case
          CommitteeVoter cred -> credScriptHash cred
          DRepVoter cred -> credScriptHash cred
          StakePoolVoter _ -> Nothing

    proposingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (txBody ^. proposalProceduresTxBodyL) $
            \asIxItem@(AsIxItem _ proposal) ->
              (ProposingPurpose asIxItem,) <$> getProposalScriptHash proposal
      where
        getProposalScriptHash ProposalProcedure {pProcGovAction} =
          case pProcGovAction of
            ParameterChange _ _ (SJust govPolicyHash) -> Just govPolicyHash
            TreasuryWithdrawals _ (SJust govPolicyHash) -> Just govPolicyHash
            _ -> Nothing

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
