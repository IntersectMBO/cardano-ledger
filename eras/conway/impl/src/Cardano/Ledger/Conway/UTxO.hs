{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.UTxO (
  conwayConsumed,
  conwayProducedValue,
  getConwayWitsVKeyNeeded,
  getConwayScriptsNeeded,
  txNonDistinctRefScriptsSize,
  getConwayMinFeeTxUtxo,
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
  getReferenceScriptsNonDistinct,
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance.Procedures (
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  unVotingProcedures,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue, getProducedMaryValue)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeededNoGov)
import Cardano.Ledger.Val (Val (..), inject)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
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

conwayConsumed ::
  (EraUTxO era, ConwayEraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody era ->
  Value era
conwayConsumed pp certState =
  getConsumedValue
    pp
    (lookupDepositDState $ certState ^. certDStateL)
    (lookupDepositVState $ certState ^. certVStateL)

conwayProducedValue ::
  (ConwayEraTxBody era, Value era ~ MaryValue) =>
  PParams era ->
  (KeyHash 'StakePool -> Bool) ->
  TxBody era ->
  Value era
conwayProducedValue pp isStakePool txBody =
  getProducedMaryValue pp isStakePool txBody
    <+> inject (txBody ^. treasuryDonationTxBodyL)

instance EraUTxO ConwayEra where
  type ScriptsNeeded ConwayEra = AlonzoScriptsNeeded ConwayEra

  consumed = conwayConsumed

  getConsumedValue = getConsumedMaryValue

  getProducedValue = conwayProducedValue

  getScriptsProvided = getBabbageScriptsProvided

  getScriptsNeeded = getConwayScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded -- TODO: This also changes for Conway

  getWitsVKeyNeeded _ = getConwayWitsVKeyNeeded

  getMinFeeTxUtxo = getConwayMinFeeTxUtxo

instance AlonzoEraUTxO ConwayEra where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum

getConwayMinFeeTxUtxo ::
  ( EraTx era
  , BabbageEraTxBody era
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Coin
getConwayMinFeeTxUtxo pparams tx utxo =
  getMinFeeTx pparams tx $ txNonDistinctRefScriptsSize utxo tx

-- | Calculate the total size of reference scripts used by the transactions. Duplicate
-- scripts will be counted as many times as they occur, since there is never a reason to
-- include an input with the same reference script.
--
-- Any input that appears in both regular inputs and reference inputs of a transaction is
-- only used once in this computation.
txNonDistinctRefScriptsSize :: (EraTx era, BabbageEraTxBody era) => UTxO era -> Tx era -> Int
txNonDistinctRefScriptsSize utxo tx = getSum $ foldMap (Sum . originalBytesSize . snd) refScripts
  where
    inputs = (tx ^. bodyTxL . referenceInputsTxBodyL) `Set.union` (tx ^. bodyTxL . inputsTxBodyL)
    refScripts = getReferenceScriptsNonDistinct utxo inputs

getConwayWitsVKeyNeeded ::
  (EraTx era, ConwayEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  Set.Set (KeyHash 'Witness)
getConwayWitsVKeyNeeded utxo txBody =
  getShelleyWitsVKeyNeededNoGov utxo txBody
    `Set.union` (txBody ^. reqSignerHashesTxBodyL)
    `Set.union` voterWitnesses txBody

voterWitnesses ::
  ConwayEraTxBody era =>
  TxBody era ->
  Set.Set (KeyHash 'Witness)
voterWitnesses txb =
  Map.foldrWithKey' accum mempty (unVotingProcedures (txb ^. votingProceduresTxBodyL))
  where
    accum voter _ khs =
      maybe khs (`Set.insert` khs) $
        case voter of
          CommitteeVoter cred -> credKeyHashWitness cred
          DRepVoter cred -> credKeyHashWitness cred
          StakePoolVoter poolId -> Just $ asWitness poolId
