{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.UTxO (
  babelProducedValue,
  getBabelWitsVKeyNeeded,
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
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (
  ConwayEraTxBody (proposalProceduresTxBodyL, votingProceduresTxBodyL),
  treasuryDonationTxBodyL,
 )
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  unVotingProcedures,
 )
import Cardano.Ledger.Credential (credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue, getProducedMaryValue)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeededNoGov)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Ledger.Val (Val (..), inject)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import Lens.Micro ((^.))

getBabelScriptsNeeded ::
  BabelEraTxBody era =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getBabelScriptsNeeded utxo txBody =
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

babelProducedValue ::
  (BabelEraTxBody era, Value era ~ MaryValue (EraCrypto era)) =>
  PParams era ->
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  TxBody era ->
  Value era
babelProducedValue pp isStakePool txBody =
  getProducedMaryValue pp isStakePool txBody
    <+> inject (txBody ^. treasuryDonationTxBodyL)

instance Crypto c => EraUTxO (BabelEra c) where
  type ScriptsNeeded (BabelEra c) = AlonzoScriptsNeeded (BabelEra c)

  getConsumedValue = getConsumedMaryValue

  getProducedValue = babelProducedValue

  getScriptsProvided = getBabbageScriptsProvided

  getScriptsNeeded = getBabelScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded _ = getBabelWitsVKeyNeeded

  getMinFeeTxUtxo = getBabelMinFeeTxUtxo

instance Crypto c => AlonzoEraUTxO (BabelEra c) where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum

getBabelMinFeeTxUtxo ::
  ( EraTx era
  , BabbageEraTxBody era
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Coin
getBabelMinFeeTxUtxo pparams tx utxo =
  getMinFeeTx pparams tx refScriptsSize
  where
    ins = (tx ^. bodyTxL . referenceInputsTxBodyL) `Set.union` (tx ^. bodyTxL . inputsTxBodyL)
    refScripts = getReferenceScriptsNonDistinct utxo ins
    refScriptsSize = getSum $ foldMap (Sum . originalBytesSize . snd) refScripts

getBabelWitsVKeyNeeded ::
  (EraTx era, BabelEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
getBabelWitsVKeyNeeded utxo txBody =
  getShelleyWitsVKeyNeededNoGov utxo txBody
    `Set.union` (txBody ^. reqSignerHashesTxBodyL)
    `Set.union` voterWitnesses txBody

voterWitnesses ::
  BabelEraTxBody era =>
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
