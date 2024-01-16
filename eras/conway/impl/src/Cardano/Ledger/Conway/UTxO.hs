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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.UTxO (
  conwayProducedValue,
  getConwayWitsVKeyNeeded,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsHashesNeeded,
 )
import Cardano.Ledger.Babbage.UTxO (
  getBabbageScriptsProvided,
  getBabbageSpendingDatum,
  getBabbageSupplementalDataHashes,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance.Procedures (
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  unVotingProcedures,
 )
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Credential (credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeededNoGov, shelleyProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), getScriptHash)
import Cardano.Ledger.Val (Val (..), inject)
import Data.Foldable (Foldable (..), toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (strictMaybeToMaybe)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

getConwayScriptsNeeded ::
  (ConwayEraTxBody era, PlutusPurpose AsItem era ~ ConwayPlutusPurpose AsItem era) =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getConwayScriptsNeeded (UTxO utxo) txBody =
  AlonzoScriptsNeeded (spending ++ rewarding ++ certifying ++ minting ++ voting ++ proposing)
  where
    collect !txIn = do
      addr <- view addrTxOutL <$> Map.lookup txIn utxo
      hash <- getScriptHash addr
      return (ConwaySpending (AsItem txIn), hash)

    spending = mapMaybe collect (Set.toList $ txBody ^. inputsTxBodyL)

    rewarding = mapMaybe fromRewardAccount (Map.keys withdrawals)
      where
        withdrawals = unWithdrawals $ txBody ^. withdrawalsTxBodyL
        fromRewardAccount rewardAccount = do
          hash <- credScriptHash $ getRwdCred rewardAccount
          return (ConwayRewarding (AsItem rewardAccount), hash)

    certifying =
      mapMaybe (\cred -> (ConwayCertifying (AsItem cred),) <$> getScriptWitnessTxCert cred) $
        toList (txBody ^. certsTxBodyL)

    minting =
      map (\policyId@(PolicyID hash) -> (ConwayMinting (AsItem policyId), hash)) $
        Set.toList (txBody ^. mintedTxBodyF)

    voting =
      mapMaybe toConwayVoting $
        Map.keys (unVotingProcedures (txBody ^. votingProceduresTxBodyL))
      where
        getVoterScriptHash = \case
          CommitteeVoter cred -> credScriptHash cred
          DRepVoter cred -> credScriptHash cred
          StakePoolVoter _ -> Nothing
        toConwayVoting voter = do
          scriptHash <- getVoterScriptHash voter
          let !purpose = ConwayVoting (AsItem voter)
          pure (purpose, scriptHash)

    proposing =
      mapMaybe proposalPolicy . toList $ txBody ^. proposalProceduresTxBodyL
      where
        proposalPolicy proposal@ProposalProcedure {pProcGovAction} = do
          govPolicy <- case pProcGovAction of
            ParameterChange _ _ p -> strictMaybeToMaybe p
            TreasuryWithdrawals _ p -> strictMaybeToMaybe p
            _ -> Nothing
          pure (ConwayProposing (AsItem proposal), govPolicy)

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
