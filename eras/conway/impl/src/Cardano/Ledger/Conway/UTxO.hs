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

import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core (
  Era (..),
  EraTxBody (..),
  MaryEraTxBody,
  PParams,
  Value,
 )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance.Procedures (
  ProposalProcedure (..),
  Voter (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepDepositL)
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert,
  pattern RegDRepTxCert,
  pattern UnRegDRepTxCert,
 )
import Cardano.Ledger.Credential (Credential, credScriptHash)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.UTxO (shelleyProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO)
import Cardano.Ledger.Val (Val (..))
import Data.Foldable (foldMap', foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (getSum)
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
  ( ConwayEraTxBody era
  , ConwayEraPParams era
  ) =>
  PParams era ->
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  TxBody era ->
  Value era
conwayProducedValue pp isStakePool txBody =
  inject propDeposits
    <+> inject (txBody ^. treasuryDonationTxBodyL)
    <+> shelleyProducedValue pp isStakePool txBody
    <+> inject drepDeposits
  where
    propDeposits = foldMap pProcDeposit $ txBody ^. proposalProceduresTxBodyL
    drepDeposits = getSum @Int (foldMap' (\case RegDRepTxCert {} -> 1; _ -> 0) (txBody ^. certsTxBodyL)) <×> pp ^. ppDRepDepositL

conwayConsumedValue ::
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , MaryEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  PParams era ->
  (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
  (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
  UTxO era ->
  TxBody era ->
  Value era
conwayConsumedValue pp lookupKeyDeposit lookupDRepDeposit utxo txBody =
  let maryRefunds = getConsumedMaryValue pp lookupKeyDeposit utxo txBody
      drepRefunds =
        let go accum@(!drepRegsInTx, !totalRefund) = \case
              RegDRepTxCert c _ _ ->
                -- Track registrations
                (Set.insert c drepRegsInTx, totalRefund)
              UnRegDRepTxCert c _
                -- DRep previously registered in the same tx.
                | Set.member c drepRegsInTx -> (Set.delete c drepRegsInTx, totalRefund <+> pp ^. ppDRepDepositL)
                -- DRep previously registered in some other tx.
                | Just deposit <- lookupDRepDeposit c -> (drepRegsInTx, totalRefund <+> deposit)
              _ -> accum
         in inject $ snd $ foldl' go (Set.empty, Coin 0) $ txBody ^. certsTxBodyL
   in maryRefunds <+> drepRefunds

instance Crypto c => EraUTxO (ConwayEra c) where
  type ScriptsNeeded (ConwayEra c) = AlonzoScriptsNeeded (ConwayEra c)

  getConsumedValue = conwayConsumedValue

  getProducedValue = conwayProducedValue

  getScriptsProvided = getBabbageScriptsProvided

  getScriptsNeeded = getConwayScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded -- TODO: This also changes for Conway

instance Crypto c => AlonzoEraUTxO (ConwayEra c) where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum
