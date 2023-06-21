{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.UTxO (
  conwayProducedValue,
) where

import Cardano.Ledger.Alonzo.UTxO (
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsHashesNeeded,
  getAlonzoScriptsNeeded,
 )
import Cardano.Ledger.Conway.Core (
  ConwayEraTxBody (..),
  Era (..),
  EraTxBody (..),
  PParams,
  Value,
 )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance (Voter (..), VotingProcedure (..))
import Cardano.Ledger.Conway.Governance.Procedures (ProposalProcedure (..))
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Credential (credScriptHash)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.UTxO (shelleyProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO)
import Cardano.Ledger.Val (Val (..))
import Data.Foldable (Foldable (..))
import Data.Maybe (mapMaybe)
import Lens.Micro ((^.))

getConwayScriptsNeeded ::
  ConwayEraTxBody era =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getConwayScriptsNeeded utxo txb = getAlonzoScriptsNeeded utxo txb <> voterScripts
  where
    getMaybeScriptHash VotingProcedure {vProcVoter} =
      (,) (error "TODO: Implement VoterScriptPurpose")
        <$> case vProcVoter of
          CommitteeVoter cred -> credScriptHash cred
          DRepVoter cred -> credScriptHash cred
          StakePoolVoter _ -> Nothing

    voterScripts =
      AlonzoScriptsNeeded . mapMaybe getMaybeScriptHash . toList $
        txb ^. votingProceduresTxBodyL

conwayProducedValue ::
  ConwayEraTxBody era =>
  PParams era ->
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  TxBody era ->
  Value era
conwayProducedValue pp f txb =
  inject propDeposits
    <> shelleyProducedValue pp f txb
  where
    propDeposits = foldMap pProcDeposit $ txb ^. proposalProceduresTxBodyL

instance Crypto c => EraUTxO (ConwayEra c) where
  type ScriptsNeeded (ConwayEra c) = AlonzoScriptsNeeded (ConwayEra c)

  getConsumedValue = getConsumedMaryValue

  getProducedValue = conwayProducedValue

  getScriptsNeeded = getConwayScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded
