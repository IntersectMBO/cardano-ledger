{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.UTxO () where

import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded,
  getAlonzoScriptsHashesNeeded,
 )
import Cardano.Ledger.Babbage.UTxO (
  getBabbageScriptsProvided,
  getBabbageSpendingDatum,
  getBabbageSupplementalDataHashes,
 )
import Cardano.Ledger.Conway.UTxO (
  conwayConsumed,
  conwayProducedValue,
  getConwayMinFeeTxUtxo,
  getConwayScriptsNeeded,
  getConwayWitsVKeyNeeded,
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.State (EraUTxO (..))
import Cardano.Ledger.Dijkstra.State.CertState ()
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)

instance EraUTxO DijkstraEra where
  type ScriptsNeeded DijkstraEra = AlonzoScriptsNeeded DijkstraEra

  consumed = conwayConsumed

  getConsumedValue = getConsumedMaryValue

  getProducedValue = conwayProducedValue

  getScriptsProvided = getBabbageScriptsProvided

  getScriptsNeeded = getConwayScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded _ = getConwayWitsVKeyNeeded

  getMinFeeTxUtxo = getConwayMinFeeTxUtxo

instance AlonzoEraUTxO DijkstraEra where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum
