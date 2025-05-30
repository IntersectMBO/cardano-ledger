{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxAuxData () where

import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  metadataAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  timelockScriptsAlonzoTxAuxDataL,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Conway.Core (
  AllegraEraTxAuxData (..),
  AlonzoEraTxAuxData (..),
  EraTxAuxData (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()

instance EraTxAuxData DijkstraEra where
  type TxAuxData DijkstraEra = AlonzoTxAuxData DijkstraEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  validateTxAuxData = validateAlonzoTxAuxData

instance AllegraEraTxAuxData DijkstraEra where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL

instance AlonzoEraTxAuxData DijkstraEra where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
