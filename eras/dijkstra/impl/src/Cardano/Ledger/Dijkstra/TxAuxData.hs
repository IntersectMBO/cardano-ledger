{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxAuxData () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (SupportedPlutusRunnable (..))
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  metadataAlonzoTxAuxDataL,
  nativeScriptsAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Data.Map.Strict (Map)

instance
  StAnnTxCache DijkstraEra ~ Map ScriptHash (SupportedPlutusRunnable DijkstraEra) =>
  EraTxAuxData DijkstraEra
  where
  type TxAuxData DijkstraEra = AlonzoTxAuxData DijkstraEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  validateTxAuxData = validateAlonzoTxAuxData

instance
  StAnnTxCache DijkstraEra ~ Map ScriptHash (SupportedPlutusRunnable DijkstraEra) =>
  AllegraEraTxAuxData DijkstraEra
  where
  nativeScriptsTxAuxDataL = nativeScriptsAlonzoTxAuxDataL

instance
  StAnnTxCache DijkstraEra ~ Map ScriptHash (SupportedPlutusRunnable DijkstraEra) =>
  AlonzoEraTxAuxData DijkstraEra
  where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
