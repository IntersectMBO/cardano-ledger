{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxAuxData () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (SupportedPlutusRunnable (..))
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  metadataAlonzoTxAuxDataL,
  nativeScriptsAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Scripts ()
import Data.Map.Strict (Map)

instance
  StAnnTxCache ConwayEra ~ Map ScriptHash (SupportedPlutusRunnable ConwayEra) =>
  EraTxAuxData ConwayEra
  where
  type TxAuxData ConwayEra = AlonzoTxAuxData ConwayEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  validateTxAuxData = validateAlonzoTxAuxData

instance
  StAnnTxCache ConwayEra ~ Map ScriptHash (SupportedPlutusRunnable ConwayEra) =>
  AllegraEraTxAuxData ConwayEra
  where
  nativeScriptsTxAuxDataL = nativeScriptsAlonzoTxAuxDataL

instance
  StAnnTxCache ConwayEra ~ Map ScriptHash (SupportedPlutusRunnable ConwayEra) =>
  AlonzoEraTxAuxData ConwayEra
  where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
