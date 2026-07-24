{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxAuxData () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (SupportedPlutusRunnable (..))
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  metadataAlonzoTxAuxDataL,
  nativeScriptsAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Scripts ()
import Data.Map (Map)

instance
  StAnnTxCache BabbageEra ~ Map ScriptHash (SupportedPlutusRunnable BabbageEra) =>
  EraTxAuxData BabbageEra
  where
  type TxAuxData BabbageEra = AlonzoTxAuxData BabbageEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL
  validateTxAuxData = validateAlonzoTxAuxData

instance
  StAnnTxCache BabbageEra ~ Map ScriptHash (SupportedPlutusRunnable BabbageEra) =>
  AllegraEraTxAuxData BabbageEra
  where
  nativeScriptsTxAuxDataL = nativeScriptsAlonzoTxAuxDataL

instance
  StAnnTxCache BabbageEra ~ Map ScriptHash (SupportedPlutusRunnable BabbageEra) =>
  AlonzoEraTxAuxData BabbageEra
  where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
