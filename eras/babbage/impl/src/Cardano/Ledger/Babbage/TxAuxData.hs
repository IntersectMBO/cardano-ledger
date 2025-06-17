{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxAuxData () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  metadataAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  timelockScriptsAlonzoTxAuxDataL,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Scripts ()

instance EraTxAuxData BabbageEra where
  type TxAuxData BabbageEra = AlonzoTxAuxData BabbageEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL
  validateTxAuxData = validateAlonzoTxAuxData

instance AllegraEraTxAuxData BabbageEra where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL

instance AlonzoEraTxAuxData BabbageEra where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
