{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxAuxData () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  hashAlonzoTxAuxData,
  metadataAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  timelockScriptsAlonzoTxAuxDataL,
  translateAlonzoTxAuxData,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Scripts ()

instance EraTxAuxData BabbageEra where
  type TxAuxData BabbageEra = AlonzoTxAuxData BabbageEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  upgradeTxAuxData = translateAlonzoTxAuxData

  hashTxAuxData = hashAlonzoTxAuxData

  validateTxAuxData = validateAlonzoTxAuxData

instance AllegraEraTxAuxData BabbageEra where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL

instance AlonzoEraTxAuxData BabbageEra where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
