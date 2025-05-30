{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxAuxData () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  metadataAlonzoTxAuxDataL,
  plutusScriptsAllegraTxAuxDataL,
  timelockScriptsAlonzoTxAuxDataL,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Scripts ()

instance EraTxAuxData ConwayEra where
  type TxAuxData ConwayEra = AlonzoTxAuxData ConwayEra

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  validateTxAuxData = validateAlonzoTxAuxData

instance AllegraEraTxAuxData ConwayEra where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL

instance AlonzoEraTxAuxData ConwayEra where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
