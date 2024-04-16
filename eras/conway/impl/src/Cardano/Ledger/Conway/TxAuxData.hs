{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxAuxData () where

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
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Crypto

instance Crypto c => EraTxAuxData (ConwayEra c) where
  type TxAuxData (ConwayEra c) = AlonzoTxAuxData (ConwayEra c)

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  upgradeTxAuxData = translateAlonzoTxAuxData

  hashTxAuxData = hashAlonzoTxAuxData

  validateTxAuxData = validateAlonzoTxAuxData

instance Crypto c => AllegraEraTxAuxData (ConwayEra c) where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL

instance Crypto c => AlonzoEraTxAuxData (ConwayEra c) where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
