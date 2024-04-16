{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxAuxData () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  hashAlonzoTxAuxData,
  metadataAlonzoTxAuxDataL,
  timelockScriptsAlonzoTxAuxDataL,
  translateAlonzoTxAuxData,
  validateAlonzoTxAuxData,
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.Crypto

instance Crypto c => EraTxAuxData (BabbageEra c) where
  type TxAuxData (BabbageEra c) = AlonzoTxAuxData (BabbageEra c)

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  upgradeTxAuxData = translateAlonzoTxAuxData

  hashTxAuxData = hashAlonzoTxAuxData

  validateTxAuxData = validateAlonzoTxAuxData

instance Crypto c => AllegraEraTxAuxData (BabbageEra c) where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL
