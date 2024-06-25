{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.TxAuxData () where

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
import Cardano.Ledger.Babel.Era
import Cardano.Ledger.Babel.Scripts ()
import Cardano.Ledger.Crypto

instance Crypto c => EraTxAuxData (BabelEra c) where
  type TxAuxData (BabelEra c) = AlonzoTxAuxData (BabelEra c)

  mkBasicTxAuxData = AlonzoTxAuxData mempty mempty mempty

  metadataTxAuxDataL = metadataAlonzoTxAuxDataL

  upgradeTxAuxData = translateAlonzoTxAuxData

  hashTxAuxData = hashAlonzoTxAuxData

  validateTxAuxData = validateAlonzoTxAuxData

instance Crypto c => AllegraEraTxAuxData (BabelEra c) where
  timelockScriptsTxAuxDataL = timelockScriptsAlonzoTxAuxDataL

instance Crypto c => AlonzoEraTxAuxData (BabelEra c) where
  plutusScriptsTxAuxDataL = plutusScriptsAllegraTxAuxDataL
