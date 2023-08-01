{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Babbage.TxAuxData () where

import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData,
  hashAlonzoTxAuxData,
  validateAlonzoTxAuxData,
  translateAlonzoTxAuxData
 )
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto

instance Crypto c => EraTxAuxData (BabbageEra c) where
  type TxAuxData (BabbageEra c) = AlonzoTxAuxData (BabbageEra c)

  upgradeTxAuxData = translateAlonzoTxAuxData

  hashTxAuxData = hashAlonzoTxAuxData

  validateTxAuxData = validateAlonzoTxAuxData
