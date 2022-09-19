{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.UTxO () where

import Cardano.Ledger.Alonzo.UTxO
  ( AlonzoScriptsNeeded,
    getAlonzoScriptsHashesNeeded,
    getAlonzoScriptsNeeded,
  )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..))

instance Crypto c => EraUTxO (BabbageEra c) where
  type ScriptsNeeded (BabbageEra c) = AlonzoScriptsNeeded (BabbageEra c)

  getConsumedValue = getConsumedMaryValue

  getScriptsNeeded = getAlonzoScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded
