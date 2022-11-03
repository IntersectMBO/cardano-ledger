{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.UTxO () where

import Cardano.Ledger.Alonzo.UTxO
  ( AlonzoScriptsNeeded,
    getAlonzoScriptsHashesNeeded,
    getAlonzoScriptsNeeded,
  )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.LedgerState (DPState)
import Cardano.Ledger.UTxO (EraUTxO (..))

instance Crypto c => EraUTxO (ConwayEra c) where
  type ScriptsNeeded (ConwayEra c) = AlonzoScriptsNeeded (ConwayEra c)
  type DepositInfo (ConwayEra c) = DPState c

  getConsumedValue = getConsumedMaryValue

  getScriptsNeeded = getAlonzoScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded
