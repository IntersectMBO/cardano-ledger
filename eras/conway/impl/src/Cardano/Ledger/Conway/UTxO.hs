{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.UTxO () where

import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..))

instance Crypto c => EraUTxO (ConwayEra c) where
  getConsumedValue = getConsumedMaryValue
