{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.UTxO () where

import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.UTxO
  ( EraUTxO (..),
    ShelleyScriptsNeeded (..),
    getConsumedCoin,
    getShelleyScriptsNeeded,
  )
import Cardano.Ledger.ShelleyMA.Era (MaryOrAllegra (Allegra), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.TxBody ()

instance Crypto c => EraUTxO (ShelleyMAEra 'Allegra c) where
  type ScriptsNeeded (ShelleyMAEra 'Allegra c) = ShelleyScriptsNeeded (ShelleyMAEra 'Allegra c)

  getConsumedValue = getConsumedCoin

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes
