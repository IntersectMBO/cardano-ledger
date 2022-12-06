{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.UTxO () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.TxBody ()
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.UTxO
  ( ShelleyScriptsNeeded (..),
    getConsumedCoin,
    getShelleyScriptsNeeded,
  )
import Cardano.Ledger.UTxO (EraUTxO (..))

instance Crypto c => EraUTxO (AllegraEra c) where
  type ScriptsNeeded (AllegraEra c) = ShelleyScriptsNeeded (AllegraEra c)

  getConsumedValue = getConsumedCoin

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes
