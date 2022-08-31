{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.UTxO () where

import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), getConsumedCoin)
import Cardano.Ledger.ShelleyMA.Era (MaryOrAllegra (Allegra), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.TxBody ()

instance Crypto c => EraUTxO (ShelleyMAEra 'Allegra c) where
  getConsumedValue = getConsumedCoin
