{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Era (ShelleyEra) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), Value)
import Cardano.Ledger.Crypto as CC (Crypto)

data ShelleyEra crypto

instance CC.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c
  type ProtVerLow (ShelleyEra c) = 2

type instance Value (ShelleyEra _c) = Coin
