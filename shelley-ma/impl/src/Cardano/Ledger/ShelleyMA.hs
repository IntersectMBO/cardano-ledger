{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.ShelleyMA where

import qualified Cardano.Ledger.Crypto
import Cardano.Ledger.Era

-- | The Shelley Multiasset era
data ShelleyMA c

instance Cardano.Ledger.Crypto.Crypto c => Era (ShelleyMA c) where
  type Crypto (ShelleyMA c) = c
