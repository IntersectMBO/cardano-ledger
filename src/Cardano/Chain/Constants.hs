-- | Non-configurable constants
--   For configurable constants, see Cardano.Genesis.Configuration.

module Cardano.Chain.Constants
       ( sharedSeedLength
       , accountGenesisIndex
       , wAddressGenesisIndex
       ) where

import           Cardano.Prelude

import           Cardano.Crypto.HD (firstHardened)

-- | First index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = firstHardened

-- | Second index in derivation path for HD account, which is put to genesis
-- utxo
wAddressGenesisIndex :: Word32
wAddressGenesisIndex = firstHardened

--------------------------------------------------------------------------------
-- Constants which are not configurable
--------------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32
