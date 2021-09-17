-- | Non-configurable constants
--   For configurable constants, see Cardano.Genesis.Configuration.
module Cardano.Chain.Constants
  ( sharedSeedLength,
  )
where

import Cardano.Prelude

--------------------------------------------------------------------------------
-- Constants which are not configurable
--------------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32
