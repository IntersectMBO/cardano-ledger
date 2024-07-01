-- | Specs necessary to generate state for the NEWEPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.NewEpoch where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API.Types
import Constrained

newEpochStateSpec :: Specification fn (NewEpochState (ConwayEra StandardCrypto))
newEpochStateSpec = TrueSpec
