-- | Specs necessary to generate state for the NEWEPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.NewEpoch where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.API.Types
import Constrained.API

newEpochStateSpec :: Specification (NewEpochState ConwayEra)
newEpochStateSpec = TrueSpec
