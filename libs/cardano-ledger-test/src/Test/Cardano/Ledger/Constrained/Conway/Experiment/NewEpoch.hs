-- | Specs necessary to generate state for the NEWEPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.Experiment.NewEpoch where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.API.Types
import Constrained.Experiment.API

newEpochStateSpec :: Specification (NewEpochState ConwayEra)
newEpochStateSpec = TrueSpec
