{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.JSON (
  roundTripJsonConwayEraSpec,
) where

import Cardano.Ledger.Conway.Transition
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Era
import Test.Cardano.Ledger.Core.JSON

roundTripJsonConwayEraSpec :: forall era. ConayEraTest era => Spec
roundTripJsonConwayEraSpec =
  describe ("Conway era JSON Roundtrip: " <> eraName @era) $ do
    roundTripJsonSpec @DRep
