{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.JSON (
  roundTripJsonShelleyEraSpec,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Transition
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON
import Test.Cardano.Ledger.Shelley.Era

roundTripJsonShelleyEraSpec :: forall era. ShelleyEraTest era => Spec
roundTripJsonShelleyEraSpec =
  describe ("Shelley era JSON Roundtrip: " <> eraName @era) $ do
    roundTripJsonSpec @(TransitionConfig era)
