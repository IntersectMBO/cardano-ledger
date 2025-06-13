{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Spec (
  eraSpec,
) where

import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Core.Binary.Annotator (decoderEquivalenceCoreEraTypesSpec)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripCoreEraTypesSpec)

-- | This is the `Spec` that is applicable to all eras
eraSpec :: forall era. EraTest era => Spec
eraSpec =
  describe "Every era spec" $ do
    describe "Serialization" $ do
      describe "Binary" $ do
        roundTripCoreEraTypesSpec @era
        roundTripAllPredicateFailures @era
        decoderEquivalenceCoreEraTypesSpec @era
