{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.BinarySpec (spec) where

import Cardano.Ledger.Shelley (ShelleyEra)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as Binary (decoderEquivalenceCoreEraTypesSpec, txSizeSpec)
import qualified Test.Cardano.Ledger.Shelley.Binary.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Shelley.Binary.RoundTripSpec as RoundTrip

spec :: Spec
spec = do
  Golden.spec
  RoundTrip.spec
  Binary.decoderEquivalenceCoreEraTypesSpec @ShelleyEra
  Binary.txSizeSpec @ShelleyEra
