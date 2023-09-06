{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.BinarySpec (spec) where

import Cardano.Ledger.Allegra
import Data.Default.Class (def)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

spec :: Spec
spec = do
  specUpgrade @Allegra def
  describe "RoundTrip" $ do
    roundTripShelleyCommonSpec @Allegra
