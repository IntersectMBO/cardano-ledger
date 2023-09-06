{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.BinarySpec (spec) where

import Cardano.Ledger.Mary
import Data.Default.Class (def)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

spec :: Spec
spec = do
  specUpgrade @Mary def
  describe "RoundTrip" $ do
    roundTripShelleyCommonSpec @Mary
