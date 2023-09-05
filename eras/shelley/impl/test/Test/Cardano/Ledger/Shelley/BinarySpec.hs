module Test.Cardano.Ledger.Shelley.BinarySpec (spec) where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.Binary.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Shelley.Binary.RoundTripSpec as RoundTrip

spec :: Spec
spec = do
  Golden.spec
  RoundTrip.spec
