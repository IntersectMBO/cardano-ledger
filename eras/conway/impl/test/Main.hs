module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatifySpec
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as GenesisSpec
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorderSpec
import qualified Test.Cardano.Ledger.Conway.RatifySpec as RatifySpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway" $ do
      BinarySpec.spec
      RatifySpec.spec
      CommitteeRatifySpec.spec
      GenesisSpec.spec
      GovActionReorderSpec.spec
