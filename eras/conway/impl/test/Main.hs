module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatifySpec
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatifySpec
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as GenesisSpec
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorderSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway" $ do
      BinarySpec.spec
      DRepRatifySpec.spec
      CommitteeRatifySpec.spec
      GenesisSpec.spec
      GovActionReorderSpec.spec
