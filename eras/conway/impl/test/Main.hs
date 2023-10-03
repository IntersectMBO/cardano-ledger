{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (Conway)
import Data.Proxy (Proxy (..))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatifySpec
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatifySpec
import qualified Test.Cardano.Ledger.Conway.EpochSpec as EpochSpec
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as GenesisSpec
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorderSpec
import qualified Test.Cardano.Ledger.Conway.GovSpec as GovSpec
import qualified Test.Cardano.Ledger.Conway.PParamsSpec as PParamsSpec

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway" $ do
      BinarySpec.spec
      DRepRatifySpec.spec
      CommitteeRatifySpec.spec
      GenesisSpec.spec
      GovActionReorderSpec.spec
      EpochSpec.spec
      GovSpec.spec
      PParamsSpec.spec $ Proxy @Conway
