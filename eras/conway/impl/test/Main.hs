{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Core (PParams)
import Data.Proxy (Proxy (..))
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Conway.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatify
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as Genesis
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorder
import qualified Test.Cardano.Ledger.Conway.Imp as ConwayImp
import qualified Test.Cardano.Ledger.Conway.PParamsSpec as PParams
import qualified Test.Cardano.Ledger.Core.JSON as JSON
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway" $ do
      Binary.spec
      Cddl.spec
      DRepRatify.spec
      CommitteeRatify.spec
      Genesis.spec
      GovActionReorder.spec
      PParams.spec $ Proxy @Conway
      -- TODO: JSON serialization of CostModels in Conway is not roundtripping
      xdescribe "JSON" $ do
        JSON.roundTripEraSpec @(PParams Conway)
      describe "Imp" $ do
        ConwayImp.spec @Conway
        ShelleyImp.spec @Conway
      describe "CostModels" $ do
        CostModelsSpec.spec @Conway Proxy
