{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.TxInfo ()
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import qualified Test.Cardano.Ledger.Conway.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Conway.Binary.Regression as Regression
import qualified Test.Cardano.Ledger.Conway.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatify
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as Genesis
import Test.Cardano.Ledger.Conway.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Conway.GoldenTranslation as GoldenTranslation
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorder
import qualified Test.Cardano.Ledger.Conway.Imp as Imp
import Test.Cardano.Ledger.Conway.Plutus.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.Conway.Proposals as Proposals
import qualified Test.Cardano.Ledger.Conway.SPORatifySpec as SPORatifySpec
import qualified Test.Cardano.Ledger.Conway.Spec as Spec
import qualified Test.Cardano.Ledger.Conway.TxInfoSpec as TxInfo
import qualified Test.Cardano.Ledger.Core.EraPParamsSpec as EraPParams
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway" $ do
      GoldenTranslation.spec
      Golden.spec
      Spec.spec
      Proposals.spec
      Binary.spec
      Cddl.spec
      DRepRatify.spec
      CommitteeRatify.spec
      SPORatifySpec.spec
      Genesis.spec
      GovActionReorder.spec
      roundTripJsonEraSpec @ConwayEra
      describe "Imp" $
        Imp.spec @ConwayEra
      describe "CostModels" $ do
        CostModelsSpec.spec @ConwayEra
      describe "TxWits" $ do
        TxWitsSpec.spec @ConwayEra
      describe "Plutus" $ do
        PlutusSpec.spec
      Regression.spec @ConwayEra
      TxInfo.spec
      EraPParams.spec @ConwayEra
      eraPParamsPlutusDataSpec

eraPParamsPlutusDataSpec :: HasCallStack => Spec
eraPParamsPlutusDataSpec = describe "EraPParams PlutusData" $ do
  prop "PParams toPlutusData equivalence" $ \(ppu :: PParamsUpdate ConwayEra) -> do
    let pd1 = toPlutusData ppu
    let pd2 = toPlutusDataPParamsUpdate @ConwayEra ppu
    pd1 `shouldBe` pd2

    let deserialized = fromPlutusDataPParamsUpdate pd1
    deserialized `shouldBe` Just ppu
