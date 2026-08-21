{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.GenesisSpec (spec) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Language (Language (PlutusV4))
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

spec :: Spec
spec = do
  describe "DijkstraGenesis" $ do
    prop "Upgrades" propDijkstraPParamsUpgrade

propDijkstraPParamsUpgrade ::
  UpgradeDijkstraPParams Identity DijkstraEra -> PParams ConwayEra -> Property
propDijkstraPParamsUpgrade ppu pp = property $ do
  let pp' = upgradePParams ppu pp :: PParams DijkstraEra
      oldCostModels = costModelsValid (pp ^. ppCostModelsL)
      newCostModels = costModelsValid (pp' ^. ppCostModelsL)
  pp' ^. ppMaxRefScriptSizePerBlockL `shouldBe` udppMaxRefScriptSizePerBlock ppu
  pp' ^. ppMaxRefScriptSizePerTxL `shouldBe` udppMaxRefScriptSizePerTx ppu
  pp' ^. ppRefScriptCostStrideL `shouldBe` udppRefScriptCostStride ppu
  pp' ^. ppRefScriptCostMultiplierL `shouldBe` udppRefScriptCostMultiplier ppu
  pp' ^. ppMaxPledgeLeverageL `shouldBe` udppMaxPledgeLeverage ppu
  pp' ^. ppMinPoolMarginL `shouldBe` udppMinPoolMargin ppu
  -- The PlutusV4 CostModel from DijkstraGenesis must win over any pre-existing entry
  Map.lookup PlutusV4 newCostModels `shouldBe` Just (udppPlutusV4CostModel ppu)
  -- All other cost models must carry over from Conway unchanged
  Map.delete PlutusV4 newCostModels `shouldBe` Map.delete PlutusV4 oldCostModels
