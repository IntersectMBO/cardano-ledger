{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.GenesisSpec (spec) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Plutus.CostModels (costModelsValid, getCostModelParams)
import Cardano.Ledger.Plutus.Language (Language (PlutusV4))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Examples (exampleDijkstraGenesis)

spec :: Spec
spec = do
  describe "DijkstraGenesis" $ do
    prop "Upgrades" propDijkstraPParamsUpgrade
    it "round-trips the complete example genesis with the proposed V4 cost model" $ do
      let genesis = exampleDijkstraGenesis
      length (getCostModelParams (udppPlutusV4CostModel (dgUpgradePParams genesis))) `shouldBe` 369
      Aeson.eitherDecode (Aeson.encode genesis) `shouldBe` Right genesis
    prop "round-trips generated complete genesis through strict JSON" $ \(genesis :: DijkstraGenesis) ->
      Aeson.eitherDecode (Aeson.encode genesis) === Right genesis
    forM_ ([251, 368, 370] :: [Int]) $ \parameterCount ->
      it ("rejects a genesis V4 cost model with " <> show parameterCount <> " parameters") $ do
        let params = getCostModelParams $ udppPlutusV4CostModel $ dgUpgradePParams exampleDijkstraGenesis
            malformedParams = take parameterCount (params <> repeat 0)
        case Aeson.toJSON exampleDijkstraGenesis of
          Aeson.Object genesisFields -> case KeyMap.lookup "plutusV4CostModel" genesisFields of
            Just (Aeson.Array _) -> do
              let malformed =
                    Aeson.Object $
                      KeyMap.insert
                        "plutusV4CostModel"
                        (Aeson.toJSON malformedParams)
                        genesisFields
                  result = Aeson.eitherDecode (Aeson.encode malformed) :: Either String DijkstraGenesis
              case result of
                Left err ->
                  err
                    `shouldContain` ( "Number of parameters supplied "
                                        <> show parameterCount
                                        <> " does not match the expected number of 369"
                                    )
                Right _ -> assertFailure "Accepted a genesis V4 cost model with an incorrect initial parameter count"
            _ -> assertFailure "Example genesis is missing its V4 cost-model array"
          _ -> assertFailure "Example genesis must encode as a JSON object"
    it "clears the Peras bootstrap round with a nested update" $ do
      let pp = (emptyPParams & ppPerasBootstrapRoundL .~ SJust 42) :: PParams DijkstraEra
          ppu = (emptyPParamsUpdate & ppuPerasBootstrapRoundL .~ SJust SNothing) :: PParamsUpdate DijkstraEra
          pp' = applyPPUpdates pp ppu
      pp' ^. ppPerasBootstrapRoundL `shouldBe` SNothing

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
  pp' ^. ppPerasMinCandidateBlockAgeL `shouldBe` udppPerasMinCandidateBlockAge ppu
  pp' ^. ppPerasHealingFactorL `shouldBe` udppPerasHealingFactor ppu
  pp' ^. ppPerasCertBoostL `shouldBe` udppPerasCertBoost ppu
  pp' ^. ppPerasTargetCommitteeSizeL `shouldBe` udppPerasTargetCommitteeSize ppu
  pp' ^. ppPerasBootstrapRoundL `shouldBe` udppPerasBootstrapRound ppu
  pp' ^. ppPerasQuorumThresholdSafetyMarginL `shouldBe` udppPerasQuorumThresholdSafetyMargin ppu
  -- The PlutusV4 CostModel from DijkstraGenesis must win over any pre-existing entry
  Map.lookup PlutusV4 newCostModels `shouldBe` Just (udppPlutusV4CostModel ppu)
  -- All other cost models must carry over from Conway unchanged
  Map.delete PlutusV4 newCostModels `shouldBe` Map.delete PlutusV4 oldCostModels
