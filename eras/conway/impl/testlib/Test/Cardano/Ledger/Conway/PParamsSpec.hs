{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.PParamsSpec (spec) where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModelApplyError (..), CostModelError (..), CostModels (..), mkCostModel)
import Cardano.Ledger.Conway.Core (AlonzoEraPParams, EraPParams (..), emptyPParams, emptyPParamsUpdate, ppCostModelsL, ppuCostModelsL)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Alonzo.CostModel (costModelParamsCount)
import Test.Cardano.Ledger.Common

spec :: forall era. AlonzoEraPParams era => Proxy era -> Spec
spec _ = describe "PParams updates" $ do
  it "CostModels update per language" $ do
    let
      mkTestCostModel pv x =
        either
          (\e -> error $ "Failed to make a costmodel:\n" <> show e)
          id
          (mkCostModel pv [x | _ <- [1 .. costModelParamsCount pv]])
      langCostModels =
        Map.fromList
          [ (PlutusV1, mkTestCostModel PlutusV1 1)
          , (PlutusV2, mkTestCostModel PlutusV2 2)
          ]
      updateCostModels =
        Map.fromList
          [ (PlutusV2, mkTestCostModel PlutusV2 3)
          ]
      expectedCostModels =
        Map.fromList
          [ (PlutusV1, mkTestCostModel PlutusV1 1)
          , (PlutusV2, mkTestCostModel PlutusV2 3)
          ]
      langUnk =
        Map.fromList
          [ (1, [1, 2, 3])
          , (2, [4, 5, 6])
          ]
      unkUpdate =
        Map.fromList
          [ (2, [7, 8, 9])
          ]
      unkExpected =
        Map.fromList
          [ (1, [1, 2, 3])
          , (2, [7, 8, 9])
          ]
      errs =
        Map.fromList
          [ (PlutusV1, CostModelError $ CMUnknownParamError "foo")
          , (PlutusV2, CostModelError $ CMUnknownParamError "bar")
          ]
      errsUpdate =
        Map.fromList
          [ (PlutusV2, CostModelError $ CMUnknownParamError "baz")
          ]
      errsExpected =
        Map.fromList
          [ (PlutusV1, CostModelError $ CMUnknownParamError "foo")
          , (PlutusV2, CostModelError $ CMUnknownParamError "baz")
          ]
      pp =
        emptyPParams
          & ppCostModelsL .~ CostModels langCostModels errs langUnk
      ppUpd =
        emptyPParamsUpdate
          & ppuCostModelsL
            .~ SJust
              (CostModels updateCostModels errsUpdate unkUpdate)
      ppExpected =
        pp
          & ppCostModelsL .~ CostModels expectedCostModels errsExpected unkExpected

    applyPPUpdates @era pp ppUpd `shouldBe` ppExpected
