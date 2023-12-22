{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.PlutusSpec (spec) where

import Cardano.Ledger.Plutus
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  costModelsSpec


costModelsSpec :: Spec
costModelsSpec =
  describe "CostModels" $ do
    prop "flattenCostModel . mkCostModelsLenient" $ \valid unknown -> do
      let cms1 = flattenCostModel valid <> unknown
          cms2 = unknown <> flattenCostModel valid
      flattenCostModel (mkCostModelsLenient cms1) `shouldBe` cms1
      flattenCostModel (mkCostModelsLenient cms2) `shouldBe` cms2
