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
import Test.Cardano.Ledger.Plutus.ToPlutusData (plutusDataRoundtripSpec)

spec :: Spec
spec = do
  costModelsSpec

costModelsSpec :: Spec
costModelsSpec = do
  describe "CostModels" $ do
    prop "flattenCostModels . mkCostModelsLenient" $ \valid unknown -> do
      let cms1 = flattenCostModels valid <> unknown
          cms2 = unknown <> flattenCostModels valid
      flattenCostModels (mkCostModelsLenient cms1) `shouldBe` cms1
      flattenCostModels (mkCostModelsLenient cms2) `shouldBe` cms2
  plutusDataRoundtripSpec
