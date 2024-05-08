{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.PlutusSpec (spec) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  ProtVer (..),
  UnitInterval,
 )
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Plutus
import Data.Map.Strict (Map)
import Data.Word
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Plutus.ToPlutusData (roundTripPlutusDataSpec)

spec :: Spec
spec = do
  costModelsSpec
  describe "roundtrip ToPlutusData" $ do
    roundTripPlutusDataSpec @Version
    roundTripPlutusDataSpec @Word
    roundTripPlutusDataSpec @Word8
    roundTripPlutusDataSpec @Word16
    roundTripPlutusDataSpec @Word32
    roundTripPlutusDataSpec @[Word]
    roundTripPlutusDataSpec @[Word8]
    roundTripPlutusDataSpec @(Map Word Version)
    roundTripPlutusDataSpec @Coin
    roundTripPlutusDataSpec @ExUnits
    roundTripPlutusDataSpec @Prices
    roundTripPlutusDataSpec @Natural
    roundTripPlutusDataSpec @UnitInterval
    roundTripPlutusDataSpec @EpochInterval
    roundTripPlutusDataSpec @NonNegativeInterval
    roundTripPlutusDataSpec @ProtVer
    roundTripPlutusDataSpec @CostModels
    roundTripPlutusDataSpec @Integer

costModelsSpec :: Spec
costModelsSpec = do
  describe "CostModels" $ do
    prop "flattenCostModels . mkCostModelsLenient" $ \valid unknown -> do
      let cms1Flat = flattenCostModels valid <> unknown
          cms2Flat = unknown <> flattenCostModels valid
      cms1 <- mkCostModelsLenient cms1Flat
      cms2 <- mkCostModelsLenient cms2Flat
      flattenCostModels cms1 `shouldBe` cms1Flat
      flattenCostModels cms2 `shouldBe` cms2Flat
