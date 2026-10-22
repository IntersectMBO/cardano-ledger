{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.PlutusSpec (spec) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  NonZero,
  PositiveInterval,
  ProtVer (..),
  StrictMaybe,
  UnitInterval,
  getVersion,
  strictMaybeToMaybe,
  unboundRational,
 )
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Cardano.Slotting.Slot (SlotInterval (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Plutus.ToPlutusData (roundTripPlutusDataSpec, sameAsToBuiltinDataSpec)

spec :: Spec
spec = do
  describe "Plutus" $ do
    costModelsSpec
    exUnitsSpec
    dataSpec

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

exUnitsSpec :: Spec
exUnitsSpec = do
  describe "ExUnits" $ do
    prop "Round-trip to ExBudget" exUnitsToExBudgetRoundTrip
    prop "Round-trip from ExBudget" exBudgetToExUnitsRoundTrip

-- ExUnits should remain intact when translating to and from the Plutus ExBudget type
exUnitsToExBudgetRoundTrip :: Gen Property
exUnitsToExBudgetRoundTrip = do
  e <- arbitrary
  let result = exBudgetToExUnits $ transExUnits e
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e

-- Plutus ExBudget should remain intact when translating to and from the ExUnits type
exBudgetToExUnitsRoundTrip :: Gen Property
exBudgetToExUnitsRoundTrip = do
  e <- arbitrary
  let result = transExUnits <$> exBudgetToExUnits e
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e

dataSpec :: Spec
dataSpec = describe "ToPlutusData" $ do
  describe "RoundTrip" $ do
    roundTripPlutusDataSpec @Version
    roundTripPlutusDataSpec @ProtVer
    roundTripPlutusDataSpec @Rational
    roundTripPlutusDataSpec @UnitInterval
    roundTripPlutusDataSpec @NonNegativeInterval
    roundTripPlutusDataSpec @PositiveInterval
    roundTripPlutusDataSpec @CostModels
    roundTripPlutusDataSpec @ExUnits
    roundTripPlutusDataSpec @Prices
    roundTripPlutusDataSpec @Coin
    roundTripPlutusDataSpec @(CompactForm Coin)
    roundTripPlutusDataSpec @CoinPerByte
    roundTripPlutusDataSpec @MaxPledgeLeverage
    roundTripPlutusDataSpec @Word32
    roundTripPlutusDataSpec @Word16
    roundTripPlutusDataSpec @Word8
    roundTripPlutusDataSpec @EpochInterval
    roundTripPlutusDataSpec @SlotInterval
    roundTripPlutusDataSpec @Natural
    roundTripPlutusDataSpec @Integer
    roundTripPlutusDataSpec @Word
    roundTripPlutusDataSpec @(NonZero Integer)
    roundTripPlutusDataSpec @[Word]
    roundTripPlutusDataSpec @[Word8]
    roundTripPlutusDataSpec @(Map Word Version)
  describe "Same as ToData" $ do
    describe "PlutusV4" $ do
      sameAsToBuiltinDataSpec @Integer id
      sameAsToBuiltinDataSpec @Word8 toInteger
      sameAsToBuiltinDataSpec @Word16 toInteger
      sameAsToBuiltinDataSpec @Word32 toInteger
      sameAsToBuiltinDataSpec @Natural toInteger
      sameAsToBuiltinDataSpec @Rational PV4.fromHaskellRatio
      sameAsToBuiltinDataSpec @UnitInterval (PV4.fromHaskellRatio . unboundRational)
      sameAsToBuiltinDataSpec @NonNegativeInterval (PV4.fromHaskellRatio . unboundRational)
      sameAsToBuiltinDataSpec @PositiveInterval (PV4.fromHaskellRatio . unboundRational)
      sameAsToBuiltinDataSpec @MaxPledgeLeverage $
        fmap (PV4.fromHaskellRatio . unboundRational)
          . strictMaybeToMaybe
          . unMaxPledgeLeverage
      sameAsToBuiltinDataSpec @Coin (PV4.Lovelace . unCoin)
      sameAsToBuiltinDataSpec @(CompactForm Coin) (PV4.Lovelace . unCoin . fromCompact)
      sameAsToBuiltinDataSpec @CoinPerByte (PV4.Lovelace . unCoin . fromCompact . unCoinPerByte)
      sameAsToBuiltinDataSpec @(StrictMaybe Integer) strictMaybeToMaybe
      sameAsToBuiltinDataSpec @[Integer] id
      sameAsToBuiltinDataSpec @(Map Integer Integer) (PV4.unsafeFromList . Map.toList)
      sameAsToBuiltinDataSpec $ \ProtVer {pvMajor, pvMinor} -> PV4.ProtocolVersion (getVersion pvMajor) (toInteger pvMinor)
