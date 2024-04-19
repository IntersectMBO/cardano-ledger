{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Language
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word (Word8)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (genEraLanguage)
import Test.Cardano.Ledger.Common

spec :: forall era. (AlonzoEraPParams era, AlonzoEraScript era) => Spec
spec = do
  describe "CBOR deserialization" $ do
    validCostModelProp @era
    underspecifiedCostModelProp @era
    unknownCostModelProp @era
  prop "applyPPUpdates" $ \valid validUpdate unknown unknownUpdate -> do
    original <- mkCostModelsLenient (flattenCostModels valid <> unknown)
    originalUpdate <- mkCostModelsLenient (flattenCostModels validUpdate <> unknownUpdate)
    let
      pp = emptyPParams & ppCostModelsL .~ original
      ppUpdate = emptyPParamsUpdate & ppuCostModelsL .~ SJust originalUpdate
      updated = applyPPUpdates @era pp ppUpdate
    -- Starting with Conway we update CostModel on per-language basis, while before
    -- that CostModels where overwritten completely
    if eraProtVerLow @era >= natVersion @9
      then do
        expected <- mkCostModelsLenient (flattenCostModels originalUpdate <> flattenCostModels original)
        updated `shouldBe` (pp & ppCostModelsL .~ expected)
      else updated `shouldBe` (pp & ppCostModelsL .~ originalUpdate)

validCostModelProp ::
  forall era.
  (AlonzoEraPParams era, AlonzoEraScript era) =>
  Spec
validCostModelProp = do
  prop "valid CostModels deserialize correctly, both independently and within PParamsUpdate" $
    forAll (genEraLanguage @era) $ \(lang :: Language) -> do
      forAllShow (genValidCostModelEnc lang) (showEnc @era) $
        \validCmEnc -> do
          encodeAndCheckDecoded @era validCmEnc $
            \cmDecoded ppuDecoded -> do
              expectRight cmDecoded >>= (`shouldSatisfy` validCm)
              ppuRes <- expectRight ppuDecoded
              ppuRes `shouldSatisfy` \ppu -> (validCm <$> ppu ^. ppuCostModelsL) == SJust True
  where
    genValidCostModelEnc lang = genCostModelEncForLanguage lang (costModelParamsCount lang)
    validCm cms =
      not (null (costModelsValid cms)) && null (costModelsUnknown cms)

-- | Underspecified is a CostModel that has less than the normal number of parameters
underspecifiedCostModelProp ::
  forall era.
  AlonzoEraPParams era =>
  Spec
underspecifiedCostModelProp = do
  prop "CostModels with less than expected parameters within PParamsUpdate" $
    \(lang :: Language) -> do
      forAllShow (genUnderspecifiedCostModelEnc lang) (showEnc @era) $
        \shortCmEnc -> do
          encodeAndCheckDecoded @era shortCmEnc $
            \cmDecoded ppuDecoded -> do
              -- pre-Conway we are failing when deserializing underspecified costmodels
              if eraProtVerHigh @era < natVersion @9
                then do
                  expectDeserialiseFailure cmDecoded (Just "CostModels")
                  expectDeserialiseFailure ppuDecoded Nothing
                else do
                  -- post-Conway, we are retaining CostModels that specified less parameters than expected
                  cmRes <- expectRight cmDecoded
                  cmRes `shouldSatisfy` not . null . costModelsValid
  where
    genUnderspecifiedCostModelEnc lang = do
      let validCount = costModelParamsCount lang
      count <- choose (0, validCount - 1)
      genCostModelEncForLanguage lang count

unknownCostModelProp ::
  forall era.
  AlonzoEraPParams era =>
  Spec
unknownCostModelProp = do
  prop "unknown CostModels deserialize correctly within PParamsUpdate starting with Conway" $
    forAllShow genUnknownCostModelEnc (showEnc @era) $
      \unknownCmEnc -> do
        encodeAndCheckDecoded @era unknownCmEnc $
          \cmDecoded ppuDecoded -> do
            -- pre-Conway we are failing when deserializing unknown costmodels
            if eraProtVerHigh @era < natVersion @9
              then do
                expectDeserialiseFailure cmDecoded (Just "CostModels")
                expectDeserialiseFailure ppuDecoded Nothing
              else do
                -- post-Conway, we are collecting unknown CostModels
                cmRes <- expectRight cmDecoded
                cmRes `shouldSatisfy` unknownCm
                ppuRes <- expectRight ppuDecoded
                ppuRes `shouldSatisfy` \ppu -> (unknownCm <$> ppu ^. ppuCostModelsL) == SJust True
  where
    genUnknownCostModelEnc = do
      let firstUnknownLang = fromEnum (maxBound @Language) + 1
      lang <- choose (fromIntegral firstUnknownLang, maxBound @Word8)
      NonNegative count <- arbitrary
      genCostModelsEnc lang count
    unknownCm cms =
      null (costModelsValid cms) && not (null (costModelsUnknown cms))

encodeAndCheckDecoded ::
  forall era.
  AlonzoEraPParams era =>
  Encoding ->
  (Either DecoderError CostModels -> Either DecoderError (PParamsUpdate era) -> IO ()) ->
  IO ()
encodeAndCheckDecoded cmEnc check = do
  let ver = eraProtVerHigh @era
      ppuEnc = encCBOR (Map.singleton (18 :: Int) cmEnc)
      cmBytes = serialize ver cmEnc
      ppuBytes = serialize ver ppuEnc
  check
    (decodeFull @CostModels ver cmBytes)
    (decodeFull @(PParamsUpdate era) ver ppuBytes)

genCostModelsEnc :: Word8 -> Int -> Gen Encoding
genCostModelsEnc lang count = do
  values <- vectorOf count (arbitrary :: Gen Int)
  pure $ encCBOR $ Map.singleton lang values

genCostModelEncForLanguage :: Language -> Int -> Gen Encoding
genCostModelEncForLanguage = genCostModelsEnc . fromIntegral . fromEnum

expectDeserialiseFailure :: (HasCallStack, Show t) => Either DecoderError t -> Maybe Text -> IO ()
expectDeserialiseFailure e expectedTxt = do
  res <- expectLeft e
  res `shouldSatisfy` \case
    DecoderErrorDeserialiseFailure txt _ -> maybe True (== txt) expectedTxt
    _ -> False

showEnc :: forall era. Era era => Encoding -> String
showEnc enc = show $ toPlainEncoding (eraProtVerHigh @era) enc
