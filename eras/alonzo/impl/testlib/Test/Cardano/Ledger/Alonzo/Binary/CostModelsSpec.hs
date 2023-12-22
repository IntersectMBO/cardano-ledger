{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (ToCBOR (..))
import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Language
import qualified Data.Map as Map
import Data.Proxy
import Data.Text (Text)
import Data.Word (Word8)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Common

spec :: forall era. AlonzoEraPParams era => Spec
spec = do
  describe "CBOR deserialization" $ do
    validCostModelProp @era Proxy
    invalidCostModelProp @era Proxy
    unknownCostModelProp @era Proxy
  prop "applyPPUpdates" $ \valid validUpdate unknown unknownUpdate -> do
    let
      validExpected = mkCostModels (costModelsValid validUpdate <> costModelsValid valid)
      unknownExpected = unknownUpdate <> unknown
      original = mkCostModelsLenient (flattenCostModels valid <> unknown)
      update = mkCostModelsLenient (flattenCostModels validUpdate <> unknownUpdate)
      expected = mkCostModelsLenient (flattenCostModels validExpected <> unknownExpected)
      pp =
        emptyPParams & ppCostModelsL .~ original
      ppUpdate =
        emptyPParamsUpdate & ppuCostModelsL .~ SJust update
    -- Starting with Conway we update CostModel on per-language basis, while before
    -- that CostModels where overwritten completely
    applyPPUpdates @era pp ppUpdate
      `shouldBe` if eraProtVerLow @era >= natVersion @9
        then pp & ppCostModelsL .~ expected
        else pp & ppCostModelsL .~ update

validCostModelProp ::
  forall era.
  AlonzoEraPParams era =>
  Proxy era ->
  Spec
validCostModelProp pxy = do
  prop "valid CostModels deserialize correctly, both independently and within PParamsUpdate" $
    \(lang :: Language) -> do
      forAllShow (genValidCostModelEnc lang) (showEnc pxy) $
        \validCmEnc -> do
          encodeAndCheckDecoded @era validCmEnc $
            \cmDecoded ppuDecoded -> do
              expectRight cmDecoded >>= (`shouldSatisfy` validCm)
              ppuRes <- expectRight ppuDecoded
              ppuRes `shouldSatisfy` \ppu -> (validCm <$> ppu ^. ppuCostModelsL) == SJust True
  where
    genValidCostModelEnc lang = genCostModelEncForLanguage lang (costModelParamsCount lang)
    validCm cms =
      not (null (costModelsValid cms)) && null (costModelsErrors cms) && null (costModelsUnknown cms)

invalidCostModelProp ::
  forall era.
  AlonzoEraPParams era =>
  Proxy era ->
  Spec
invalidCostModelProp pxy = do
  prop "invalid CostModels fail within PParamsUpdate" $
    \(lang :: Language) -> do
      forAllShow (genInvalidCostModelEnc lang) (showEnc pxy) $
        \invalidCmEnc -> do
          encodeAndCheckDecoded @era invalidCmEnc $
            \cmDecoded ppuDecoded -> do
              -- pre-Conway we are failing when deserializing invalid costmodels
              if eraProtVerHigh @era < natVersion @9
                then expectDeserialiseFailure cmDecoded (Just "CostModels")
                else do
                  -- post-Conway, we are collecting CostModels deserialization errors
                  cmRes <- expectRight cmDecoded
                  cmRes `shouldSatisfy` invalidCm

              -- in no era are we deserializing invalid costmodels within PParamsUpdate
              expectDeserialiseFailure ppuDecoded Nothing
  where
    genInvalidCostModelEnc lang = do
      let validCount = costModelParamsCount lang
      count <- choose (0, validCount - 1)
      genCostModelEncForLanguage lang count

    invalidCm cms =
      null (costModelsValid cms) && not (null (costModelsErrors cms))

unknownCostModelProp ::
  forall era.
  AlonzoEraPParams era =>
  Proxy era ->
  Spec
unknownCostModelProp pxy = do
  prop "unknown CostModels deserialize correctly within PParamsUpdate starting with Conway" $
    forAllShow genUnknownCostModelEnc (showEnc pxy) $
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
      null (costModelsValid cms) && null (costModelsErrors cms) && not (null (costModelsUnknown cms))

encodeAndCheckDecoded ::
  forall era.
  AlonzoEraPParams era =>
  Encoding ->
  (Either DecoderError CostModels -> Either DecoderError (PParamsUpdate era) -> IO ()) ->
  IO ()
encodeAndCheckDecoded cmEnc check = do
  let ver = eraProtVerHigh @era
      ppuEnc =
        fromPlainEncoding
          . toCBOR
          . Map.singleton (18 :: Int)
          . toPlainEncoding ver
          $ cmEnc
      cmBytes = serialize ver cmEnc
      ppuBytes = serialize ver ppuEnc
  check
    (decodeFull @CostModels ver cmBytes)
    (decodeFull @(PParamsUpdate era) ver ppuBytes)

genCostModelsEnc :: Word8 -> Int -> Gen Encoding
genCostModelsEnc lang count = do
  values <- vectorOf count (arbitrary :: Gen Int)
  pure $ fromPlainEncoding . toCBOR $ Map.singleton lang values

genCostModelEncForLanguage :: Language -> Int -> Gen Encoding
genCostModelEncForLanguage lang =
  genCostModelsEnc (fromIntegral (fromEnum lang))

expectDeserialiseFailure :: (HasCallStack, Show t) => Either DecoderError t -> Maybe Text -> IO ()
expectDeserialiseFailure e expectedTxt = do
  res <- expectLeft e
  res `shouldSatisfy` \case
    DecoderErrorDeserialiseFailure txt _ -> maybe True (== txt) expectedTxt
    _ -> True

showEnc :: forall era. Era era => Proxy era -> Encoding -> String
showEnc _ enc = show $ toPlainEncoding (eraProtVerHigh @era) enc
