{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DeserialiseFailure (..))
import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Cardano.Ledger.Plutus.Language
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE (toList)
import qualified Data.Map as Map
import Test.Cardano.Ledger.Alonzo.Arbitrary
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import Test.Cardano.Ledger.Common

spec ::
  forall era.
  ( EraPlutusContext era
  , Arbitrary (NativeScript era)
  ) =>
  Spec
spec = do
  describe "AlonzoTxWits deserialization" $ do
    describe "plutus scripts" $ do
      emptyFieldsProps @era
      plutusScriptsProp @era
      nativeScriptsProp @era

emptyFieldsProps ::
  forall era. AlonzoEraScript era => Spec
emptyFieldsProps = do
  prop "fails to deserialize if fields contain an empty collection" $
    conjoin $
      emptyFieldProp <$> [0 .. 7]
  where
    emptyFieldProp :: Int -> Property
    emptyFieldProp k =
      property $
        expectDeserialiseFailureFromVersion @era
          (natVersion @9)
          (emptyEnc k)
          "Empty list"
    emptyEnc k = encCBOR $ Map.singleton k (encCBOR ([] :: [Encoding]))

plutusScriptsProp ::
  forall era. EraPlutusContext era => Spec
plutusScriptsProp = do
  prop "fails to deserialize if empty or if it contains duplicates, starting with Conway" $
    conjoin $
      [ distinctProp
      , duplicateProp
      ]
        <*> NE.toList (supportedLanguages @era)
  where
    distinctProp lang =
      forAllShow (genEncoding lang False) (showEnc @era) $
        expectDeserialiseSuccess @era

    duplicateProp lang =
      forAllShow (genEncoding lang True) (showEnc @era) $
        \enc ->
          expectDeserialiseFailureFromVersion @era
            (natVersion @9)
            enc
            "Final number of elements"

    genEncoding :: SupportedLanguage era -> Bool -> Gen Encoding
    genEncoding supportedLanguage@(SupportedLanguage slang) duplicate = do
      plutusScript <- genPlutusScript supportedLanguage
      let plutusScripts
            | duplicate = [plutusScript, plutusScript]
            | otherwise = [plutusScript]
      pure $ encCBOR $ Map.singleton (keys slang) (encCBOR (plutusScriptBinary <$> plutusScripts))
    keys :: SLanguage l -> Int
    keys SPlutusV1 = 3
    keys SPlutusV2 = 6
    keys SPlutusV3 = 7
    keys SPlutusV4 = 8

nativeScriptsProp ::
  forall era.
  ( AlonzoEraScript era
  , Arbitrary (NativeScript era)
  ) =>
  Spec
nativeScriptsProp = do
  prop
    "fails to deserialize if empty, starting with Conway"
    distinctProp
  where
    distinctProp =
      forAllShow (genEncoding False) (showEnc @era) $
        expectDeserialiseSuccess @era

    -- TODO: enable this after we enforce distinct entries for Annotator
    -- duplicateProp =
    --   forAllShow (genEncoding True) (showEnc  @era) $
    --     \enc ->
    --       expectDeserialiseFailureFromVersion @era
    --       (natVersion @9)
    --       enc
    --       "Final number of elements"

    genEncoding :: Bool -> Gen Encoding
    genEncoding duplicate = do
      nativeScript <- genNativeScript @era
      let nativeScripts
            | duplicate = [nativeScript, nativeScript]
            | otherwise = [nativeScript]

      pure $ encCBOR $ Map.singleton (1 :: Int) (encCBOR nativeScripts)

expectDeserialiseSuccess ::
  forall era.
  (AlonzoEraScript era, HasCallStack) =>
  Encoding ->
  IO ()
expectDeserialiseSuccess enc =
  encodeAndCheckDecoded @era enc $
    \decoded -> void $ expectRight decoded

expectDeserialiseFailureFromVersion ::
  forall era.
  (AlonzoEraScript era, HasCallStack) =>
  Version ->
  Encoding ->
  String ->
  IO ()
expectDeserialiseFailureFromVersion v enc errMsgPrefix =
  encodeAndCheckDecoded @era enc $
    \decoded -> do
      if eraProtVerHigh @era < v
        then void $ expectRight decoded
        else expectDeserialiseFailure (void decoded) errMsgPrefix

expectDeserialiseFailure ::
  (HasCallStack, Show t) =>
  Either DecoderError t ->
  String ->
  IO ()
expectDeserialiseFailure e errMsgPrefix = do
  res <- expectLeft e
  res `shouldSatisfy` \case
    DecoderErrorDeserialiseFailure _ (DeserialiseFailure _ errMsg) ->
      errMsgPrefix `isPrefixOf` errMsg
    _ -> False

encodeAndCheckDecoded ::
  forall era.
  AlonzoEraScript era =>
  Encoding ->
  (Either DecoderError (Annotator (AlonzoTxWits era)) -> IO ()) ->
  IO ()
encodeAndCheckDecoded enc check = do
  let ver = eraProtVerHigh @era
      bytes = serialize ver enc
  check (decodeFull @(Annotator (AlonzoTxWits era)) ver bytes)

showEnc :: forall era. Era era => Encoding -> String
showEnc enc = show $ toPlainEncoding (eraProtVerHigh @era) enc
