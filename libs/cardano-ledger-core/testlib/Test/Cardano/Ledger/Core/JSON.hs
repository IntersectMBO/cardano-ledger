{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.JSON (
  roundTripJsonSpec,
  roundTripJsonEraSpec,
  roundTripJsonProperty,
  goldenJsonPParamsSpec,
  goldenJsonPParamsUpdateSpec,
) where

import Cardano.Ledger.Core
import Data.Aeson (FromJSON, ToJSON, eitherDecode, eitherDecodeFileStrict, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Stack
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Era (EraTest)

-- | QuickCheck property spec that uses `roundTripJsonProperty`
roundTripJsonSpec ::
  forall t.
  (HasCallStack, Typeable t, Show t, Eq t, ToJSON t, FromJSON t, Arbitrary t) =>
  Spec
roundTripJsonSpec =
  prop (show (typeRep $ Proxy @t)) $ roundTripJsonProperty @t

-- | Roundtrip JSON testing for types that implement ToJSON/FromJSON.
roundTripJsonProperty ::
  forall t.
  (HasCallStack, Show t, Eq t, ToJSON t, FromJSON t) =>
  t ->
  Property
roundTripJsonProperty original = do
  let encoded = encode original
      encodedString =
        "Encoded: \n  " <> T.unpack (T.decodeUtf8 (BSL.toStrict (encodePretty original)))
  case eitherDecode encoded of
    Left err ->
      property False
        & counterexample ("Failed decoding: \n  " <> err <> "\n" <> encodedString)
    Right result ->
      property (result `shouldBe` original)
        & counterexample encodedString

-- | Roundtrip JSON testing for core type families.
roundTripJsonEraSpec ::
  forall era.
  ( HasCallStack
  , EraTest era
  ) =>
  Spec
roundTripJsonEraSpec =
  describe (eraName @era) $ do
    describe "RoundTrip JSON" $ do
      roundTripJsonSpec @(PParams era)
      roundTripJsonSpec @(TranslationContext era)

goldenJsonPParamsSpec ::
  forall era.
  EraPParams era =>
  SpecWith FilePath
goldenJsonPParamsSpec =
  it "Golden JSON specs for PParams " $
    eitherDecodeFileStrict @(PParams era) >=> expectRightDeepExpr_

goldenJsonPParamsUpdateSpec ::
  forall era.
  EraTest era =>
  SpecWith FilePath
goldenJsonPParamsUpdateSpec =
  it "Golden JSON specs for PParamsUpdate" $ \file -> do
    let ppu = runGen 100 100 (arbitrary @(PParamsUpdate era))
    let encoded = T.decodeUtf8 (BSL.toStrict (encodePretty ppu)) <> "\n"
    fileContent <- T.decodeUtf8 . BSL.toStrict <$> BSL.readFile file
    encoded `shouldBe` fileContent
