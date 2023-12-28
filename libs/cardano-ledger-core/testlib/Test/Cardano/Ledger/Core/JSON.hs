{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.JSON (
  roundTripJsonSpec,
  roundTripJsonEraSpec,
  roundTripJsonProperty,
) where

import Cardano.Ledger.Core
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Stack
import Test.Cardano.Ledger.Common

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
  , EraPParams era
  , Arbitrary (PParams era)
  ) =>
  Spec
roundTripJsonEraSpec =
  describe (eraName @era) $ do
    describe "RoundTrip JSON" $ do
      roundTripJsonSpec @(PParams era)
