{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Data.JSON.Utils (
  roundTripJsonSpec,
  roundTripJsonProperty,
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable, typeRep)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary, Property, counterexample, property)

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
        "Encoded: \n  " <> T.unpack (T.decodeUtf8 (BSL.toStrict encoded))
  case eitherDecode encoded of
    Left err ->
      property False
        & counterexample ("Failed decoding: \n  " <> err <> "\n" <> encodedString)
    Right result ->
      property (result `shouldBe` original)
        & counterexample encodedString
