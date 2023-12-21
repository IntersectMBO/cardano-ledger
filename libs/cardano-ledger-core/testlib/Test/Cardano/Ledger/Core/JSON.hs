{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.JSON (
  roundTripEraSpec,
  roundTripProperty,
) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Function ((&))
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Test.Cardano.Ledger.Common (Arbitrary, Property, Spec, counterexample, prop, property, (===))

-- | QuickCheck property spec that uses `roundTripProperty`
roundTripEraSpec ::
  forall t.
  (Typeable t, Show t, Eq t, ToJSON t, FromJSON t, Arbitrary t) =>
  Spec
roundTripEraSpec =
  prop (show (typeRep $ Proxy @t)) $ roundTripProperty @t

-- | Roundtrip JSON testing for types and type families that implement
-- ToJSON/FromJSON. Requires TypeApplication of an @@era@
roundTripProperty ::
  forall t.
  (Show t, Eq t, ToJSON t, FromJSON t) =>
  t ->
  Property
roundTripProperty original = do
  let encoded = encode original
  case eitherDecode encoded of
    Left err ->
      property False
        & counterexample ("failed: " <> err)
    Right result ->
      result === original
        & counterexample ("after:  " <> show result)
        & counterexample ("before: " <> show original)
