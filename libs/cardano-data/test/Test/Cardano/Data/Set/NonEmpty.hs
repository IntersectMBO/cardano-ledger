{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Data.Set.NonEmpty where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set.NonEmpty as NE
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Data.JSON.Utils (roundTripJsonSpec)
import Test.Hspec
import Prelude hiding (elem, filter, lookup, null)

spec :: Spec
spec =
  describe "Set.NonEmpty" $ do
    context "JSON round-trip" $ do
      roundTripJsonSpec @(NE.NonEmptySet Int)
    context "JSON parsing" $ do
      it "should reject empty JSON array" $ do
        let emptyJson = "[]" :: BSL.ByteString
        case eitherDecode emptyJson :: Either String (NE.NonEmptySet Int) of
          Left err -> err `shouldContain` "Empty set found, expected non-empty"
          Right _ -> expectationFailure "Expected parsing to fail for empty set, but it succeeded"
