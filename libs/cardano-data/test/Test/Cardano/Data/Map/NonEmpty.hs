{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Data.Map.NonEmpty where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.NonEmpty as NE
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Data.JSON.Utils (roundTripJsonSpec)
import Test.Hspec

spec :: Spec
spec =
  describe "Map.NonEmpty" $ do
    context "JSON round-trip" $ do
      roundTripJsonSpec @(NE.NonEmptyMap Int Int)
    context "JSON parsing" $ do
      it "should reject empty JSON object" $ do
        let emptyJson = "{}" :: BSL.ByteString
        case eitherDecode emptyJson :: Either String (NE.NonEmptyMap Int Int) of
          Left err -> err `shouldContain` "Empty map found, expected non-empty"
          Right _ -> expectationFailure "Expected parsing to fail for empty map, but it succeeded"
