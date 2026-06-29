{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ValueSpec (spec) where

import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Binary (DecoderError, decodeFull, serialize)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
import Control.Exception (AssertionFailed (AssertionFailed), evaluate)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.CanonicalMaps (canonicalInsert)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Exts
import Test.Cardano.Data
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripCborExpectation,
  roundTripCborFailureExpectation,
  roundTripCborRangeExpectation,
  roundTripCborRangeFailureExpectation,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary (
  genMaryValue,
  genMultiAsset,
  genMultiAssetCompletelyEmpty,
  genMultiAssetNestedEmpty,
  genMultiAssetToFail,
  genMultiAssetZero,
  genNegativeInt,
 )

spec :: Spec
spec = do
  describe "MultiAsset" $ do
    prop "Canonical construction agrees" $
      withMaxSuccess 10000 propCanonicalConstructionAgrees
  describe "CBOR roundtrip" $ do
    describe "Coin" $ do
      prop "Non-negative Coin succeeds for all eras" $
        \(NonNegative i) -> roundTripCborExpectation (Coin i)
      prop "Negative Coin fails to deserialise for all eras" $
        \(Negative i) -> roundTripCborRangeFailureExpectation (natVersion @0) maxBound (Coin i)
    describe "MultiAsset" $ do
      prop "Non-zero-valued MultiAsset succeeds for all eras" $
        roundTripCborExpectation @MultiAsset
      prop "Zero-valued MultiAsset fails for Conway" $
        forAll genMultiAssetZero $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "MultiAsset with empty nested asset maps fails for Conway and Dijkstra" $
        forAll genMultiAssetNestedEmpty $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "Completely empty MultiAsset succeeds for Conway (but fails for Dijkstra)" $
        forAll genMultiAssetCompletelyEmpty $
          roundTripCborRangeExpectation (natVersion @4) (natVersion @11)
      prop "Completely empty MultiAsset fails for Dijkstra" $
        forAll genMultiAssetCompletelyEmpty $
          roundTripCborRangeFailureExpectation (natVersion @12) maxBound
    describe "MaryValue" $ do
      prop "Positive MaryValue succeeds for all eras" $ \(mv :: MaryValue) ->
        roundTripCborExpectation mv
      prop "Negative MaryValue fails for all eras" $
        forAll
          (genMaryValue (genMultiAsset (toInteger <$> genNegativeInt)))
          roundTripCborFailureExpectation
      prop "Zero MaryValue fails Conway onwards" $
        forAll (genMaryValue genMultiAssetZero) $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "MaryValue with empty nested asset maps fails Conway onwards" $
        forAll (genMaryValue genMultiAssetNestedEmpty) $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "MaryValue with completely empty MultiAsset succeeds for Conway" $
        forAll (genMaryValue genMultiAssetCompletelyEmpty) $
          roundTripCborRangeExpectation (natVersion @4) (natVersion @11)
      prop "MaryValue with completely empty MultiAsset fails for Dijkstra" $ \(Positive c) ->
        forM_ [natVersion @12 .. maxBound] $ \version -> do
          let serialized :: BSL.ByteString
              serialized = serialize @(Int, Map.Map () ()) version (c, Map.empty)
          case decodeFull version serialized :: Either DecoderError MaryValue of
            Left _ -> pure ()
            Right (m :: MaryValue) ->
              expectationFailure $
                mconcat
                  [ "Should not have deserialized: <version: "
                  , show version
                  , "> "
                  , show m
                  ]
      -- Test pre-Conway behavior (should allow everything)
      prop "All MultiAsset types succeed for pre-Conway eras" $
        forAll (oneof [genMultiAssetCompletelyEmpty, genMultiAssetZero, genMultiAssetNestedEmpty]) $ \ma -> do
          forM_ [natVersion @4 .. natVersion @8] $ \version -> do
            let serialized :: BSL.ByteString
                serialized = serialize @MultiAsset version ma
            case decodeFull version serialized :: Either DecoderError MultiAsset of
              Right _ -> pure ()
              Left _ ->
                expectationFailure $
                  mconcat
                    [ "Should have deserialized successfully: <version: "
                    , show version
                    , "> "
                    , show ma
                    ]
      it "Too many assets should fail" $
        property $
          forAll
            (genMaryValue (genMultiAssetToFail True))
            ( roundTripCborRangeFailureExpectation @MaryValue
                (eraProtVerLow @MaryEra)
                maxBound
            )
  describe "MaryValue compacting" $ do
    prop "Canonical generator" $
      \(ma :: MaryValue) ->
        fromCompact (fromJust (toCompact ma)) `shouldBe` ma
    prop "Failing generator" $
      forAll (genMaryValue (genMultiAssetToFail True)) $
        \ma ->
          evaluate (fromCompact (fromJust (toCompact ma)))
            `shouldThrow` (\(AssertionFailed errorMsg) -> take 16 errorMsg == "Assertion failed")

instance IsString AssetName where
  fromString = AssetName . either error SBS.toShort . BS16.decode . BS8.pack

propCanonicalConstructionAgrees ::
  [(PolicyID, AssetName, Integer)] ->
  [(PolicyID, AssetName, Integer)] ->
  Property
propCanonicalConstructionAgrees xs ys = property $ do
  let ma1@(MultiAsset a1) = multiAssetFromList xs
      ma2@(MultiAsset a2) = multiAssetFromList ys
  expectValidMap a1
  expectValidMap a2
  let mb1@(MultiAsset b1) =
        mconcat
          [ MultiAsset $
              canonicalInsert const pid (canonicalInsert const an i mempty) mempty
          | (pid, an, i) <- xs
          ]
      mb2@(MultiAsset b2) =
        mconcat
          [ MultiAsset $
              canonicalInsert const pid (canonicalInsert const an i mempty) mempty
          | (pid, an, i) <- ys
          ]
  expectValidMap b1
  expectValidMap b2
  ma1 `shouldBe` mb1
  ma2 `shouldBe` mb2
  ma1 <> ma2 `shouldBe` mb1 <> mb2
  ma1 <> mb2 `shouldBe` mb1 <> ma2
