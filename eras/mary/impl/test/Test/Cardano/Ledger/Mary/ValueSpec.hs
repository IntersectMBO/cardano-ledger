{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ValueSpec (spec) where

import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
import Control.Exception (AssertionFailed (AssertionFailed), evaluate)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.CanonicalMaps (canonicalInsert)
import Data.Maybe (fromJust)
import GHC.Exts
import Test.Cardano.Data
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripCborExpectation,
  roundTripCborFailureExpectation,
  roundTripCborRangeFailureExpectation,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary (
  genEmptyMultiAsset,
  genMaryValue,
  genMultiAsset,
  genMultiAssetToFail,
  genMultiAssetZero,
  genNegativeInt,
 )

spec :: Spec
spec = do
  describe "MultiAsset" $ do
    prop "Canonical construction agrees" $
      withMaxSuccess 10000 $
        propCanonicalConstructionAgrees
  describe "CBOR roundtrip" $ do
    context "Coin" $ do
      prop "Non-negative Coin succeeds for all eras" $
        \(NonNegative i) -> roundTripCborExpectation (Coin i)
      prop "Negative Coin fails to deserialise for all eras" $
        \(Negative i) -> roundTripCborRangeFailureExpectation (natVersion @0) maxBound (Coin i)
    context "MultiAsset" $ do
      prop "Non-zero-valued MultiAsset succeeds for all eras" $
        roundTripCborExpectation @MultiAsset
      prop "Zero-valued MultiAsset fails for Conway" $
        forAll genMultiAssetZero $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "Empty MultiAsset fails for Conway" $
        forAll genEmptyMultiAsset $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
    context "MaryValue" $ do
      prop "Positive MaryValue succeeds for all eras" $ \(mv :: MaryValue) ->
        roundTripCborExpectation mv
      prop "Negative MaryValue fails for all eras" $
        forAll
          (genMaryValue (genMultiAsset (toInteger <$> genNegativeInt)))
          roundTripCborFailureExpectation
      prop "Zero MaryValue fails for Conway" $
        forAll (genMaryValue genMultiAssetZero) $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "Empty MaryValue fails for Conway" $
        forAll (genMaryValue genEmptyMultiAsset) $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
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
