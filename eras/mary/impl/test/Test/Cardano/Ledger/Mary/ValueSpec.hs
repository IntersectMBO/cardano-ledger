{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ValueSpec (spec) where

import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Value
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.CanonicalMaps (canonicalInsert)
import GHC.Exts
import Test.Cardano.Data
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripCborExpectation,
  roundTripCborRangeExpectation,
  roundTripFailureCborExpectation,
  roundTripFailureCborRangeExpectation,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary (genEmptyMultiAsset, genMaryValue, genMultiAsset)

spec :: Spec
spec = do
  describe "MultiAsset" $ do
    prop "Canonical construction agrees" $ propCanonicalConstructionAgrees @StandardCrypto
  describe "CBOR roundtrip" $ do
    context "Coin" $ do
      prop "Non-negative Coin succeeds for all eras" $
        \(NonNegative i) -> roundTripCborExpectation (Coin i)
      prop "Negative Coin succeeds for pre-Conway" $
        \(Negative i) -> roundTripCborRangeExpectation minBound (natVersion @8) (Coin i)
      prop "Negative Coin fails to deserialise for Conway" $
        \(Negative i) -> roundTripFailureCborRangeExpectation (natVersion @9) (natVersion @9) (Coin i)
    context "MultiAsset" $ do
      prop "Non-zero-valued MultiAsset succeeds for all eras" $
        roundTripCborExpectation @(MultiAsset StandardCrypto)
      prop "zero-valued MultiAsset fails for pre-Conway, due to pruning, and for Conway, due to decoder implementation" $
        forAll (genMultiAsset @StandardCrypto (pure 0)) $
          roundTripFailureCborExpectation
      prop "Empty MultiAsset fails for Conway" $
        forAll (genEmptyMultiAsset @StandardCrypto) $
          roundTripFailureCborRangeExpectation (natVersion @9) (natVersion @9)
    context "MaryValue" $ do
      prop "Positive MaryValue succeeds for all eras" $
        forAll (genMaryValue (genMultiAsset @StandardCrypto (toInteger . getPositive @Int <$> arbitrary))) $
          roundTripCborExpectation
      prop "Negative MaryValue fails for all eras" $
        forAll (genMaryValue (genMultiAsset @StandardCrypto (toInteger . getNegative @Int <$> arbitrary))) $
          roundTripFailureCborExpectation
      prop "Zero MaryValue fails for pre-Conway, due to pruning, and for Conway, due to decoder implementation" $
        forAll (genMaryValue (genMultiAsset @StandardCrypto (pure 0))) $
          roundTripFailureCborExpectation
      prop "Empty MaryValue fails for Conway" $
        forAll (genMaryValue (genEmptyMultiAsset @StandardCrypto)) $
          roundTripFailureCborRangeExpectation (natVersion @9) (natVersion @9)

instance IsString AssetName where
  fromString = AssetName . either error SBS.toShort . BS16.decode . BS8.pack

propCanonicalConstructionAgrees ::
  Crypto c =>
  [(PolicyID c, AssetName, Integer)] ->
  [(PolicyID c, AssetName, Integer)] ->
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
