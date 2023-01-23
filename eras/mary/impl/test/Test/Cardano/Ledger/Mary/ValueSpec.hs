{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ValueSpec (spec) where

import Cardano.Crypto.Hash.Class -- (hashWith, castHash)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Hashes
import Cardano.Ledger.Mary.Value
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.CanonicalMaps (canonicalInsert)
import GHC.Exts
import Test.Cardano.Data
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary

spec :: Spec
spec = do
  describe "MultiAsset" $ do
    prop "Canonical construction agrees" $ propCanonicalConstructionAgrees @StandardCrypto

instance IsString AssetName where
  fromString = AssetName . either error SBS.toShort . BS16.decode . BS8.pack

instance Arbitrary AssetName where
  arbitrary =
    AssetName
      <$> oneof
        [ do
            len <- choose (1, 32)
            genShortByteString len
        , -- We need duplicates for quality tests
          elements digitByteStrings
        ]

digitByteStrings :: IsString s => [s]
digitByteStrings = [fromString [x] | x <- ['0' .. '9']]

hashOfDigitByteStrings :: HashAlgorithm h => [Hash h a]
hashOfDigitByteStrings = castHash . hashWith id <$> digitByteStrings

instance Crypto c => Arbitrary (PolicyID c) where
  arbitrary =
    PolicyID . ScriptHash
      <$> oneof
        [ arbitrary
        , elements hashOfDigitByteStrings
        ]

instance Crypto c => Arbitrary (MultiAsset c) where
  arbitrary =
    genMultiAsset $
      toInteger
        <$> oneof
          [ choose (1 :: Int, maxBound)
          , choose (minBound :: Int, -1)
          ]

instance Crypto c => Arbitrary (MaryValue c) where
  arbitrary = do
    v <- toInteger <$> choose (0 :: Int, maxBound)
    ma <-
      genMultiAsset $ toInteger <$> choose (1 :: Int, maxBound)
    pure $ MaryValue v ma

genMultiAsset :: Crypto c => Gen Integer -> Gen (MultiAsset c)
genMultiAsset genAmount =
  MultiAsset <$> genNonEmptyMap arbitrary (genNonEmptyMap arbitrary genAmount)

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
