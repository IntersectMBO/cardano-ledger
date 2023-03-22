{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Arbitrary (
  genEmptyMultiAsset,
  genMaryValue,
  genMultiAsset,
  genMultiAssetToFail,
) where

import Cardano.Crypto.Hash.Class (Hash, HashAlgorithm, castHash, hashWith)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import Data.Int (Int64)
import qualified Data.Map.Strict as Map (empty)
import Data.Maybe.Strict (StrictMaybe)
import Data.String (IsString (fromString))
import Test.Cardano.Data (genNonEmptyMap)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Binary.Arbitrary (genShortByteString)
import Test.QuickCheck (
  Arbitrary,
  Gen,
  arbitrary,
  choose,
  elements,
  frequency,
  listOf,
  oneof,
  resize,
  vectorOf,
 )

instance Arbitrary AssetName where
  arbitrary =
    AssetName
      <$> frequency
        [ (1, elements digitByteStrings)
        , (99, genShortByteString =<< choose (1, 32))
        ]

instance
  ( EraTxOut era
  , Era era
  , Arbitrary (TxOut era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (MaryTxBody era)
  where
  arbitrary =
    MaryTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | Variant on @multiAssetFromList@ that makes sure that generated values stay
-- bounded within the range of a given integral type.
multiAssetFromListBounded ::
  forall i c.
  (Bounded i, Integral i) =>
  [(PolicyID c, AssetName, i)] ->
  MultiAsset c
multiAssetFromListBounded =
  foldr
    (\(p, n, fromIntegral -> i) ans -> ConcreteValue.insertMultiAsset comb p n i ans)
    mempty
  where
    comb :: Integer -> Integer -> Integer
    comb a b =
      max
        (fromIntegral $ minBound @i)
        (min (fromIntegral $ maxBound @i) (a + b))

instance Crypto c => Arbitrary (PolicyID c) where
  arbitrary =
    PolicyID . ScriptHash
      <$> oneof
        [ arbitrary
        , elements hashOfDigitByteStrings
        ]

genMultiAssetTriple :: Crypto c => Gen Int64 -> Gen (PolicyID c, AssetName, Int64)
genMultiAssetTriple genAmount = (,,) <$> arbitrary <*> arbitrary <*> genAmount

-- | The number 910 is chosen with the following rationale.
--
-- When we generate a number of MultiAssets all at once, that number happens to have
-- an implicit upper limit due to the Cardano.Ledger.Mary.Value.{to,from}-based
-- compacting operation. This operation is also performed when we serialise to and from CBOR.
--
-- Refering to the haddock for 'Cardano.Ledger.Mary.Value.to' we surmise that
--   1. The offsets for AssetName and PolicyID are stored as Word16 (maxBound = 65535).
--   2. All offsets (including those for AssetName and PolicyID) are relative to the whole
--      of the representation (binary blob) rather than the start of their respective regions.
--   3. If the offsets exceed their maxBounds, they will overflow.
--   4. So, we need to ensure that at least the last of the offsets (AssetName offsets) do
--      not exceed 65535.
--   5. With `n` as the total number of assets, the inequality to be satisfied is thus:
--           8n -- Word64 asset quantities
--        +  2n -- Word16 policy id offsets
--        +  2n -- Word16 asset name offsets
--        + 28n -- 28-byte policy ids
--        + 32n -- 32-byte asset names (a maximum of 32 bytes)
--        should be <= 65535
--        65535 / 72 ~ 910.2 is the maximum number of triples to be safely generated
--
-- NOTE: There are some conditions due to which exceeding this number may not
-- result in a guaranteed failure to compact without overflow, because, during compacting
--   1. The asset names and policy ids are deduplicated
--   2. Not all generated asset names are 32-bytes long
-- But, exceeding this number does make the probability of causing overflow > 0.
genMultiAsset :: Crypto c => Gen Integer -> Gen (MultiAsset c)
genMultiAsset genAmount =
  multiAssetFromListBounded
    <$> resize 910 (listOf $ genMultiAssetTriple $ fromIntegral <$> genAmount)

-- For negative tests
genMultiAssetToFail :: Crypto c => Int -> Gen (MultiAsset c)
genMultiAssetToFail triplesSize =
  multiAssetFromListBounded @Int64
    <$> vectorOf
      triplesSize
      ( (,,)
          <$> arbitrary
          <*> (AssetName <$> genShortByteString 32)
          <*> (fromIntegral <$> choose (1 :: Int, maxBound))
      )

instance Crypto c => Arbitrary (MultiAsset c) where
  arbitrary =
    genMultiAsset $
      toInteger
        <$> oneof
          [ choose (1 :: Int, maxBound)
          , choose (minBound :: Int, -1)
          ]

genEmptyMultiAsset :: Crypto c => Gen (MultiAsset c)
genEmptyMultiAsset =
  MultiAsset <$> genNonEmptyMap arbitrary (pure Map.empty)

genMaryValue :: Gen (MultiAsset c) -> Gen (MaryValue c)
genMaryValue genMA = do
  i <- toInteger <$> choose (0 :: Int, maxBound)
  ma <- genMA
  pure $ MaryValue i ma

instance Crypto c => Arbitrary (MaryValue c) where
  arbitrary = do
    v <- toInteger <$> choose (0 :: Int, maxBound)
    ma <-
      genMultiAsset $ toInteger <$> choose (1 :: Int, maxBound)
    pure $ MaryValue v ma

digitByteStrings :: IsString s => [s]
digitByteStrings = [fromString [x] | x <- ['0' .. '9']]

hashOfDigitByteStrings :: HashAlgorithm h => [Hash h a]
hashOfDigitByteStrings = castHash . hashWith id <$> digitByteStrings
