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
  genMultiAssetZero,
) where

import Cardano.Crypto.Hash.Class (Hash, HashAlgorithm, castHash, hashWith)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
  isMultiAssetSmallEnough,
 )
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import Data.Int (Int64)
import qualified Data.Map.Strict as Map (empty)
import Data.Maybe.Strict (StrictMaybe)
import Data.String (IsString (fromString))
import Test.Cardano.Data (genNonEmptyMap)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Binary.Arbitrary (genByteString, genShortByteString)
import Test.QuickCheck (
  Arbitrary,
  Gen,
  NonZero (getNonZero),
  Positive (getPositive),
  arbitrary,
  choose,
  chooseInt,
  elements,
  frequency,
  listOf1,
  oneof,
  scale,
  vectorOf,
 )

instance Arbitrary AssetName where
  arbitrary =
    AssetName
      <$> frequency
        [ (3, elements digitByteStrings)
        , (7, genShortByteString =<< choose (1, 32))
        ]

instance
  ( EraTxOut era
  , EraDCert era
  , Era era
  , Arbitrary (TxOut era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (DCert era)
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
--   5. With `n` as the total number of assets, `p` the number of policy ids, the inequality to be satisfied is thus:
--           8n -- Word64 asset quantities
--        +  2n -- Word16 policy id offsets
--        +  2n -- Word16 asset name offsets
--        + 28p -- 28-byte policy ids
--        + 32n -- 32-byte asset names (a maximum of 32 bytes)
--        should be <= 65535, assuming the numer of policies to be maximal (i.e. equal to number of assets)
--        65535 / 72 ~ 910.2 is the maximum number of triples to be safely generated.
--        Or, in other words, 44n + 28p <= 65535
--
-- NOTE: There are some conditions due to which exceeding this number may not
-- result in a guaranteed failure to compact without overflow, because, during compacting
--   1. The asset names and policy ids are deduplicated
--   2. Not all generated asset names are 32-bytes long
-- But, exceeding this number does make the probability of causing overflow > 0.
genMultiAsset :: forall c. Crypto c => Gen Integer -> Gen (MultiAsset c)
genMultiAsset genAmount = do
  ma <-
    oneof
      [ MultiAsset <$> genNonEmptyMap arbitrary (genNonEmptyMap arbitrary genAmount)
      , multiAssetFromListBounded <$> listOf1 (genMultiAssetTriple $ fromIntegral <$> genAmount)
      ]
  if isMultiAssetSmallEnough ma
    then pure ma
    else scale (`div` 2) $ genMultiAsset genAmount

-- | For tests, because `insertMultiAsset` called through `genMultiAsset` filters out zero values
genMultiAssetZero :: Crypto c => Gen (MultiAsset c)
genMultiAssetZero = MultiAsset <$> genNonEmptyMap arbitrary (genNonEmptyMap arbitrary (pure 0))

-- For negative tests, we need a definite generator that will produce just large-enough MultiAssets
-- that will fail decoding, but not large-enough to consume too much resource.
-- The first Bool argument indicates whether the generation is for use in a MaryValue (MaryValue MultiAssets have Positive values)
genMultiAssetToFail :: forall c. Crypto c => Bool -> Gen (MultiAsset c)
genMultiAssetToFail isForMaryValue = do
  let genAssetNameToFail = AssetName <$> genShortByteString 32
      genPolicyIDToFail = PolicyID . ScriptHash . castHash . hashWith id <$> genByteString 28
  (numP, numA) <- do
    -- When numAssetNames > 1489 it is enough to have at least 1 policy id
    numAssetNames <- chooseInt (1, 1500)
    -- Make at least 1 policy id
    let minNumPolicyIds = max 0 ((65535 - 44 * numAssetNames) `div` 28)
    numPolicyIds <- (minNumPolicyIds +) . getPositive <$> arbitrary
    -- Ensure we have at least as many asset names as there are policy ids
    pure (numPolicyIds, max numPolicyIds numAssetNames)

  -- Here we generate separately a list of asset names and a list of policy ids and
  -- randomly shuffle them into a MultiAsset, ensuring that each policy has at least one asset.
  ps <- vectorOf numP genPolicyIDToFail
  as <-
    vectorOf numA $
      (,)
        <$> genAssetNameToFail
        <*> if isForMaryValue then getPositive @Int <$> arbitrary else getNonZero @Int <$> arbitrary
  let initialTriples = zipWith (\p (a, v) -> (p, a, v)) ps as -- All policies should have at least one asset
      remainingAs = drop numP as
  remainingTriples <-
    traverse
      ( \(a, v) -> do
          policy <- elements ps -- For every remaining asset, randomly assign a policy
          pure (policy, a, v)
      )
      remainingAs
  let MultiAsset ma = multiAssetFromListBounded $ initialTriples <> remainingTriples
  -- Ensure that the large numbers aren't reduced due to duplicates.
  -- This is impossible in practice, since PRNG produces uniform values.
  if length ma == numP && sum (length <$> ma) == numA
    then pure $ MultiAsset ma
    else genMultiAssetToFail isForMaryValue

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
