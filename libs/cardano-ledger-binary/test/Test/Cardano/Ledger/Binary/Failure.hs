{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Failure (spec) where

import Cardano.Ledger.Binary
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Test.Cardano.Ledger.Binary.RoundTrip (Trip (..), embedTripRangeFailureExpectation)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | Generate an association list with at least one duplicate key
genDuplicateAssocList :: Gen [(Int, Int)]
genDuplicateAssocList = do
  xs <- getNonEmpty <$> arbitrary
  (a, _) <- elements xs -- pick a key to duplicate
  c <- arbitrary -- pick a value for duplicate key
  shuffle ((a, c) : xs)

-- | Generate a CBOR encoded association list with at least one duplicate key
genDuplicateAssocListEncoding :: Gen Encoding
genDuplicateAssocListEncoding = do
  xs <- genDuplicateAssocList
  let flatXs = concat [[a, b] | (a, b) <- xs]
  oneof
    [ pure $ encodeMapLen (fromIntegral $ Prelude.length xs) <> foldMap encCBOR flatXs
    , pure $ encodeMapLenIndef <> foldMap encCBOR flatXs <> encodeBreak
    ]

-- | Generate a list with at least one duplicate
genDuplicateList :: Gen [Int]
genDuplicateList = do
  xs <- getNonEmpty <$> arbitrary
  a <- elements xs -- pick an element to duplicate
  shuffle (a : xs)

-- | Generate a CBOR encoded list with at least one duplicate, with and with the set tag
genDuplicateListEncoding :: Gen Encoding
genDuplicateListEncoding = do
  xs <- genDuplicateList
  let definite = encodeListLen (fromIntegral $ Prelude.length xs) <> foldMap encCBOR xs
      indefinite = encodeListLenIndef <> foldMap encCBOR xs <> encodeBreak
  elements
    [ definite
    , encodeTag 258 <> definite
    , indefinite
    , encodeTag 258 <> indefinite
    ]

-- | Starting in version 9, do not accept duplicates in CBOR maps
prop_shouldFailMapWithDupKeys :: Property
prop_shouldFailMapWithDupKeys =
  forAllBlind genDuplicateAssocListEncoding $
    \mapEncoding ->
      let trip = Trip id (decCBOR @(Map Int Int)) (dropCBOR (Proxy @(Map Int Int)))
       in property $ embedTripRangeFailureExpectation trip (natVersion @9) maxBound mapEncoding

-- | Starting in version 9, do not accept duplicates in CBOR sets
prop_shouldFailSetWithDupKeys :: Property
prop_shouldFailSetWithDupKeys =
  forAllBlind genDuplicateListEncoding $
    \setEncoding ->
      let trip = Trip id (decCBOR @(Set Int)) (dropCBOR (Proxy @(Set Int)))
       in property $ embedTripRangeFailureExpectation trip (natVersion @9) maxBound setEncoding

spec :: Spec
spec = do
  describe "Failures" $ do
    prop "map duplicates are not allowed starting v9" prop_shouldFailMapWithDupKeys
    prop "set duplicates are not allowed starting v9" prop_shouldFailSetWithDupKeys
