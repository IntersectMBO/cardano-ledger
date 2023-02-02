{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Failure (spec) where

import Cardano.Ledger.Binary
import Data.Either (isLeft)
import Data.Map (Map)
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
    [ pure $ encodeMapLen (fromIntegral $ Prelude.length xs) <> foldMap toCBOR flatXs
    , pure $ encodeMapLenIndef <> foldMap toCBOR flatXs <> encodeBreak
    ]

-- | Starting in version 9, do not accept duplicates in CBOR maps
prop_shouldFailMapWithDupKeys :: Property
prop_shouldFailMapWithDupKeys =
  forAllBlind genDuplicateAssocListEncoding $
    ( \encodedMap ->
        (property . isLeft) (decode (natVersion @9) encodedMap :: Either DecoderError (Map Int Int))
    )

decode :: FromCBOR a => Version -> Encoding -> Either DecoderError a
decode version enc =
  let encoded = serializeEncoding version enc
   in decodeFull version encoded

spec :: Spec
spec = do
  describe "Failures" $ do
    prop "map duplicates are not allowed starting v9" prop_shouldFailMapWithDupKeys
