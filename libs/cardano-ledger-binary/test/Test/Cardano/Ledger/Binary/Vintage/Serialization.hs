{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Ledger.Binary.Vintage.Serialization (tests) where

import Cardano.Ledger.Binary hiding (Range)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Short as BS.Short
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Gen.QuickCheck (arbitrary)
import qualified Hedgehog.Range as Range
import Test.Cardano.Ledger.Binary.Arbitrary ()

{- HLINT ignore "Redundant <$>" -}

tests :: IO Bool
tests = checkParallel $$(discover)

data TestStruct = TestStruct
  { tsUnit :: ()
  , tsBool :: !Bool
  , tsInteger :: !Integer
  , tsWord :: !Word
  , tsWord8 :: !Word8
  , tsWord16 :: !Word16
  , tsWord32 :: !Word32
  , tsWord64 :: !Word64
  , tsInt :: !Int
  , tsFloat :: !Float
  , tsInt32 :: !Int32
  , tsInt64 :: !Int64
  , tsTupleBoolBool :: !(Bool, Bool)
  , tsTupleBoolBoolBool :: !(Bool, Bool, Bool)
  , tsTupleBoolBoolBoolBool :: !(Bool, Bool, Bool, Bool)
  , tsByteString :: !BS.ByteString
  , tsText :: !Text
  , tsListBool :: ![Bool]
  , tsEitherBoolBool :: !(Either Bool Bool)
  , tsNonEmptyBool :: !(NonEmpty Bool)
  , tsMaybeBool :: !(Maybe Bool)
  , tsMapBoolBool :: !(Map Bool Bool)
  , tsSetBool :: !(Set Bool)
  , tsVectorBool :: !(V.Vector Bool)
  , tsLByteString :: BS.Lazy.ByteString
  , tsSByteString :: BS.Short.ShortByteString
  , tsUTCTime :: Time.UTCTime
  }
  deriving (Show, Eq)

genTestStruct :: Gen TestStruct
genTestStruct =
  TestStruct
    <$> pure ()
    <*> Gen.bool
    <*> Gen.integral (Range.linearFrom 0 (-1e40) 1e40 :: Range Integer)
    <*> Gen.word Range.constantBounded
    <*> Gen.word8 Range.constantBounded
    <*> Gen.word16 Range.constantBounded
    <*> Gen.word32 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.int Range.constantBounded
    <*> Gen.float (Range.constant (-1e12) 1e12)
    <*> Gen.int32 Range.constantBounded
    <*> Gen.int64 Range.constantBounded
    <*> ((,) <$> Gen.bool <*> Gen.bool)
    <*> ((,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool)
    <*> ((,,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool)
    <*> Gen.bytes (Range.linear 0 20)
    <*> Gen.text (Range.linear 0 20) Gen.unicode
    <*> Gen.list (Range.constant 0 10) Gen.bool
    <*> Gen.choice [Right <$> Gen.bool, Left <$> Gen.bool]
    <*> Gen.nonEmpty (Range.linear 1 20) Gen.bool
    <*> Gen.maybe Gen.bool
    <*> Gen.map (Range.constant 0 2) ((,) <$> Gen.bool <*> Gen.bool)
    <*> Gen.set (Range.constant 0 2) Gen.bool
    <*> (V.fromList <$> Gen.list (Range.constant 0 10) Gen.bool)
    <*> (BS.Lazy.fromStrict <$> Gen.bytes (Range.linear 0 20))
    <*> (BS.Short.toShort <$> Gen.bytes (Range.linear 0 20))
    <*> arbitrary

instance EncCBOR TestStruct where
  encCBOR ts =
    encodeListLen 1
      <> encCBOR (tsUnit ts)
      <> encCBOR (tsBool ts)
      <> encCBOR (tsInteger ts)
      <> encCBOR (tsWord ts)
      <> encCBOR (tsWord8 ts)
      <> encCBOR (tsWord16 ts)
      <> encCBOR (tsWord32 ts)
      <> encCBOR (tsWord64 ts)
      <> encCBOR (tsInt ts)
      <> encCBOR (tsFloat ts)
      <> encCBOR (tsInt32 ts)
      <> encCBOR (tsInt64 ts)
      <> encCBOR (tsTupleBoolBool ts)
      <> encCBOR (tsTupleBoolBoolBool ts)
      <> encCBOR (tsTupleBoolBoolBoolBool ts)
      <> encCBOR (tsByteString ts)
      <> encCBOR (tsText ts)
      <> encCBOR (tsListBool ts)
      <> encCBOR (tsEitherBoolBool ts)
      <> encCBOR (tsNonEmptyBool ts)
      <> encCBOR (tsMaybeBool ts)
      <> encCBOR (tsMapBoolBool ts)
      <> encCBOR (tsSetBool ts)
      <> encCBOR (tsVectorBool ts)
      <> encCBOR (tsLByteString ts)
      <> encCBOR (tsSByteString ts)
      <> encCBOR (tsUTCTime ts)

instance DecCBOR TestStruct where
  decCBOR = do
    decodeListLenOf 1
    TestStruct
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR

prop_roundTripSerialize' :: Property
prop_roundTripSerialize' = property $ do
  ts <- forAll genTestStruct
  (unsafeDeserialize' byronProtVer . serialize' byronProtVer $ ts) === ts

prop_roundTripEncodeNestedCbor :: Property
prop_roundTripEncodeNestedCbor = property $ do
  ts <- forAll genTestStruct
  let encoded = serialize byronProtVer . encodeNestedCbor $ ts
  decodeFullDecoder byronProtVer "" decodeNestedCbor encoded === Right ts

prop_decodeContainerSkelWithReplicate :: Property
prop_decodeContainerSkelWithReplicate = property $
  assert $ case decode vec of
    Right _ -> True
    _ -> False
  where
    decode :: Encoding -> Either DecoderError (V.Vector ())
    decode enc = decodeFull byronProtVer (serialize byronProtVer enc)

    vec = encodeListLen 4097 <> mconcat (replicate 4097 encodeNull)
