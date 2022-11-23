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
import qualified Data.Time.Calendar.OrdinalDate as Time
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{- HLINT ignore "Redundant <$>" -}

tests :: IO Bool
tests = checkParallel $$(discover)

data TestStruct = TestStruct
  { tsUnit :: (),
    tsBool :: !Bool,
    tsInteger :: !Integer,
    tsWord :: !Word,
    tsWord8 :: !Word8,
    tsWord16 :: !Word16,
    tsWord32 :: !Word32,
    tsWord64 :: !Word64,
    tsInt :: !Int,
    tsFloat :: !Float,
    tsInt32 :: !Int32,
    tsInt64 :: !Int64,
    tsTupleBoolBool :: !(Bool, Bool),
    tsTupleBoolBoolBool :: !(Bool, Bool, Bool),
    tsTupleBoolBoolBoolBool :: !(Bool, Bool, Bool, Bool),
    tsByteString :: !BS.ByteString,
    tsText :: !Text,
    tsListBool :: ![Bool],
    tsEitherBoolBool :: !(Either Bool Bool),
    tsNonEmptyBool :: !(NonEmpty Bool),
    tsMaybeBool :: !(Maybe Bool),
    tsMapBoolBool :: !(Map Bool Bool),
    tsSetBool :: !(Set Bool),
    tsVectorBool :: !(V.Vector Bool),
    tsLByteString :: BS.Lazy.ByteString,
    tsSByteString :: BS.Short.ShortByteString,
    tsUTCTime :: Time.UTCTime
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
    <*> genUTCTime

instance ToCBOR TestStruct where
  toCBOR ts =
    encodeListLen 1
      <> toCBOR (tsUnit ts)
      <> toCBOR (tsBool ts)
      <> toCBOR (tsInteger ts)
      <> toCBOR (tsWord ts)
      <> toCBOR (tsWord8 ts)
      <> toCBOR (tsWord16 ts)
      <> toCBOR (tsWord32 ts)
      <> toCBOR (tsWord64 ts)
      <> toCBOR (tsInt ts)
      <> toCBOR (tsFloat ts)
      <> toCBOR (tsInt32 ts)
      <> toCBOR (tsInt64 ts)
      <> toCBOR (tsTupleBoolBool ts)
      <> toCBOR (tsTupleBoolBoolBool ts)
      <> toCBOR (tsTupleBoolBoolBoolBool ts)
      <> toCBOR (tsByteString ts)
      <> toCBOR (tsText ts)
      <> toCBOR (tsListBool ts)
      <> toCBOR (tsEitherBoolBool ts)
      <> toCBOR (tsNonEmptyBool ts)
      <> toCBOR (tsMaybeBool ts)
      <> toCBOR (tsMapBoolBool ts)
      <> toCBOR (tsSetBool ts)
      <> toCBOR (tsVectorBool ts)
      <> toCBOR (tsLByteString ts)
      <> toCBOR (tsSByteString ts)
      <> toCBOR (tsUTCTime ts)

instance FromCBOR TestStruct where
  fromCBOR = do
    decodeListLenOf 1
    TestStruct
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

genUTCTime :: Gen Time.UTCTime
genUTCTime =
  Time.UTCTime
    <$> genDay
    <*> genDiffTimeOfDay
  where
    -- UTC time takes a DiffTime s.t. 0 <= t < 86401s
    genDiffTimeOfDay :: Gen Time.DiffTime
    genDiffTimeOfDay =
      Time.picosecondsToDiffTime
        <$> Gen.integral (Range.constantFrom 0 0 ((86401e12) - 1))

genDay :: Gen Time.Day
genDay = Time.fromOrdinalDate <$> genYear <*> genDayOfYear

genYear :: Gen Integer
genYear = Gen.integral (Range.linear (-10000) 10000)

genDayOfYear :: Gen Int
genDayOfYear = Gen.int (Range.constantFrom 1 1 366)

prop_roundTripSerialize' :: Property
prop_roundTripSerialize' = property $ do
  ts <- forAll genTestStruct
  (unsafeDeserialize' byronProtVer . serialize' byronProtVer $ ts) === ts

prop_roundTripEncodeNestedCbor :: Property
prop_roundTripEncodeNestedCbor = property $ do
  ts <- forAll genTestStruct
  let encoded = serializeEncoding byronProtVer . encodeNestedCbor $ ts
  decodeFullDecoder byronProtVer "" decodeNestedCbor encoded === Right ts

prop_decodeContainerSkelWithReplicate :: Property
prop_decodeContainerSkelWithReplicate = property $
  assert $ case decode vec of
    Right _ -> True
    _ -> False
  where
    decode :: Encoding -> Either DecoderError (V.Vector ())
    decode enc = decodeFull byronProtVer (serializeEncoding byronProtVer enc)

    vec = encodeListLen 4097 <> mconcat (replicate 4097 encodeNull)
