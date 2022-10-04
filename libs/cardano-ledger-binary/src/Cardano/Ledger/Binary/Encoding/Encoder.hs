{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Binary.Encoding.Encoder
  ( -- * Decoders
    Encoding,
    toBuilder,
    C.Tokens (..),
    toPlainEncoding,
    fromPlainEncoding,
    fromPlainEncodingWithVersion,
    withCurrentEncodingVersion,
    enforceVersionEncoding,

    -- ** Custom
    encodeMaybe,
    encodeNullMaybe,
    encodePair,
    encodeTuple,
    encodeRatio,

    -- *** Containers
    encodeList,
    encodeSeq,
    encodeSet,
    encodeMap,
    encodeVMap,
    encodeVector,

    -- *** Time
    encodeUTCTime,
    encodeNominalDiffTime,

    -- *** Network
    encodeIPv4,
    ipv4ToBytes,
    encodeIPv6,
    ipv6ToBytes,

    -- ** Original
    encodeWord,
    encodeWord8,
    encodeWord16,
    encodeWord32,
    encodeWord64,
    encodeInt,
    encodeInt8,
    encodeInt16,
    encodeInt32,
    encodeInt64,
    encodeInteger,
    encodeBytes,
    encodeBytesIndef,
    encodeByteArray,
    encodeString,
    encodeStringIndef,
    encodeUtf8ByteArray,
    encodeListLen,
    encodeListLenIndef,
    encodeMapLen,
    encodeMapLenIndef,
    encodeBreak,
    encodeTag,
    encodeTag64,
    encodeBool,
    encodeUndef,
    encodeNull,
    encodeSimple,
    encodeFloat16,
    encodeFloat,
    encodeDouble,
    encodePreEncoded,
    encodeTerm,
  )
where

import qualified Cardano.Binary as C
import Cardano.Ledger.Binary.Decoding.Decoder (setTag)
import Cardano.Ledger.Binary.Version
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray)
import qualified Codec.CBOR.Term as C (Term (..), encodeTerm)
import qualified Codec.CBOR.Write as CBOR (toBuilder)
import Data.Binary.Put (putWord32le, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BSL
import Data.Fixed (E12, resolution)
import Data.Foldable (foldMap')
import Data.IP (IPv4, IPv6, toHostAddress, toHostAddress6)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (Proxy))
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), diffTimeToPicoseconds)
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import Data.Word (Word16, Word32, Word64, Word8)
import Prelude hiding (encodeFloat)

--------------------------------------------------------------------------------
-- Versioned Encoder
--------------------------------------------------------------------------------

newtype Encoding = Encoding (Version -> C.Encoding)
  deriving (Semigroup, Monoid)

fromPlainEncoding :: C.Encoding -> Encoding
fromPlainEncoding enc = Encoding (const enc)

fromPlainEncodingWithVersion :: (Version -> C.Encoding) -> Encoding
fromPlainEncodingWithVersion = Encoding

toPlainEncoding :: Version -> Encoding -> C.Encoding
toPlainEncoding v (Encoding enc) = enc v

toBuilder :: Version -> Encoding -> Builder
toBuilder version (Encoding enc) = CBOR.toBuilder $ enc version

-- | Get access to the current version being used in the encoder
withCurrentEncodingVersion :: (Version -> Encoding) -> Encoding
withCurrentEncodingVersion f =
  Encoding $ \version -> toPlainEncoding version $ f version

-- | Ignore the current version of the encoder and enforce the supplied one instead.
enforceVersionEncoding :: Version -> Encoding -> Encoding
enforceVersionEncoding version encoding = fromPlainEncoding (toPlainEncoding version encoding)

-- | Conditionoly choose the encoder newer or older deceder, depending on the current
-- version. Supplied version acts as a pivot.
--
-- =====__Example__
ifEncodingVersionAtLeast ::
  Version ->
  -- | Use this encoder if current encoder version is larger or equal to the supplied
  -- `Version`
  Encoding ->
  -- | Use this encoder if current encoder version is lower than the supplied `Version`
  Encoding ->
  Encoding
ifEncodingVersionAtLeast atLeast (Encoding newerEncoding) (Encoding olderEncoding) =
  Encoding $ \cur ->
    if cur >= atLeast
      then newerEncoding cur
      else olderEncoding cur

--------------------------------------------------------------------------------
-- Wrapped CBORG encoders
--------------------------------------------------------------------------------

encodeWord :: Word -> Encoding
encodeWord e = fromPlainEncoding (C.encodeWord e)

encodeWord8 :: Word8 -> Encoding
encodeWord8 e = fromPlainEncoding (C.encodeWord8 e)

encodeWord16 :: Word16 -> Encoding
encodeWord16 e = fromPlainEncoding (C.encodeWord16 e)

encodeWord32 :: Word32 -> Encoding
encodeWord32 e = fromPlainEncoding (C.encodeWord32 e)

encodeWord64 :: Word64 -> Encoding
encodeWord64 e = fromPlainEncoding (C.encodeWord64 e)

encodeInt :: Int -> Encoding
encodeInt e = fromPlainEncoding (C.encodeInt e)

encodeInt8 :: Int8 -> Encoding
encodeInt8 e = fromPlainEncoding (C.encodeInt8 e)

encodeInt16 :: Int16 -> Encoding
encodeInt16 e = fromPlainEncoding (C.encodeInt16 e)

encodeInt32 :: Int32 -> Encoding
encodeInt32 e = fromPlainEncoding (C.encodeInt32 e)

encodeInt64 :: Int64 -> Encoding
encodeInt64 e = fromPlainEncoding (C.encodeInt64 e)

encodeInteger :: Integer -> Encoding
encodeInteger e = fromPlainEncoding (C.encodeInteger e)

encodeBytes :: ByteString -> Encoding
encodeBytes e = fromPlainEncoding (C.encodeBytes e)

encodeBytesIndef :: Encoding
encodeBytesIndef = fromPlainEncoding C.encodeBytesIndef

encodeByteArray :: SlicedByteArray -> Encoding
encodeByteArray e = fromPlainEncoding (C.encodeByteArray e)

encodeString :: Text -> Encoding
encodeString e = fromPlainEncoding (C.encodeString e)

encodeStringIndef :: Encoding
encodeStringIndef = fromPlainEncoding C.encodeStringIndef

encodeUtf8ByteArray :: SlicedByteArray -> Encoding
encodeUtf8ByteArray e = fromPlainEncoding (C.encodeUtf8ByteArray e)

encodeListLen :: Word -> Encoding
encodeListLen e = fromPlainEncoding (C.encodeListLen e)

encodeListLenIndef :: Encoding
encodeListLenIndef = fromPlainEncoding C.encodeListLenIndef

encodeMapLen :: Word -> Encoding
encodeMapLen e = fromPlainEncoding (C.encodeMapLen e)

encodeMapLenIndef :: Encoding
encodeMapLenIndef = fromPlainEncoding C.encodeMapLenIndef

encodeBreak :: Encoding
encodeBreak = fromPlainEncoding C.encodeBreak

encodeTag :: Word -> Encoding
encodeTag e = fromPlainEncoding (C.encodeTag e)

encodeTag64 :: Word64 -> Encoding
encodeTag64 e = fromPlainEncoding (C.encodeTag64 e)

encodeBool :: Bool -> Encoding
encodeBool e = fromPlainEncoding (C.encodeBool e)

encodeUndef :: Encoding
encodeUndef = fromPlainEncoding C.encodeUndef

encodeNull :: Encoding
encodeNull = fromPlainEncoding C.encodeNull

encodeSimple :: Word8 -> Encoding
encodeSimple e = fromPlainEncoding (C.encodeSimple e)

encodeFloat16 :: Float -> Encoding
encodeFloat16 e = fromPlainEncoding (C.encodeFloat16 e)

encodeFloat :: Float -> Encoding
encodeFloat e = fromPlainEncoding (C.encodeFloat e)

encodeDouble :: Double -> Encoding
encodeDouble e = fromPlainEncoding (C.encodeDouble e)

encodePreEncoded :: ByteString -> Encoding
encodePreEncoded e = fromPlainEncoding (C.encodePreEncoded e)

encodeTerm :: C.Term -> Encoding
encodeTerm = fromPlainEncoding . C.encodeTerm

--------------------------------------------------------------------------------
-- Custom
--------------------------------------------------------------------------------

encodeRatio :: (t -> Encoding) -> Ratio t -> Encoding
encodeRatio encodeNumeric r =
  ifEncodingVersionAtLeast (natVersion @2) (encodeTag 30) mempty
    <> encodeListLen 2
    <> encodeNumeric (numerator r)
    <> encodeNumeric (denominator r)

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

encodeMaybe :: (a -> Encoding) -> Maybe a -> Encoding
encodeMaybe encodeValue = \case
  Nothing -> encodeListLen 0
  Just x -> encodeListLen 1 <> encodeValue x

-- | Alternative way to encode a Maybe type.
--
-- /Note/ - this is not the default method for encoding `Maybe`, use `encodeMaybe` instead
encodeNullMaybe :: (a -> Encoding) -> Maybe a -> Encoding
encodeNullMaybe encodeValue = \case
  Nothing -> encodeNull
  Just x -> encodeValue x

encodeTuple :: (a -> Encoding) -> (b -> Encoding) -> (a, b) -> Encoding
encodeTuple encodeFirst encodeSecond (x, y) =
  encodeListLen 2
    <> encodeFirst x
    <> encodeSecond y

encodePair :: (a -> Encoding) -> (b -> Encoding) -> (a, b) -> Encoding
encodePair = encodeTuple
{-# DEPRECATED encodePair "In favor of `encodeTuple`" #-}

--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

-- | Encode a Map. Versions variance:
--
-- * [< 2] - Variable length encoding.
--
-- * [>= 2] - Variable length encoding for Maps larger than 23 key value pairs, otherwise exact
--   length encoding
encodeMap ::
  (k -> Encoding) ->
  (v -> Encoding) ->
  Map.Map k v ->
  Encoding
encodeMap encodeKey encodeValue m =
  let mapEncoding = Map.foldMapWithKey (\k v -> encodeKey k <> encodeValue v) m
   in ifEncodingVersionAtLeast
        (natVersion @2)
        (variableMapLenEncoding (Map.size m) mapEncoding)
        (exactMapLenEncoding (Map.size m) mapEncoding)
{-# INLINE encodeMap #-}

encodeVMap ::
  (VMap.Vector kv k, VMap.Vector vv v) =>
  (k -> Encoding) ->
  (v -> Encoding) ->
  VMap.VMap kv vv k v ->
  Encoding
encodeVMap encodeKey encodeValue m =
  let mapEncoding = VMap.foldMapWithKey (\k v -> encodeKey k <> encodeValue v) m
   in ifEncodingVersionAtLeast
        (natVersion @2)
        (variableMapLenEncoding (VMap.size m) mapEncoding)
        (exactMapLenEncoding (VMap.size m) mapEncoding)
{-# INLINE encodeVMap #-}

-- Usage of fromIntegral in `exactMapLenEncoding` is safe, since it is an internal function
-- and is applied to Map's size.
exactMapLenEncoding :: Int -> Encoding -> Encoding
exactMapLenEncoding len contents =
  encodeMapLen (fromIntegral len :: Word) <> contents
{-# INLINE exactMapLenEncoding #-}

-- | Conditionally use variable length encoding, but only for Maps larger than 23
variableMapLenEncoding :: Int -> Encoding -> Encoding
variableMapLenEncoding len contents =
  if len <= lengthThreshold
    then exactMapLenEncoding len contents
    else encodeMapLenIndef <> contents <> encodeBreak
{-# INLINE variableMapLenEncoding #-}

-- | This is the optimal maximum number for encoding exact length. Above that threashold
-- using variable length encoding will result in less bytes on the wire.
lengthThreshold :: Int
lengthThreshold = 23

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

-- Usage of fromIntegral in `exactListLenEncoding` is safe, since it is an internal function
-- and is applied to List's size.
exactListLenEncoding :: Int -> Encoding -> Encoding
exactListLenEncoding len contents =
  encodeListLen (fromIntegral len :: Word) <> contents
{-# INLINE exactListLenEncoding #-}

-- | Conditionally use variable length encoding, but only for lists and sets larger than
-- 23
variableListLenEncoding :: Int -> Encoding -> Encoding
variableListLenEncoding len contents =
  if len <= lengthThreshold
    then exactListLenEncoding len contents
    else encodeListLenIndef <> contents <> encodeBreak
{-# INLINE variableListLenEncoding #-}

-- | Encode a Set. Versions variance:
--
-- * [< 2] - Variable length encoding. Also prefixes with a special 258 tag.
--
-- * [>= 2] - Variable length encoding for Sets larger than 23 elements, otherwise exact
--   length encoding
encodeSet :: (a -> Encoding) -> Set.Set a -> Encoding
encodeSet encodeValue f =
  let foldableEncoding = foldMap' encodeValue f
   in ifEncodingVersionAtLeast
        (natVersion @2)
        (variableListLenEncoding (Set.size f) foldableEncoding)
        (encodeTag setTag <> exactListLenEncoding (Set.size f) foldableEncoding)
{-# INLINE encodeSet #-}

-- | Encode a list. Versions variance:
--
-- * [< 2] - Variable length encoding
--
-- * [>= 2] - Variable length encoding for lists longer than 23 elements, otherwise exact
--   length encoding
encodeList :: (a -> Encoding) -> [a] -> Encoding
encodeList encodeValue xs =
  let varLenEncList =
        encodeListLenIndef <> foldr (\x r -> encodeValue x <> r) encodeBreak xs
      (Sum len, exactLenEncList) = foldMap' (\v -> (1, encodeValue v)) xs
      -- we don't want to compute the length of the list, unless it is smaller than the
      -- threshold
      encListVer2 =
        case drop lengthThreshold xs of
          [] -> exactListLenEncoding len exactLenEncList
          _ -> varLenEncList
   in ifEncodingVersionAtLeast (natVersion @2) encListVer2 varLenEncList

-- | Encode a Seq. Variable length encoding for Sequences larger than 23 elements,
--   otherwise exact length encoding
encodeSeq :: (a -> Encoding) -> Seq.Seq a -> Encoding
encodeSeq encodeValue f = variableListLenEncoding (Seq.length f) (foldMap' encodeValue f)
{-# INLINE encodeSeq #-}

--------------------------------------------------------------------------------
-- Vector
--------------------------------------------------------------------------------
encodeContainerSkel ::
  (Word -> Encoding) ->
  (container -> Int) ->
  (accumFunc -> Encoding -> container -> Encoding) ->
  accumFunc ->
  container ->
  Encoding
encodeContainerSkel encodeLen size foldFunction f c =
  encodeLen (fromIntegral (size c)) <> foldFunction f mempty c
{-# INLINE encodeContainerSkel #-}

-- | Generic encoder for vectors. Its intended use is to allow easy
-- definition of 'ToCBOR' instances for custom vector
encodeVector :: VG.Vector v a => (a -> Encoding) -> v a -> Encoding
encodeVector encodeValue =
  encodeContainerSkel
    encodeListLen
    VG.length
    VG.foldr
    (\a b -> encodeValue a <> b)
{-# INLINE encodeVector #-}

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

encodeUTCTime :: UTCTime -> Encoding
encodeUTCTime (UTCTime day timeOfDay) =
  encodeListLen 3
    <> encodeInteger year
    <> encodeInt dayOfYear
    <> encodeInteger timeOfDayPico
  where
    (year, dayOfYear) = toOrdinalDate day
    timeOfDayPico = diffTimeToPicoseconds timeOfDay

encodeNominalDiffTime :: NominalDiffTime -> Encoding
encodeNominalDiffTime = encodeInteger . (`div` 1_000_000) . toPicoseconds
  where
    toPicoseconds t =
      numerator (toRational t * toRational (resolution $ Proxy @E12))

--------------------------------------------------------------------------------
-- Network
--------------------------------------------------------------------------------

ipv4ToBytes :: IPv4 -> BS.ByteString
ipv4ToBytes = BSL.toStrict . runPut . putWord32le . toHostAddress

encodeIPv4 :: IPv4 -> Encoding
encodeIPv4 = encodeBytes . ipv4ToBytes

ipv6ToBytes :: IPv6 -> BS.ByteString
ipv6ToBytes ipv6 = BSL.toStrict . runPut $ do
  let (w1, w2, w3, w4) = toHostAddress6 ipv6
  putWord32le w1
  putWord32le w2
  putWord32le w3
  putWord32le w4

encodeIPv6 :: IPv6 -> Encoding
encodeIPv6 = encodeBytes . ipv6ToBytes
