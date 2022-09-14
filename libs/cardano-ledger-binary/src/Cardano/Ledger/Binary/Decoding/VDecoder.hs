{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Binary.Decoding.VDecoder
  ( -- * Decoders
    VDecoder,
    toVDecoder,
    fromVDecoder,
    C.DecoderError (..),
    C.ByteOffset,
    C.DecodeAction (..),
    C.TokenType (..),

    -- ** Running decoders
    decodeFullDecoder,
    decodeFullDecoderProxy,

    -- ** Versioning
    decVerToProxy,
    getDecVerProxy,
    decVer,
    getDecVer,
    ifDecVerAtLeast,
    ifDecVerAtLeastProxy,

    -- * Error reporting
    cborError,

    -- ** Custom decoders
    decodeUTCTime,
    decodeNominalDiffTime,
    decodeVector,
    decodeSet,
    decodeMap,
    decodeVMap,

    -- ** Lifted @cborg@ decoders
    enforceSize,
    matchSize,
    fromCBORMaybe,
    decodeList,
    decodeBool,
    decodeBreakOr,
    decodeByteArray,
    decodeByteArrayCanonical,
    decodeBytes,
    decodeBytesCanonical,
    decodeBytesIndef,
    decodeDouble,
    decodeDoubleCanonical,
    decodeFloat,
    decodeFloat16Canonical,
    decodeFloatCanonical,
    decodeInt,
    decodeInt16,
    decodeInt16Canonical,
    decodeInt32,
    decodeInt32Canonical,
    decodeInt64,
    decodeInt64Canonical,
    decodeInt8,
    decodeInt8Canonical,
    decodeIntCanonical,
    decodeInteger,
    decodeIntegerCanonical,
    decodeNatural,
    decodeListLen,
    decodeListLenCanonical,
    decodeListLenCanonicalOf,
    decodeListLenIndef,
    decodeListLenOf,
    decodeListLenOrIndef,
    decodeMapLen,
    decodeMapLenCanonical,
    decodeMapLenIndef,
    decodeMapLenOrIndef,
    decodeNegWord,
    decodeNegWord64,
    decodeNegWord64Canonical,
    decodeNegWordCanonical,
    decodeNull,
    decodeSequenceLenIndef,
    decodeSequenceLenN,
    decodeSimple,
    decodeSimpleCanonical,
    decodeString,
    decodeStringCanonical,
    decodeStringIndef,
    decodeTag,
    decodeTag64,
    decodeTag64Canonical,
    decodeTagCanonical,
    decodeUtf8ByteArray,
    decodeUtf8ByteArrayCanonical,
    decodeWithByteSpan,
    decodeWord,
    decodeWord16,
    decodeWord16Canonical,
    decodeWord32,
    decodeWord32Canonical,
    decodeWord64,
    decodeWord64Canonical,
    decodeWord8,
    decodeWord8Canonical,
    decodeWordCanonical,
    decodeWordCanonicalOf,
    decodeWordOf,
    peekAvailable,
    peekByteOffset,
    peekTokenType,
  )
where

import qualified Cardano.Binary as C
  ( DecoderError (..),
    decodeFullDecoder,
    decodeListWith,
    enforceSize,
    fromCBORMaybe,
    matchSize,
  )
import Codec.CBOR.ByteArray (ByteArray)
import qualified Codec.CBOR.Decoding as C
  ( ByteOffset,
    DecodeAction (..),
    Decoder,
    TokenType (..),
    decodeBool,
    decodeBreakOr,
    decodeByteArray,
    decodeByteArrayCanonical,
    decodeBytes,
    decodeBytesCanonical,
    decodeBytesIndef,
    decodeDouble,
    decodeDoubleCanonical,
    decodeFloat,
    decodeFloat16Canonical,
    decodeFloatCanonical,
    decodeInt,
    decodeInt16,
    decodeInt16Canonical,
    decodeInt32,
    decodeInt32Canonical,
    decodeInt64,
    decodeInt64Canonical,
    decodeInt8,
    decodeInt8Canonical,
    decodeIntCanonical,
    decodeInteger,
    decodeIntegerCanonical,
    decodeListLen,
    decodeListLenCanonical,
    decodeListLenCanonicalOf,
    decodeListLenIndef,
    decodeListLenOf,
    decodeListLenOrIndef,
    decodeMapLen,
    decodeMapLenCanonical,
    decodeMapLenIndef,
    decodeMapLenOrIndef,
    decodeNegWord,
    decodeNegWord64,
    decodeNegWord64Canonical,
    decodeNegWordCanonical,
    decodeNull,
    decodeSequenceLenIndef,
    decodeSequenceLenN,
    decodeSimple,
    decodeSimpleCanonical,
    decodeString,
    decodeStringCanonical,
    decodeStringIndef,
    decodeTag,
    decodeTag64,
    decodeTag64Canonical,
    decodeTagCanonical,
    decodeUtf8ByteArray,
    decodeUtf8ByteArrayCanonical,
    decodeWithByteSpan,
    decodeWord,
    decodeWord16,
    decodeWord16Canonical,
    decodeWord32,
    decodeWord32Canonical,
    decodeWord64,
    decodeWord64Canonical,
    decodeWord8,
    decodeWord8Canonical,
    decodeWordCanonical,
    decodeWordCanonicalOf,
    decodeWordOf,
    peekAvailable,
    peekByteOffset,
    peekTokenType,
  )
import Control.Monad (replicateM, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Reflection (reifyNat)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), picosecondsToDiffTime)
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import Data.Word (Word16, Word32, Word64, Word8)
import Formatting (build, formatToString)
import GHC.Exts as Exts (IsList (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Numeric.Natural (Natural)
import Prelude hiding (decodeFloat)

#if __GLASGOW_HASKELL__ < 902
import Cardano.Ledger.Binary.Decoding.NatsCompat (OrderingI(LTI), cmpNat)
#endif

--------------------------------------------------------------------------------
-- Versioned Decoder
--------------------------------------------------------------------------------

newtype VDecoder (v :: Nat) s a = VDecoder {unVDecoder :: C.Decoder s a}
  deriving (Functor, Applicative, Monad, MonadFail)

-- | Promote a regular `C.Decoder` to a version one
toVDecoder :: C.Decoder s a -> VDecoder v s a
toVDecoder = VDecoder

-- | Extract the underlying `C.Decoder` by specifying the concrete version to be used.
fromVDecoder :: forall v s a. KnownNat v => VDecoder v s a -> C.Decoder s a
fromVDecoder vd@(VDecoder d) = d
  where
    -- We need to ensure that 'v' type parameter is set to a specific version, before we
    -- unwrap the VDecoder.
    _enforceKnownNat = decVer vd

decodeFullDecoder ::
  Natural ->
  T.Text ->
  (forall v s. KnownNat v => VDecoder v s a) ->
  BSL.ByteString ->
  Either C.DecoderError a
decodeFullDecoder v txt vd bsl =
  reifyNat (toInteger v) (\px -> decodeFullDecoderProxy px txt vd bsl)

decodeFullDecoderProxy ::
  forall v a.
  Proxy v ->
  T.Text ->
  (forall s. VDecoder v s a) ->
  BSL.ByteString ->
  Either C.DecoderError a
decodeFullDecoderProxy _ txt vd = C.decodeFullDecoder txt (unVDecoder vd)

--------------------------------------------------------------------------------
-- Working with current decoder version
--------------------------------------------------------------------------------

-- | Extract type level version of the decoder from its type
decVerToProxy :: VDecoder v s a -> Proxy v
decVerToProxy _ = Proxy

-- | Use monadic syntax to extract type level version of the decoder from its type
getDecVerProxy :: VDecoder v s (Proxy v)
getDecVerProxy = pure Proxy

-- | Extract value level version of a decoder's type
--
-- >>> decVer $ decodeInt @3
-- 3
decVer :: KnownNat v => VDecoder v s a -> Natural
decVer = fromInteger . natVal . decVerToProxy

-- | Use monadic syntax to extract value level version of the decoder from its type
--
-- >>> decodeFullDecoder 3 "Version" getDecVer ""
-- Right 3
getDecVer :: KnownNat v => VDecoder v s Natural
getDecVer = fromInteger . natVal <$> getDecVerProxy

-- | Conditionoly choose the decoder newer or older deceder, depending on the current
-- version, which is supplied as a type argument.
--
-- =====__Example__
--
-- Let's say prior to the version 2 some type `Foo` was backed by `Word16`, but at the 2nd
-- version onwards it was switched to `Word32` instead. In order to support both versions,
-- we change the type, but we also use this condition to keep backwards compatibility of
-- the decoder:
--
-- >>> :set -XTypeApplications
-- >>> newtype Foo = Foo Word32
-- >>> decFoo = Foo <$> ifDecVerAtLeast @2 (fromIntegral <$> decodeWord16) decodeWord32
-- >>> :t decFoo
ifDecVerAtLeast ::
  forall vl v s a.
  (KnownNat vl, KnownNat v) =>
  VDecoder v s a ->
  VDecoder v s a ->
  VDecoder v s a
ifDecVerAtLeast = ifDecVerAtLeastProxy (Proxy :: Proxy vl)

ifDecVerAtLeastProxy ::
  (KnownNat vl, KnownNat v) =>
  -- | If cuurent decoder version is higher or equal to supplied version then first
  -- argument is used, otherwise the second
  Proxy vl ->
  -- | Newer decoder
  VDecoder v s a ->
  -- | Older decoder
  VDecoder v s a ->
  VDecoder v s a
ifDecVerAtLeastProxy atLeast newerDecoder olderDecoder = do
  let cur = decVerToProxy newerDecoder
  case cmpNat cur atLeast of
    LTI -> olderDecoder
    _ -> newerDecoder

--------------------------------------------------------------------------------
-- Error reporting
--------------------------------------------------------------------------------

cborError :: C.DecoderError -> VDecoder v s a
cborError = fail . formatToString build

decodeContainerSkelWithReplicate ::
  -- | How to get the size of the container
  VDecoder v s Int ->
  -- | replicateM for the container
  (Int -> VDecoder v s c) ->
  -- | concat for the container
  ([c] -> c) ->
  VDecoder v s c
decodeContainerSkelWithReplicate decodeLen replicateFun concatList = do
  -- Look at how much data we have at the moment and use it as the limit for
  -- the size of a single call to replicateFun. We don't want to use
  -- replicateFun directly on the result of decodeLen since this might lead to
  -- DOS attack (attacker providing a huge value for length). So if it's above
  -- our limit, we'll do manual chunking and then combine the containers into
  -- one.
  sz <- decodeLen
  limit <- peekAvailable
  if sz <= limit
    then replicateFun sz
    else do
      -- Take the max of limit and a fixed chunk size (note: limit can be
      -- 0). This basically means that the attacker can make us allocate a
      -- container of size 128 even though there's no actual input.
      let chunkSize = max limit 128
          (d, m) = sz `divMod` chunkSize
      containers <- sequence $ replicateFun m : replicate d (replicateFun chunkSize)
      return $! concatList containers
{-# INLINE decodeContainerSkelWithReplicate #-}

--------------------------------------------------------------------------------
-- Decoder for Map
--------------------------------------------------------------------------------

-- | Checks canonicity by comparing the new key being decoded with
--   the previous one, to enfore these are sorted the correct way.
--   See: https://tools.ietf.org/html/rfc7049#section-3.9
--   "[..]The keys in every map must be sorted lowest value to highest.[...]"
decodeMapSkel ::
  forall k a m v s.
  Ord k =>
  -- | Decoded list is guaranteed to be sorted on keys in descending order without any
  -- duplicate keys.
  ([(k, a)] -> m) ->
  -- | Decoder for keys
  VDecoder v s k ->
  -- | Decoder for values
  VDecoder v s a ->
  VDecoder v s m
decodeMapSkel fromDistinctDescList decodeKey decodeValue = do
  n <- decodeMapLen
  fromDistinctDescList <$> case n of
    0 -> return []
    _ -> do
      (firstKey, firstValue) <- decodeEntry
      decodeEntries (n - 1) firstKey [(firstKey, firstValue)]
  where
    -- Decode a single (k,v).
    decodeEntry :: VDecoder v s (k, a)
    decodeEntry = do
      !k <- decodeKey
      !v <- decodeValue
      return (k, v)

    -- Decode all the entries, enforcing canonicity by ensuring that the
    -- previous key is smaller than the next one.
    decodeEntries :: Int -> k -> [(k, a)] -> VDecoder v s [(k, a)]
    decodeEntries 0 _ acc = pure acc
    decodeEntries !remainingPairs previousKey !acc = do
      p@(newKey, _) <- decodeEntry
      -- Order of keys needs to be strictly increasing, because otherwise it's
      -- possible to supply lists with various amount of duplicate keys which
      -- will result in the same map as long as the last value of the given
      -- key on the list is the same in all of them.
      if newKey > previousKey
        then decodeEntries (remainingPairs - 1) newKey (p : acc)
        else cborError $ C.DecoderErrorCanonicityViolation "Map"
{-# INLINE decodeMapSkel #-}

decodeMapV1 ::
  forall k a v s.
  Ord k =>
  -- | Decoder for keys
  VDecoder v s k ->
  -- | Decoder for values
  VDecoder v s a ->
  VDecoder v s (Map.Map k a)
decodeMapV1 = decodeMapSkel Map.fromDistinctDescList

decodeMapContents :: VDecoder v s a -> VDecoder v s [a]
decodeMapContents = decodeCollection decodeMapLenOrIndef

decodeCollection :: VDecoder v s (Maybe Int) -> VDecoder v s a -> VDecoder v s [a]
decodeCollection lenOrIndef el = snd <$> decodeCollectionWithLen lenOrIndef el

decodeCollectionWithLen ::
  VDecoder v s (Maybe Int) ->
  VDecoder v s a ->
  VDecoder v s (Int, [a])
decodeCollectionWithLen lenOrIndef el = do
  lenOrIndef >>= \case
    Just len -> (,) len <$> replicateM len el
    Nothing -> loop (0, []) (not <$> decodeBreakOr) el
  where
    loop (!n, !acc) condition action =
      condition >>= \case
        False -> pure (n, reverse acc)
        True -> action >>= \v -> loop (n + 1, v : acc) condition action

decodeMapByKey ::
  forall k a t v s.
  (Exts.IsList t, Exts.Item t ~ (k, a)) =>
  -- | Decoder for keys
  VDecoder v s k ->
  -- | Decoder for values
  (k -> VDecoder v s a) ->
  VDecoder v s t
decodeMapByKey decodeKey decodeValueFor =
  uncurry Exts.fromListN
    <$> decodeCollectionWithLen decodeMapLenOrIndef decodeInlinedPair
  where
    decodeInlinedPair = do
      !key <- decodeKey
      !value <- decodeValueFor key
      pure (key, value)


-- | We want to make a uniform way of encoding and decoding `Map.Map`. The otiginal ToCBOR
-- and FromCBOR instances date to Byron Era didn't support versioning were not always
-- cannonical. We want to make these cannonical improvements staring with protocol version
-- 2.
decodeMapV2 ::
  forall k a v s.
  Ord k =>
  -- | Decoder for keys
  VDecoder v s k ->
  -- | Decoder for values
  VDecoder v s a ->
  VDecoder v s (Map.Map k a)
decodeMapV2 decodeKey decodeValue = decodeMapByKey decodeKey (const decodeValue)

-- | An example of how to use versioning
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Codec.CBOR.FlatTerm
-- >>> fromFlatTerm (fromVDecoder @1 (decodeMap decodeInt decodeBytes)) [TkMapLen 2,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar"]
-- Right (fromList [(1,"Foo"),(2,"Bar")])
-- >>> fromFlatTerm (fromVDecoder @2 (decodeMap decodeInt decodeBytes)) [TkMapBegin,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar"]
-- Left "decodeMapLen: unexpected token TkMapBegin"
-- >>> fromFlatTerm (fromVDecoder @2 (decodeMap decodeInt decodeBytes)) [TkMapBegin,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar",TkBreak]
-- Right (fromList [(1,"Foo"),(2,"Bar")])
decodeMap ::
  (KnownNat v, Ord k) =>
  VDecoder v s k ->
  VDecoder v s a ->
  VDecoder v s (Map.Map k a)
decodeMap decodeKey decodeValue =
  ifDecVerAtLeast @2
    (decodeMapV2 decodeKey decodeValue)
    (decodeMapV1 decodeKey decodeValue)

decodeVMap ::
  (VMap.Vector kv k, VMap.Vector av a, Ord k) =>
  VDecoder v s k ->
  VDecoder v s a ->
  VDecoder v s (VMap.VMap kv av k a)
decodeVMap decodeKey decodeValue = decodeMapByKey decodeKey (const decodeValue)

-- | We stitch a `258` in from of a (Hash)Set, so that tools which
-- programmatically check for canonicity can recognise it from a normal
-- array. Why 258? This will be formalised pretty soon, but IANA allocated
-- 256...18446744073709551615 to "First come, first served":
-- https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml Currently `258` is
-- the first unassigned tag and as it requires 2 bytes to be encoded, it sounds
-- like the best fit.
setTag :: Word
setTag = 258

decodeSetTag :: VDecoder v s ()
decodeSetTag = do
  t <- decodeTag
  when (t /= setTag) $ cborError $ C.DecoderErrorUnknownTag "Set" (fromIntegral t)

decodeSetSkel ::
  forall a v s c.
  Ord a =>
  -- | Decoded list is guaranteed to be sorted on keys in descending order without any
  -- duplicate keys.
  ([a] -> c) ->
  VDecoder v s a ->
  VDecoder v s c
decodeSetSkel fromDistinctDescList decodeValue = do
  decodeSetTag
  n <- decodeListLen
  fromDistinctDescList <$> case n of
    0 -> return []
    _ -> do
      firstValue <- decodeValue
      decodeEntries (n - 1) firstValue [firstValue]
  where
    decodeEntries :: Int -> a -> [a] -> VDecoder v s [a]
    decodeEntries 0 _ acc = pure acc
    decodeEntries !remainingEntries previousValue !acc = do
      newValue <- decodeValue
      -- Order of values needs to be strictly increasing, because otherwise
      -- it's possible to supply lists with various amount of duplicates which
      -- will result in the same set.
      if newValue > previousValue
        then decodeEntries (remainingEntries - 1) newValue (newValue : acc)
        else cborError $ C.DecoderErrorCanonicityViolation "Set"
{-# INLINE decodeSetSkel #-}

decodeSet :: (Ord a, KnownNat v) => VDecoder v s a -> VDecoder v s (Set.Set a)
decodeSet valueDecoder =
  ifDecVerAtLeast @2
    (Set.fromList <$> decodeList valueDecoder)
    (decodeSetSkel Set.fromDistinctDescList valueDecoder)

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: VG.Vector vec a => VDecoder v s a -> VDecoder v s (vec a)
decodeVector decodeValue =
  decodeContainerSkelWithReplicate
    decodeListLen
    (`VG.replicateM` decodeValue)
    VG.concat
{-# INLINE decodeVector #-}

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

decodeUTCTime :: VDecoder v s UTCTime
decodeUTCTime = do
  enforceSize "UTCTime" 3
  year <- decodeInteger
  dayOfYear <- decodeInt
  timeOfDayPico <- decodeInteger
  return $
    UTCTime
      (fromOrdinalDate year dayOfYear)
      (picosecondsToDiffTime timeOfDayPico)

-- | For backwards compatibility we round pico precision to micro
decodeNominalDiffTime :: VDecoder v s NominalDiffTime
decodeNominalDiffTime = fromRational . (% 1_000_000) <$> decodeInteger

--------------------------------------------------------------------------------
-- Promoted CBORG primitive decoders
--------------------------------------------------------------------------------

-- | Enforces that the input size is the same as the decoded one, failing in
--   case it's not
enforceSize :: T.Text -> Int -> VDecoder v s ()
enforceSize lbl requestedSize = VDecoder (C.enforceSize lbl requestedSize)

-- | Compare two sizes, failing if they are not equal
matchSize :: T.Text -> Int -> Int -> VDecoder v s ()
matchSize lbl requestedSize actualSize = VDecoder (C.matchSize lbl requestedSize actualSize)

-- | @'D.Decoder'@ for list.
decodeList :: KnownNat v => VDecoder v s a -> VDecoder v s [a]
decodeList decodeValue@(VDecoder d) =
  ifDecVerAtLeast @2
    (decodeCollection decodeListLenOrIndef decodeValue)
    (VDecoder (C.decodeListWith d))

fromCBORMaybe :: VDecoder v s a -> VDecoder v s (Maybe a)
fromCBORMaybe = VDecoder . C.fromCBORMaybe . unVDecoder

decodeBool :: VDecoder v s Bool
decodeBool = VDecoder C.decodeBool

decodeBreakOr :: VDecoder v s Bool
decodeBreakOr = VDecoder C.decodeBreakOr

decodeByteArray :: VDecoder v s ByteArray
decodeByteArray = VDecoder C.decodeByteArray

decodeByteArrayCanonical :: VDecoder v s ByteArray
decodeByteArrayCanonical = VDecoder C.decodeByteArrayCanonical

decodeBytes :: VDecoder v s BS.ByteString
decodeBytes = VDecoder C.decodeBytes

decodeBytesCanonical :: VDecoder v s BS.ByteString
decodeBytesCanonical = VDecoder C.decodeBytesCanonical

decodeBytesIndef :: VDecoder v s ()
decodeBytesIndef = VDecoder C.decodeBytesIndef

decodeDouble :: VDecoder v s Double
decodeDouble = VDecoder C.decodeDouble

decodeDoubleCanonical :: VDecoder v s Double
decodeDoubleCanonical = VDecoder C.decodeDoubleCanonical

decodeFloat :: VDecoder v s Float
decodeFloat = VDecoder C.decodeFloat

decodeFloat16Canonical :: VDecoder v s Float
decodeFloat16Canonical = VDecoder C.decodeFloat16Canonical

decodeFloatCanonical :: VDecoder v s Float
decodeFloatCanonical = VDecoder C.decodeFloatCanonical

decodeInt :: VDecoder v s Int
decodeInt = VDecoder C.decodeInt

decodeInt16 :: VDecoder v s Int16
decodeInt16 = VDecoder C.decodeInt16

decodeInt16Canonical :: VDecoder v s Int16
decodeInt16Canonical = VDecoder C.decodeInt16Canonical

decodeInt32 :: VDecoder v s Int32
decodeInt32 = VDecoder C.decodeInt32

decodeInt32Canonical :: VDecoder v s Int32
decodeInt32Canonical = VDecoder C.decodeInt32Canonical

decodeInt64 :: VDecoder v s Int64
decodeInt64 = VDecoder C.decodeInt64

decodeInt64Canonical :: VDecoder v s Int64
decodeInt64Canonical = VDecoder C.decodeInt64Canonical

decodeInt8 :: VDecoder v s Int8
decodeInt8 = VDecoder C.decodeInt8

decodeInt8Canonical :: VDecoder v s Int8
decodeInt8Canonical = VDecoder C.decodeInt8Canonical

decodeIntCanonical :: VDecoder v s Int
decodeIntCanonical = VDecoder C.decodeIntCanonical

decodeInteger :: VDecoder v s Integer
decodeInteger = VDecoder C.decodeInteger

decodeNatural :: VDecoder v s Natural
decodeNatural = do
  !n <- decodeInteger
  if n >= 0
    then return $! fromInteger n
    else cborError $ C.DecoderErrorCustom "Natural" "got a negative number"

decodeIntegerCanonical :: VDecoder v s Integer
decodeIntegerCanonical = VDecoder C.decodeIntegerCanonical

decodeListLen :: VDecoder v s Int
decodeListLen = VDecoder C.decodeListLen

decodeListLenCanonical :: VDecoder v s Int
decodeListLenCanonical = VDecoder C.decodeListLenCanonical

decodeListLenCanonicalOf :: Int -> VDecoder v s ()
decodeListLenCanonicalOf = VDecoder . C.decodeListLenCanonicalOf

decodeListLenIndef :: VDecoder v s ()
decodeListLenIndef = VDecoder C.decodeListLenIndef

decodeListLenOf :: Int -> VDecoder v s ()
decodeListLenOf = VDecoder . C.decodeListLenOf

decodeListLenOrIndef :: VDecoder v s (Maybe Int)
decodeListLenOrIndef = VDecoder C.decodeListLenOrIndef

decodeMapLen :: VDecoder v s Int
decodeMapLen = VDecoder C.decodeMapLen

decodeMapLenCanonical :: VDecoder v s Int
decodeMapLenCanonical = VDecoder C.decodeMapLenCanonical

decodeMapLenIndef :: VDecoder v s ()
decodeMapLenIndef = VDecoder C.decodeMapLenIndef

decodeMapLenOrIndef :: VDecoder v s (Maybe Int)
decodeMapLenOrIndef = VDecoder C.decodeMapLenOrIndef

decodeNegWord :: VDecoder v s Word
decodeNegWord = VDecoder C.decodeNegWord

decodeNegWord64 :: VDecoder v s Word64
decodeNegWord64 = VDecoder C.decodeNegWord64

decodeNegWord64Canonical :: VDecoder v s Word64
decodeNegWord64Canonical = VDecoder C.decodeNegWord64Canonical

decodeNegWordCanonical :: VDecoder v s Word
decodeNegWordCanonical = VDecoder C.decodeNegWordCanonical

decodeNull :: VDecoder v s ()
decodeNull = VDecoder C.decodeNull

decodeSequenceLenIndef ::
  (r -> a1 -> r) -> r -> (r -> a2) -> VDecoder v s a1 -> VDecoder v s a2
decodeSequenceLenIndef a b c (VDecoder d) =
  VDecoder $ C.decodeSequenceLenIndef a b c d

decodeSequenceLenN ::
  (r -> a1 -> r) -> r -> (r -> a2) -> Int -> VDecoder v s a1 -> VDecoder v s a2
decodeSequenceLenN a b c n (VDecoder d) =
  VDecoder $ C.decodeSequenceLenN a b c n d

decodeSimple :: VDecoder v s Word8
decodeSimple = VDecoder C.decodeSimple

decodeSimpleCanonical :: VDecoder v s Word8
decodeSimpleCanonical = VDecoder C.decodeSimpleCanonical

decodeString :: VDecoder v s T.Text
decodeString = VDecoder C.decodeString

decodeStringCanonical :: VDecoder v s T.Text
decodeStringCanonical = VDecoder C.decodeStringCanonical

decodeStringIndef :: VDecoder v s ()
decodeStringIndef = VDecoder C.decodeStringIndef

decodeTag :: VDecoder v s Word
decodeTag = VDecoder C.decodeTag

decodeTag64 :: VDecoder v s Word64
decodeTag64 = VDecoder C.decodeTag64

decodeTag64Canonical :: VDecoder v s Word64
decodeTag64Canonical = VDecoder C.decodeTag64Canonical

decodeTagCanonical :: VDecoder v s Word
decodeTagCanonical = VDecoder C.decodeTagCanonical

decodeUtf8ByteArray :: VDecoder v s ByteArray
decodeUtf8ByteArray = VDecoder C.decodeUtf8ByteArray

decodeUtf8ByteArrayCanonical :: VDecoder v s ByteArray
decodeUtf8ByteArrayCanonical = VDecoder C.decodeUtf8ByteArrayCanonical

decodeWithByteSpan :: VDecoder v1 s a -> VDecoder v s (a, C.ByteOffset, C.ByteOffset)
decodeWithByteSpan (VDecoder d) = VDecoder $ C.decodeWithByteSpan d

decodeWord :: VDecoder v s Word
decodeWord = VDecoder C.decodeWord

decodeWord16 :: VDecoder v s Word16
decodeWord16 = VDecoder C.decodeWord16

decodeWord16Canonical :: VDecoder v s Word16
decodeWord16Canonical = VDecoder C.decodeWord16Canonical

decodeWord32 :: VDecoder v s Word32
decodeWord32 = VDecoder C.decodeWord32

decodeWord32Canonical :: VDecoder v s Word32
decodeWord32Canonical = VDecoder C.decodeWord32Canonical

decodeWord64 :: VDecoder v s Word64
decodeWord64 = VDecoder C.decodeWord64

decodeWord64Canonical :: VDecoder v s Word64
decodeWord64Canonical = VDecoder C.decodeWord64Canonical

decodeWord8 :: VDecoder v s Word8
decodeWord8 = VDecoder C.decodeWord8

decodeWord8Canonical :: VDecoder v s Word8
decodeWord8Canonical = VDecoder C.decodeWord8Canonical

decodeWordCanonical :: VDecoder v s Word
decodeWordCanonical = VDecoder C.decodeWordCanonical

decodeWordCanonicalOf :: Word -> VDecoder v s ()
decodeWordCanonicalOf = VDecoder . C.decodeWordCanonicalOf

decodeWordOf :: Word -> VDecoder v s ()
decodeWordOf = VDecoder . C.decodeWordOf

peekAvailable :: VDecoder v s Int
peekAvailable = VDecoder C.peekAvailable

peekByteOffset :: VDecoder v s C.ByteOffset
peekByteOffset = VDecoder C.peekByteOffset

peekTokenType :: VDecoder v s C.TokenType
peekTokenType = VDecoder C.peekTokenType
