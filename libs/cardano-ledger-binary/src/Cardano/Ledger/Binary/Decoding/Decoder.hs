{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Binary.Decoding.Decoder
  ( -- * Decoders
    Decoder,
    toPlainDecoder,
    fromPlainDecoder,
    withPlainDecoder,
    C.DecoderError (..),
    C.ByteOffset,
    C.DecodeAction (..),
    C.TokenType (..),

    -- ** Versioning
    Version (..),
    getDecoderVersion,
    ifDecoderVersionAtLeast,

    -- * Error reporting
    cborError,

    -- ** Custom decoders
    decodeNullMaybe,
    decodeUTCTime,
    decodeNominalDiffTime,
    decodeVector,
    decodeSet,
    setTag,
    decodeMap,
    decodeVMap,
    decodeSeq,
    decodeStrictSeq,
    decodeMapContents,
    decodeMapNoDuplicates,
    decodeMapTraverse,
    decodeMapContentsTraverse,

    -- ** Lifted @cborg@ decoders
    enforceSize,
    matchSize,
    decodeMaybe,
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
    decodeListWith,
    enforceSize,
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
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Functor.Compose (Compose (..))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), picosecondsToDiffTime)
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import Data.Word (Word16, Word32, Word64, Word8)
import Formatting (build, formatToString)
import GHC.Exts as Exts (IsList (..))
import Numeric.Natural (Natural)
import Prelude hiding (decodeFloat)

--------------------------------------------------------------------------------
-- Versioned Decoder
--------------------------------------------------------------------------------

newtype Version = Version Word
  deriving (Eq, Ord, Show, Num)

newtype Decoder s a = Decoder (ReaderT Version (C.Decoder s) a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadReader Version)

-- | Promote a regular `C.Decoder` to a versioned one. Which measn it will work for all
-- versions.
fromPlainDecoder :: C.Decoder s a -> Decoder s a
fromPlainDecoder d = Decoder (ReaderT (const d))

-- | Extract the underlying `C.Decoder` by specifying the concrete version to be used.
toPlainDecoder :: Version -> Decoder s a -> C.Decoder s a
toPlainDecoder v (Decoder d) = runReaderT d v

-- | Use the supplied decoder as a plain decoder with current version.
withPlainDecoder :: Decoder s a -> (C.Decoder s a -> C.Decoder s b) -> Decoder s b
withPlainDecoder vd f = do
  curVersion <- getDecoderVersion
  fromPlainDecoder (f (toPlainDecoder curVersion vd))

--------------------------------------------------------------------------------
-- Working with current decoder version
--------------------------------------------------------------------------------

-- | Use monadic syntax to extract value level version of the decoder from its type
--
-- >>> decodeFullDecoder 3 "Version" getDecoderVersion ""
-- Right 3
getDecoderVersion :: Decoder s Version
getDecoderVersion = ask

-- | Conditionoly choose the decoder newer or older deceder, depending on the current
-- version. Supplied version acts as a pivot.
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
-- >>> decFoo = Foo <$> ifDecoderVersionAtLeast 2 (fromIntegral <$> decodeWord16) decodeWord32
-- >>> :t decFoo
ifDecoderVersionAtLeast ::
  Version ->
  -- | Use this decoder if current decoder version is larger or equal to the supplied
  -- `Version`
  Decoder s a ->
  -- | Use this decoder if current decoder version is lower than the supplied `Version`
  Decoder s a ->
  Decoder s a
ifDecoderVersionAtLeast atLeast newerDecoder olderDecoder = do
  cur <- getDecoderVersion
  if cur >= atLeast
    then newerDecoder
    else olderDecoder

--------------------------------------------------------------------------------
-- Error reporting
--------------------------------------------------------------------------------

cborError :: C.DecoderError -> Decoder s a
cborError = fail . formatToString build

decodeContainerSkelWithReplicate ::
  -- | How to get the size of the container
  Decoder s Int ->
  -- | replicateM for the container
  (Int -> Decoder s c) ->
  -- | concat for the container
  ([c] -> c) ->
  Decoder s c
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
  forall k m v s.
  Ord k =>
  -- | Decoded list is guaranteed to be sorted on keys in descending order without any
  -- duplicate keys.
  ([(k, v)] -> m) ->
  -- | Decoder for keys
  Decoder s k ->
  -- | Decoder for values
  Decoder s v ->
  Decoder s m
decodeMapSkel fromDistinctDescList decodeKey decodeValue = do
  n <- decodeMapLen
  fromDistinctDescList <$> case n of
    0 -> return []
    _ -> do
      (firstKey, firstValue) <- decodeEntry
      decodeEntries (n - 1) firstKey [(firstKey, firstValue)]
  where
    -- Decode a single (k,v).
    decodeEntry :: Decoder s (k, v)
    decodeEntry = do
      !k <- decodeKey
      !v <- decodeValue
      return (k, v)

    -- Decode all the entries, enforcing canonicity by ensuring that the
    -- previous key is smaller than the next one.
    decodeEntries :: Int -> k -> [(k, v)] -> Decoder s [(k, v)]
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
  forall k v s.
  Ord k =>
  -- | Decoder for keys
  Decoder s k ->
  -- | Decoder for values
  Decoder s v ->
  Decoder s (Map.Map k v)
decodeMapV1 = decodeMapSkel Map.fromDistinctDescList

-- decodeMapContents :: Decoder s a -> Decoder s [a]
-- decodeMapContents = decodeCollection decodeMapLenOrIndef

decodeCollection :: Decoder s (Maybe Int) -> Decoder s a -> Decoder s [a]
decodeCollection lenOrIndef el = snd <$> decodeCollectionWithLen lenOrIndef el

decodeCollectionWithLen ::
  Decoder s (Maybe Int) ->
  Decoder s v ->
  Decoder s (Int, [v])
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
  forall k t v s.
  (Exts.IsList t, Exts.Item t ~ (k, v)) =>
  -- | Decoder for keys
  Decoder s k ->
  -- | Decoder for values
  (k -> Decoder s v) ->
  Decoder s t
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
  forall k v s.
  Ord k =>
  -- | Decoder for keys
  Decoder s k ->
  -- | Decoder for values
  Decoder s v ->
  Decoder s (Map.Map k v)
decodeMapV2 decodeKey decodeValue = decodeMapByKey decodeKey (const decodeValue)

-- | An example of how to use versioning
--
-- >>> :set -XOverloadedStrings
-- >>> import Codec.CBOR.FlatTerm
-- >>> fromFlatTerm (toPlainDecoder 1 (decodeMap decodeInt decodeBytes)) [TkMapLen 2,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar"]
-- Right (fromList [(1,"Foo"),(2,"Bar")])
-- >>> fromFlatTerm (toPlainDecoder 2 (decodeMap decodeInt decodeBytes)) [TkMapBegin,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar"]
-- Left "decodeMapLen: unexpected token TkMapBegin"
-- >>> fromFlatTerm (toPlainDecoder 2 (decodeMap decodeInt decodeBytes)) [TkMapBegin,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar",TkBreak]
-- Right (fromList [(1,"Foo"),(2,"Bar")])
decodeMap ::
  Ord k =>
  Decoder s k ->
  Decoder s v ->
  Decoder s (Map.Map k v)
decodeMap decodeKey decodeValue =
  ifDecoderVersionAtLeast
    2
    (decodeMapV2 decodeKey decodeValue)
    (decodeMapV1 decodeKey decodeValue)

-- encodeMap ::
--   Ord k =>
--   Map.Map k a ->
--   (k -> Encoder) ->
--   (v -> Encoder) ->
--   VEncoder
-- encodeMap m encodeKey encodeValue =
--   ifEncVerAtLeast 2
--     (encodeMapV2 m encodeKey encodeValue)
--     (encodeMapV1 m encodeKey encodeValue)

decodeVMap ::
  (VMap.Vector kv k, VMap.Vector vv v, Ord k) =>
  Decoder s k ->
  Decoder s v ->
  Decoder s (VMap.VMap kv vv k v)
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

decodeSetTag :: Decoder s ()
decodeSetTag = do
  t <- decodeTag
  when (t /= setTag) $ cborError $ C.DecoderErrorUnknownTag "Set" (fromIntegral t)

decodeSetSkel ::
  forall a s c.
  Ord a =>
  -- | Decoded list is guaranteed to be sorted on keys in descending order without any
  -- duplicate keys.
  ([a] -> c) ->
  Decoder s a ->
  Decoder s c
decodeSetSkel fromDistinctDescList decodeValue = do
  decodeSetTag
  n <- decodeListLen
  fromDistinctDescList <$> case n of
    0 -> return []
    _ -> do
      firstValue <- decodeValue
      decodeEntries (n - 1) firstValue [firstValue]
  where
    decodeEntries :: Int -> a -> [a] -> Decoder s [a]
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

decodeSet :: Ord a => Decoder s a -> Decoder s (Set.Set a)
decodeSet valueDecoder =
  ifDecoderVersionAtLeast
    2
    (Set.fromList <$> decodeList valueDecoder)
    (decodeSetSkel Set.fromDistinctDescList valueDecoder)

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: VG.Vector vec a => Decoder s a -> Decoder s (vec a)
decodeVector decodeValue =
  decodeContainerSkelWithReplicate
    decodeListLen
    (`VG.replicateM` decodeValue)
    VG.concat
{-# INLINE decodeVector #-}

decodeSeq :: Decoder s a -> Decoder s (Seq.Seq a)
decodeSeq decoder = Seq.fromList <$> decodeList decoder

decodeStrictSeq :: Decoder s a -> Decoder s (StrictSeq.StrictSeq a)
decodeStrictSeq decoder = StrictSeq.fromList <$> decodeList decoder

decodeAccWithLen ::
  Decoder s (Maybe Int) ->
  (a -> b -> b) ->
  b ->
  Decoder s a ->
  Decoder s (Int, b)
decodeAccWithLen lenOrIndef combine acc0 action = do
  mLen <- lenOrIndef
  let condition = case mLen of
        Nothing -> const <$> decodeBreakOr
        Just len -> pure (>= len)
      loop !i !acc = do
        shouldStop <- condition
        if shouldStop i
          then pure (i, acc)
          else do
            v <- action
            loop (i + 1) (v `combine` acc)
  loop 0 acc0

-- | Just like `decodeMap`, but assumes that there are no duplicate keys
decodeMapNoDuplicates :: Ord a => Decoder s a -> Decoder s b -> Decoder s (Map.Map a b)
decodeMapNoDuplicates decodeKey decodeValue =
  snd
    <$> decodeAccWithLen
      decodeMapLenOrIndef
      (uncurry Map.insert)
      Map.empty
      decodeInlinedPair
  where
    decodeInlinedPair = do
      !key <- decodeKey
      !value <- decodeValue
      pure (key, value)

decodeMapContents :: Decoder s a -> Decoder s [a]
decodeMapContents = decodeCollection decodeMapLenOrIndef

decodeMapTraverse ::
  (Ord a, Applicative t) =>
  Decoder s (t a) ->
  Decoder s (t b) ->
  Decoder s (t (Map.Map a b))
decodeMapTraverse decodeKey decodeValue =
  fmap Map.fromList <$> decodeMapContentsTraverse decodeKey decodeValue

decodeMapContentsTraverse ::
  Applicative t =>
  Decoder s (t a) ->
  Decoder s (t b) ->
  Decoder s (t [(a, b)])
decodeMapContentsTraverse decodeKey decodeValue =
  sequenceA <$> decodeMapContents decodeInlinedPair
  where
    decodeInlinedPair = getCompose $ (,) <$> Compose decodeKey <*> Compose decodeValue

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

decodeUTCTime :: Decoder s UTCTime
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
decodeNominalDiffTime :: Decoder s NominalDiffTime
decodeNominalDiffTime = fromRational . (% 1_000_000) <$> decodeInteger

--------------------------------------------------------------------------------
-- Promoted CBORG primitive decoders
--------------------------------------------------------------------------------

-- | Enforces that the input size is the same as the decoded one, failing in
--   case it's not
enforceSize :: T.Text -> Int -> Decoder s ()
enforceSize lbl requestedSize = fromPlainDecoder (C.enforceSize lbl requestedSize)

-- | Compare two sizes, failing if they are not equal
matchSize :: T.Text -> Int -> Int -> Decoder s ()
matchSize lbl requestedSize actualSize = fromPlainDecoder (C.matchSize lbl requestedSize actualSize)

-- | @'D.Decoder'@ for list.
decodeList :: Decoder s a -> Decoder s [a]
decodeList decodeValue =
  ifDecoderVersionAtLeast
    2
    (decodeCollection decodeListLenOrIndef decodeValue)
    (withPlainDecoder decodeValue C.decodeListWith)

decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe decodeValue = do
  n <- decodeListLen
  case n of
    0 -> return Nothing
    1 -> do
      !x <- decodeValue
      return (Just x)
    _ -> cborError $ C.DecoderErrorUnknownTag "Maybe" (fromIntegral n)

decodeNullMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeNullMaybe decoder = do
  peekTokenType >>= \case
    C.TypeNull -> do
      decodeNull
      pure Nothing
    _ -> Just <$> decoder

decodeBool :: Decoder s Bool
decodeBool = fromPlainDecoder C.decodeBool

decodeBreakOr :: Decoder s Bool
decodeBreakOr = fromPlainDecoder C.decodeBreakOr

decodeByteArray :: Decoder s ByteArray
decodeByteArray = fromPlainDecoder C.decodeByteArray

decodeByteArrayCanonical :: Decoder s ByteArray
decodeByteArrayCanonical = fromPlainDecoder C.decodeByteArrayCanonical

decodeBytes :: Decoder s BS.ByteString
decodeBytes = fromPlainDecoder C.decodeBytes

decodeBytesCanonical :: Decoder s BS.ByteString
decodeBytesCanonical = fromPlainDecoder C.decodeBytesCanonical

decodeBytesIndef :: Decoder s ()
decodeBytesIndef = fromPlainDecoder C.decodeBytesIndef

decodeDouble :: Decoder s Double
decodeDouble = fromPlainDecoder C.decodeDouble

decodeDoubleCanonical :: Decoder s Double
decodeDoubleCanonical = fromPlainDecoder C.decodeDoubleCanonical

decodeFloat :: Decoder s Float
decodeFloat = fromPlainDecoder C.decodeFloat

decodeFloat16Canonical :: Decoder s Float
decodeFloat16Canonical = fromPlainDecoder C.decodeFloat16Canonical

decodeFloatCanonical :: Decoder s Float
decodeFloatCanonical = fromPlainDecoder C.decodeFloatCanonical

decodeInt :: Decoder s Int
decodeInt = fromPlainDecoder C.decodeInt

decodeInt16 :: Decoder s Int16
decodeInt16 = fromPlainDecoder C.decodeInt16

decodeInt16Canonical :: Decoder s Int16
decodeInt16Canonical = fromPlainDecoder C.decodeInt16Canonical

decodeInt32 :: Decoder s Int32
decodeInt32 = fromPlainDecoder C.decodeInt32

decodeInt32Canonical :: Decoder s Int32
decodeInt32Canonical = fromPlainDecoder C.decodeInt32Canonical

decodeInt64 :: Decoder s Int64
decodeInt64 = fromPlainDecoder C.decodeInt64

decodeInt64Canonical :: Decoder s Int64
decodeInt64Canonical = fromPlainDecoder C.decodeInt64Canonical

decodeInt8 :: Decoder s Int8
decodeInt8 = fromPlainDecoder C.decodeInt8

decodeInt8Canonical :: Decoder s Int8
decodeInt8Canonical = fromPlainDecoder C.decodeInt8Canonical

decodeIntCanonical :: Decoder s Int
decodeIntCanonical = fromPlainDecoder C.decodeIntCanonical

decodeInteger :: Decoder s Integer
decodeInteger = fromPlainDecoder C.decodeInteger

decodeNatural :: Decoder s Natural
decodeNatural = do
  !n <- decodeInteger
  if n >= 0
    then return $! fromInteger n
    else cborError $ C.DecoderErrorCustom "Natural" "got a negative number"

decodeIntegerCanonical :: Decoder s Integer
decodeIntegerCanonical = fromPlainDecoder C.decodeIntegerCanonical

decodeListLen :: Decoder s Int
decodeListLen = fromPlainDecoder C.decodeListLen

decodeListLenCanonical :: Decoder s Int
decodeListLenCanonical = fromPlainDecoder C.decodeListLenCanonical

decodeListLenCanonicalOf :: Int -> Decoder s ()
decodeListLenCanonicalOf = fromPlainDecoder . C.decodeListLenCanonicalOf

decodeListLenIndef :: Decoder s ()
decodeListLenIndef = fromPlainDecoder C.decodeListLenIndef

decodeListLenOf :: Int -> Decoder s ()
decodeListLenOf = fromPlainDecoder . C.decodeListLenOf

decodeListLenOrIndef :: Decoder s (Maybe Int)
decodeListLenOrIndef = fromPlainDecoder C.decodeListLenOrIndef

decodeMapLen :: Decoder s Int
decodeMapLen = fromPlainDecoder C.decodeMapLen

decodeMapLenCanonical :: Decoder s Int
decodeMapLenCanonical = fromPlainDecoder C.decodeMapLenCanonical

decodeMapLenIndef :: Decoder s ()
decodeMapLenIndef = fromPlainDecoder C.decodeMapLenIndef

decodeMapLenOrIndef :: Decoder s (Maybe Int)
decodeMapLenOrIndef = fromPlainDecoder C.decodeMapLenOrIndef

decodeNegWord :: Decoder s Word
decodeNegWord = fromPlainDecoder C.decodeNegWord

decodeNegWord64 :: Decoder s Word64
decodeNegWord64 = fromPlainDecoder C.decodeNegWord64

decodeNegWord64Canonical :: Decoder s Word64
decodeNegWord64Canonical = fromPlainDecoder C.decodeNegWord64Canonical

decodeNegWordCanonical :: Decoder s Word
decodeNegWordCanonical = fromPlainDecoder C.decodeNegWordCanonical

decodeNull :: Decoder s ()
decodeNull = fromPlainDecoder C.decodeNull

decodeSequenceLenIndef :: (r -> a -> r) -> r -> (r -> b) -> Decoder s a -> Decoder s b
decodeSequenceLenIndef a b c dec =
  withPlainDecoder dec $ C.decodeSequenceLenIndef a b c

decodeSequenceLenN :: (r -> a -> r) -> r -> (r -> b) -> Int -> Decoder s a -> Decoder s b
decodeSequenceLenN a b c n dec =
  withPlainDecoder dec $ C.decodeSequenceLenN a b c n

decodeSimple :: Decoder s Word8
decodeSimple = fromPlainDecoder C.decodeSimple

decodeSimpleCanonical :: Decoder s Word8
decodeSimpleCanonical = fromPlainDecoder C.decodeSimpleCanonical

decodeString :: Decoder s T.Text
decodeString = fromPlainDecoder C.decodeString

decodeStringCanonical :: Decoder s T.Text
decodeStringCanonical = fromPlainDecoder C.decodeStringCanonical

decodeStringIndef :: Decoder s ()
decodeStringIndef = fromPlainDecoder C.decodeStringIndef

decodeTag :: Decoder s Word
decodeTag = fromPlainDecoder C.decodeTag

decodeTag64 :: Decoder s Word64
decodeTag64 = fromPlainDecoder C.decodeTag64

decodeTag64Canonical :: Decoder s Word64
decodeTag64Canonical = fromPlainDecoder C.decodeTag64Canonical

decodeTagCanonical :: Decoder s Word
decodeTagCanonical = fromPlainDecoder C.decodeTagCanonical

decodeUtf8ByteArray :: Decoder s ByteArray
decodeUtf8ByteArray = fromPlainDecoder C.decodeUtf8ByteArray

decodeUtf8ByteArrayCanonical :: Decoder s ByteArray
decodeUtf8ByteArrayCanonical = fromPlainDecoder C.decodeUtf8ByteArrayCanonical

decodeWithByteSpan :: Decoder s a -> Decoder s (a, C.ByteOffset, C.ByteOffset)
decodeWithByteSpan d = withPlainDecoder d C.decodeWithByteSpan

decodeWord :: Decoder s Word
decodeWord = fromPlainDecoder C.decodeWord

decodeWord16 :: Decoder s Word16
decodeWord16 = fromPlainDecoder C.decodeWord16

decodeWord16Canonical :: Decoder s Word16
decodeWord16Canonical = fromPlainDecoder C.decodeWord16Canonical

decodeWord32 :: Decoder s Word32
decodeWord32 = fromPlainDecoder C.decodeWord32

decodeWord32Canonical :: Decoder s Word32
decodeWord32Canonical = fromPlainDecoder C.decodeWord32Canonical

decodeWord64 :: Decoder s Word64
decodeWord64 = fromPlainDecoder C.decodeWord64

decodeWord64Canonical :: Decoder s Word64
decodeWord64Canonical = fromPlainDecoder C.decodeWord64Canonical

decodeWord8 :: Decoder s Word8
decodeWord8 = fromPlainDecoder C.decodeWord8

decodeWord8Canonical :: Decoder s Word8
decodeWord8Canonical = fromPlainDecoder C.decodeWord8Canonical

decodeWordCanonical :: Decoder s Word
decodeWordCanonical = fromPlainDecoder C.decodeWordCanonical

decodeWordCanonicalOf :: Word -> Decoder s ()
decodeWordCanonicalOf = fromPlainDecoder . C.decodeWordCanonicalOf

decodeWordOf :: Word -> Decoder s ()
decodeWordOf = fromPlainDecoder . C.decodeWordOf

peekAvailable :: Decoder s Int
peekAvailable = fromPlainDecoder C.peekAvailable

peekByteOffset :: Decoder s C.ByteOffset
peekByteOffset = fromPlainDecoder C.peekByteOffset

peekTokenType :: Decoder s C.TokenType
peekTokenType = fromPlainDecoder C.peekTokenType
