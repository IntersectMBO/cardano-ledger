{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.Binary.Decoding.Decoder (
  -- * Decoders
  Decoder,
  toPlainDecoder,
  fromPlainDecoder,
  withPlainDecoder,
  enforceDecoderVersion,
  DecoderError (..),
  C.ByteOffset,
  C.DecodeAction (..),
  C.TokenType (..),

  -- ** Versioning
  getDecoderVersion,
  ifDecoderVersionAtLeast,
  whenDecoderVersionAtLeast,

  -- ** Error reporting
  cborError,
  toCborError,
  showDecoderError,
  invalidKey,
  assertTag,
  enforceSize,
  matchSize,

  -- ** Compatibility tools
  binaryGetDecoder,

  -- ** Custom decoders
  decodeVersion,
  decodeRational,
  decodeRationalWithTag,
  decodeRecordNamed,
  decodeRecordNamedT,
  decodeRecordSum,
  decodeEnumBounded,

  -- *** Containers
  decodeMaybe,
  decodeEither,
  decodeList,
  decodeNullMaybe,
  decodeVector,
  decodeSet,
  setTag,
  decodeMap,
  decodeMapByKey,
  decodeVMap,
  decodeSeq,
  decodeStrictSeq,

  -- *** Time
  decodeUTCTime,

  -- *** Network
  decodeIPv4,
  decodeIPv6,
  decodeMapContents,
  decodeMapNoDuplicates,
  decodeMapTraverse,
  decodeMapContentsTraverse,

  -- ** Lifted @cborg@ decoders
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
  decodeTerm,
  peekAvailable,
  peekByteOffset,
  peekTokenType,
)
where

import Cardano.Ledger.Binary.Plain (DecoderError (..), cborError, invalidKey, toCborError)
import Cardano.Ledger.Binary.Version
import Codec.CBOR.ByteArray (ByteArray)
import qualified Codec.CBOR.Decoding as C (
  ByteOffset,
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
import qualified Codec.CBOR.Term as C (Term (..), decodeTerm)
import Control.Monad
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Identity (IdentityT (runIdentityT))
import Data.Binary.Get (Get, getWord32le, runGetOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Compose (Compose (..))
import Data.IP (IPv4, IPv6, fromHostAddress, fromHostAddress6)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (UTCTime (..), picosecondsToDiffTime)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import Data.Word (Word16, Word32, Word64, Word8)
import Formatting (build, formatToString)
import qualified Formatting.Buildable as B (Buildable (..))
import GHC.Exts as Exts (IsList (..))
import Network.Socket (HostAddress6)
import Numeric.Natural (Natural)
import Prelude hiding (decodeFloat)

--------------------------------------------------------------------------------
-- Versioned Decoder
--------------------------------------------------------------------------------

newtype Decoder s a = Decoder
  { runDecoder :: Version -> C.Decoder s a
  }

instance Functor (Decoder s) where
  fmap f (Decoder d) = Decoder (fmap f . d)
  {-# INLINE fmap #-}

instance Applicative (Decoder s) where
  pure x = Decoder (const (pure x))
  {-# INLINE pure #-}
  Decoder f <*> Decoder g = Decoder $ \v -> f v <*> g v
  {-# INLINE (<*>) #-}
  Decoder f *> Decoder g = Decoder $ \v -> f v *> g v
  {-# INLINE (*>) #-}

instance Monad (Decoder s) where
  Decoder f >>= g = Decoder $ \v -> do
    x <- f v
    runDecoder (g x) v
  {-# INLINE (>>=) #-}

instance MonadFail (Decoder s) where
  fail msg = fromPlainDecoder $ fail msg
  {-# INLINE fail #-}

-- | Promote a regular `C.Decoder` to a versioned one. Which means it will work for all
-- versions.
fromPlainDecoder :: C.Decoder s a -> Decoder s a
fromPlainDecoder d = Decoder (const d)

-- | Extract the underlying `C.Decoder` by specifying the concrete version to be used.
toPlainDecoder :: Version -> Decoder s a -> C.Decoder s a
toPlainDecoder v (Decoder d) = d v

-- | Use the supplied decoder as a plain decoder with current version.
withPlainDecoder :: Decoder s a -> (C.Decoder s a -> C.Decoder s b) -> Decoder s b
withPlainDecoder vd f = Decoder $ \curVersion -> f (toPlainDecoder curVersion vd)

-- | Ignore the current version of the decoder and enforce the supplied one instead.
enforceDecoderVersion :: Version -> Decoder s a -> Decoder s a
enforceDecoderVersion version = fromPlainDecoder . toPlainDecoder version

--------------------------------------------------------------------------------
-- Working with current decoder version
--------------------------------------------------------------------------------

-- | Extract current version of the decoder
--
-- >>> import Cardano.Ledger.Decoding
-- >>> decodeFullDecoder 3 "Version" getDecoderVersion ""
-- Right 3
getDecoderVersion :: Decoder s Version
getDecoderVersion = Decoder pure

-- | Conditionally choose the newer or older decoder, depending on the current
-- version. Version in the context of encoders/decoders is the major protocol
-- version. Supplied version acts as a pivot.
--
-- =====__Example__
--
-- Let's say prior to the version 2 some type `Foo` was backed by `Word16`, but at the 2nd
-- version onwards it was switched to `Word32` instead. In order to support both versions,
-- we change the type, but we also use this condition to keep backwards compatibility of
-- the decoder:
--
-- >>> newtype Foo = Foo Word32
-- >>> decFoo = Foo <$> ifDecoderVersionAtLeast 2 decodeWord32 (fromIntegral <$> decodeWord16)
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

-- | Optionally run a decoder depending on the current version and the supplied one.
whenDecoderVersionAtLeast ::
  Version ->
  -- | Run this decoder whenever current decoder version is larger or equal to the supplied
  -- `Version`
  Decoder s a ->
  Decoder s ()
whenDecoderVersionAtLeast atLeast decoder = do
  cur <- getDecoderVersion
  when (cur >= atLeast) (void decoder)

--------------------------------------------------------------------------------
-- Error reporting
--------------------------------------------------------------------------------

showDecoderError :: B.Buildable e => e -> String
showDecoderError = formatToString build

decodeVersion :: Decoder s Version
decodeVersion = decodeWord64 >>= mkVersion64

-- | `Decoder` for `Rational`. Versions variance:
--
-- * [>= 2] - Allows variable as well as exact list length encoding.
--
-- * [== 1] - Expects exact list length encoding.
decodeRational :: Decoder s Rational
decodeRational =
  ifDecoderVersionAtLeast
    (natVersion @2)
    decodeRationalWithoutTag
    decodeRationalFixedSizeTuple
  where
    decodeRationalFixedSizeTuple = do
      enforceSize "Rational" 2
      n <- decodeInteger
      d <- decodeInteger
      if d <= 0
        then cborError $ DecoderErrorCustom "Rational" "invalid denominator"
        else return $! n % d

-- | Future `Decoder` for `Rational` type. This decoder will be applied in future and is
-- prepared here as use case on how to do upgrades to serialization. Versions variance:
--
-- * [>= 10] - Enforces tag 30
--
-- * [>= 9] - Allows variable as well as exact list length encoding. Consumes tag 30 if
--   one is present, but does not enforce it.
--
-- * [>= 2] - Allows variable as well as exact list length encoding.
--
-- * [== 1] - Expects exact list length encoding.
_decodeRationalFuture :: Decoder s Rational
_decodeRationalFuture = do
  -- We are not using `natVersion` because these versions aren't yet supported.
  v9 <- mkVersion 9
  v10 <- mkVersion 10
  ifDecoderVersionAtLeast
    v10
    decodeRationalWithTag
    ( ifDecoderVersionAtLeast
        v9
        (allowTag 30 >> decodeRational)
        decodeRational
    )

-- | Enforces tag 30 to indicate a rational number, as per tag assignment:
-- <https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml>
--
-- <https://peteroupc.github.io/CBOR/rational.html>
decodeRationalWithTag :: Decoder s Rational
decodeRationalWithTag = assertTag 30 >> decodeRationalWithoutTag

decodeRationalWithoutTag :: Decoder s Rational
decodeRationalWithoutTag = do
  (numValues, values) <- decodeCollectionWithLen decodeListLenOrIndef decodeInteger
  case values of
    [n, d] -> do
      when (d == 0) (fail "Denominator cannot be zero")
      pure $! n % d
    _ -> cborError $ DecoderErrorSizeMismatch "Rational" 2 numValues

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

-- | @'Decoder'@ for list.
--
-- * [>= 2] - Allows variable as well as exact list length encoding.
--
-- * [< 2] - Expects variable list length encoding
decodeList :: Decoder s a -> Decoder s [a]
decodeList decodeValue =
  ifDecoderVersionAtLeast
    (natVersion @2)
    (decodeCollection decodeListLenOrIndef decodeValue)
    (decodeListWith decodeValue)

-- | @'Decoder'@ for list.
decodeListWith :: Decoder s a -> Decoder s [a]
decodeListWith decodeValue = do
  decodeListLenIndef
  decodeSequenceLenIndef (flip (:)) [] reverse decodeValue

-- | `Decoder` for `Maybe`. Versions variance:
--
-- * [>= 2] - Allows variable as well as exact list length encoding.
--
-- * [< 2] - Expects exact list length encoding
decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe decodeValue = do
  ifDecoderVersionAtLeast
    (natVersion @2)
    (decodeMaybeVarLen decodeValue)
    (decodeMaybeExactLen decodeValue)

decodeMaybeExactLen :: Decoder s a -> Decoder s (Maybe a)
decodeMaybeExactLen decodeValue = do
  n <- decodeListLen
  case n of
    0 -> return Nothing
    1 -> do
      !x <- decodeValue
      return (Just x)
    _ -> fail "too many elements while decoding Maybe."

decodeMaybeVarLen :: Decoder s a -> Decoder s (Maybe a)
decodeMaybeVarLen decodeValue = do
  maybeLength <- decodeListLenOrIndef
  case maybeLength of
    Just 0 -> pure Nothing
    Just 1 -> Just <$!> decodeValue
    Just _ -> fail "too many elements in length-style decoding of Maybe."
    Nothing -> do
      isBreak <- decodeBreakOr
      if isBreak
        then pure Nothing
        else do
          !x <- decodeValue
          isBreak2 <- decodeBreakOr
          unless isBreak2 $
            fail "too many elements in break-style decoding of Maybe."
          pure (Just x)

-- | Alternative way to decode a Maybe type.
--
-- /Note/ - this is not the default method for decoding `Maybe`, use `decodeMaybe` instead.
decodeNullMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeNullMaybe decoder = do
  peekTokenType >>= \case
    C.TypeNull -> do
      decodeNull
      pure Nothing
    _ -> Just <$> decoder

decodeEither :: Decoder s a -> Decoder s b -> Decoder s (Either a b)
decodeEither decodeLeft decodeRight = do
  decodeListLenOf 2
  t <- decodeWord
  case t of
    0 -> Left <$> decodeLeft
    1 -> Right <$> decodeRight
    _ -> cborError $ DecoderErrorUnknownTag "Either" (fromIntegral t)

decodeRecordNamed :: Text.Text -> (a -> Int) -> Decoder s a -> Decoder s a
decodeRecordNamed name getRecordSize decoder = do
  runIdentityT $ decodeRecordNamedT name getRecordSize (lift decoder)

decodeRecordNamedT ::
  (MonadTrans m, Monad (m (Decoder s))) =>
  Text.Text ->
  (a -> Int) ->
  m (Decoder s) a ->
  m (Decoder s) a
decodeRecordNamedT name getRecordSize decoder = do
  lenOrIndef <- lift decodeListLenOrIndef
  x <- decoder
  lift $ case lenOrIndef of
    Just n -> matchSize (Text.pack "Record " <> name) n (getRecordSize x)
    Nothing -> do
      isBreak <- decodeBreakOr
      unless isBreak $ cborError $ DecoderErrorCustom name "Excess terms in array"
  pure x

decodeRecordSum :: String -> (Word -> Decoder s (Int, a)) -> Decoder s a
decodeRecordSum name decoder = do
  lenOrIndef <- decodeListLenOrIndef
  tag <- decodeWord
  (size, x) <- decoder tag -- we decode all the stuff we want
  case lenOrIndef of
    Just n -> matchSize (Text.pack "Sum " <> Text.pack name) size n
    Nothing -> do
      isBreak <- decodeBreakOr -- if there is stuff left, it is unnecessary extra stuff
      unless isBreak $ cborError $ DecoderErrorCustom (Text.pack name) "Excess terms in array"
  pure x

decodeEnumBounded :: forall a s. (Enum a, Bounded a, Typeable a) => Decoder s a
decodeEnumBounded = do
  n <- decodeInt
  if fromEnum (minBound :: a) <= n && n <= fromEnum (maxBound :: a)
    then pure $ toEnum n
    else fail $ "Failed to decode an Enum: " <> show n <> " for TypeRep: " <> show (typeRep (Proxy @a))

--------------------------------------------------------------------------------
-- Decoder for Map
--------------------------------------------------------------------------------

-- | Checks canonicity by comparing the new key being decoded with
--   the previous one, to enfore these are sorted the correct way.
--   See: https://tools.ietf.org/html/rfc7049#section-3.9
--   "[..]The keys in every map must be sorted lowest value to highest.[...]"
--
-- In other words this decoder enforces strict monotonically increasing order on keys. It
-- also uses exact map length encoding.
decodeMapSkel ::
  forall k m v s.
  Ord k =>
  -- | Decoded list is guaranteed to be sorted on keys in descending order without any
  -- duplicate keys.
  ([(k, v)] -> m) ->
  -- | Decoder for keys
  Decoder s k ->
  -- | Decoder for values
  (k -> Decoder s v) ->
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
      !v <- decodeValue k
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
        else cborError $ DecoderErrorCanonicityViolation "Map"
{-# INLINE decodeMapSkel #-}

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

decodeIsListByKey ::
  forall k t v s.
  (Exts.IsList t, Exts.Item t ~ (k, v)) =>
  -- | Decoder for keys
  Decoder s k ->
  -- | Decoder for values
  (k -> Decoder s v) ->
  Decoder s t
decodeIsListByKey decodeKey decodeValueFor =
  uncurry Exts.fromListN
    <$> decodeCollectionWithLen decodeMapLenOrIndef decodeInlinedPair
  where
    decodeInlinedPair = do
      !key <- decodeKey
      !value <- decodeValueFor key
      pure (key, value)

-- | `Decoder` for `Map.Map`. Versions variance:
--
-- * [>= 2] - Allows variable as well as exact list length encoding. Duplicates are
--   silently ignored
--
-- * [< 2] - Expects exact list length encoding and enforces strict order
--   without any duplicates.
--
-- An example of how to use versioning
--
-- >>> :set -XOverloadedStrings
-- >>> import Codec.CBOR.FlatTerm
-- >>> fromFlatTerm (toPlainDecoder 1 (decodeMap decodeInt decodeBytes)) [TkMapLen 2,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar"]
-- Right (fromList [(1,"Foo"),(2,"Bar")])
-- >>> fromFlatTerm (toPlainDecoder 1 (decodeMap decodeInt decodeBytes)) [TkMapBegin,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar"]
-- Left "decodeMapLen: unexpected token TkMapBegin"
-- >>> fromFlatTerm (toPlainDecoder 2 (decodeMap decodeInt decodeBytes)) [TkMapBegin,TkInt 1,TkBytes "Foo",TkInt 2,TkBytes "Bar",TkBreak]
-- Right (fromList [(1,"Foo"),(2,"Bar")])
decodeMap ::
  Ord k =>
  Decoder s k ->
  Decoder s v ->
  Decoder s (Map.Map k v)
decodeMap decodeKey decodeValue = decodeMapByKey decodeKey (const decodeValue)

-- | Just like `decodeMap`, but also gives access to the key for the value decoder.
decodeMapByKey ::
  Ord k =>
  Decoder s k ->
  (k -> Decoder s v) ->
  Decoder s (Map.Map k v)
decodeMapByKey decodeKey decodeValue =
  ifDecoderVersionAtLeast
    (natVersion @2)
    (decodeIsListByKey decodeKey decodeValue)
    (decodeMapSkel Map.fromDistinctDescList decodeKey decodeValue)

-- | Decode `VMap`. Unlike `decodeMap` it does not behavee differently for
-- version prior to 2.
decodeVMap ::
  (VMap.Vector kv k, VMap.Vector vv v, Ord k) =>
  Decoder s k ->
  Decoder s v ->
  Decoder s (VMap.VMap kv vv k v)
decodeVMap decodeKey decodeValue = decodeIsListByKey decodeKey (const decodeValue)

-- | We stitch a `258` in from of a (Hash)Set, so that tools which
-- programmatically check for canonicity can recognise it from a normal
-- array. Why 258? This will be formalised pretty soon, but IANA allocated
-- 256...18446744073709551615 to "First come, first served":
-- https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml Currently `258` is
-- the first unassigned tag and as it requires 2 bytes to be encoded, it sounds
-- like the best fit.
--
-- <https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md>
setTag :: Word
setTag = 258

decodeSetTag :: Decoder s ()
decodeSetTag = do
  t <- decodeTag
  when (t /= setTag) $ cborError $ DecoderErrorUnknownTag "Set" (fromIntegral t)

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
        else cborError $ DecoderErrorCanonicityViolation "Set"
{-# INLINE decodeSetSkel #-}

-- | `Decoder` for `Set.Set`. Versions variance:
--
-- * [>= 2] - Allows variable as well as exact list length encoding. Duplicates are
--   silently ignored
--
-- * [< 2] - Expects exact list length encoding and enforces strict order
--   without any duplicates. Also enforces special set tag 258, which was
--   abandoned starting with version 2
decodeSet :: Ord a => Decoder s a -> Decoder s (Set.Set a)
decodeSet valueDecoder =
  ifDecoderVersionAtLeast
    (natVersion @2)
    (Set.fromList <$> decodeCollection decodeListLenOrIndef valueDecoder)
    (decodeSetSkel Set.fromDistinctDescList valueDecoder)

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

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: VG.Vector vec a => Decoder s a -> Decoder s (vec a)
decodeVector decodeValue =
  decodeContainerSkelWithReplicate
    decodeListLen
    (`VG.replicateM` decodeValue)
    VG.concat
{-# INLINE decodeVector #-}

-- | Decoder for `Seq.Seq`. Same behavior for all versions, allows variable as
-- well as exact list length encoding
decodeSeq :: Decoder s a -> Decoder s (Seq.Seq a)
decodeSeq decoder = Seq.fromList <$> decodeCollection decodeListLenOrIndef decoder

-- | Decoder for `SSeq.StrictSeq`. Same behavior for all versions, allows variable as
-- well as exact list length encoding.
decodeStrictSeq :: Decoder s a -> Decoder s (SSeq.StrictSeq a)
decodeStrictSeq decoder = SSeq.fromList <$> decodeCollection decodeListLenOrIndef decoder

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

-- | Just like `decodeMap`, but assumes that there are no duplicate keys, which is not enforced.
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

-- | `Decoder` for `UTCTime`. Versions variance:
--
-- * [>= 2] - Allows variable list length encoding, but still expects number of
--   elements to be 3.
--
-- * [< 2] - Expects exact list length encoding to be 3
decodeUTCTime :: Decoder s UTCTime
decodeUTCTime =
  ifDecoderVersionAtLeast
    (natVersion @2)
    (enforceSize "UTCTime" 3 >> timeDecoder)
    (decodeRecordNamed "UTCTime" (const 3) timeDecoder)
  where
    timeDecoder = do
      !year <- decodeInteger
      !dayOfYear <- decodeInt
      !timeOfDayPico <- decodeInteger
      return $!
        UTCTime
          (fromOrdinalDate year dayOfYear)
          (picosecondsToDiffTime timeOfDayPico)

--------------------------------------------------------------------------------
-- Network
--------------------------------------------------------------------------------

-- | Convert a `Get` monad from @binary@ package into a `Decoder`
binaryGetDecoder ::
  -- | Flag to allow left over at the end or not
  Bool ->
  -- | Name of the function or type for error reporting
  Text.Text ->
  -- | Deserializer for the @binary@ package
  Get a ->
  Decoder s a
binaryGetDecoder allowLeftOver name getter = do
  bs <- decodeBytes
  case runGetOrFail getter (BSL.fromStrict bs) of
    Left (_, _, err) -> cborError $ DecoderErrorCustom name (Text.pack err)
    Right (leftOver, _, ha)
      | allowLeftOver || BSL.null leftOver -> pure ha
      | otherwise ->
          cborError $ DecoderErrorLeftover name (BSL.toStrict leftOver)

decodeIPv4 :: Decoder s IPv4
decodeIPv4 =
  fromHostAddress
    <$> ifDecoderVersionAtLeast
      (natVersion @9)
      (binaryGetDecoder False "decodeIPv4" getWord32le)
      (binaryGetDecoder True "decodeIPv4" getWord32le)

getHostAddress6 :: Get HostAddress6
getHostAddress6 = do
  !w1 <- getWord32le
  !w2 <- getWord32le
  !w3 <- getWord32le
  !w4 <- getWord32le
  return (w1, w2, w3, w4)

decodeIPv6 :: Decoder s IPv6
decodeIPv6 =
  fromHostAddress6
    <$> ifDecoderVersionAtLeast
      (natVersion @9)
      (binaryGetDecoder False "decodeIPv6" getHostAddress6)
      (binaryGetDecoder True "decodeIPv6" getHostAddress6)

--------------------------------------------------------------------------------
-- Wrapped CBORG decoders
--------------------------------------------------------------------------------

decodeTagMaybe :: Decoder s (Maybe Word64)
decodeTagMaybe =
  peekTokenType >>= \case
    C.TypeTag -> Just . fromIntegral <$> decodeTag
    C.TypeTag64 -> Just <$> decodeTag64
    _ -> pure Nothing

allowTag :: Word -> Decoder s ()
allowTag tagExpected = do
  mTagReceived <- decodeTagMaybe
  forM_ mTagReceived $ \tagReceived ->
    unless (tagReceived == (fromIntegral tagExpected :: Word64)) $
      fail $
        "Expecteg tag " <> show tagExpected <> " but got tag " <> show tagReceived

assertTag :: Word -> Decoder s ()
assertTag tagExpected = do
  tagReceived <-
    decodeTagMaybe >>= \case
      Just tag -> pure tag
      Nothing -> fail "Expected tag"
  unless (tagReceived == (fromIntegral tagExpected :: Word64)) $
    fail $
      "Expecteg tag " <> show tagExpected <> " but got tag " <> show tagReceived

-- | Enforces that the input size is the same as the decoded one, failing in
--   case it's not
enforceSize :: Text.Text -> Int -> Decoder s ()
enforceSize lbl requestedSize = decodeListLen >>= matchSize lbl requestedSize

-- | Compare two sizes, failing if they are not equal
matchSize :: Text.Text -> Int -> Int -> Decoder s ()
matchSize lbl requestedSize actualSize =
  when (actualSize /= requestedSize) $
    cborError $
      DecoderErrorSizeMismatch
        lbl
        requestedSize
        actualSize

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
    else cborError $ DecoderErrorCustom "Natural" "got a negative number"

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

decodeString :: Decoder s Text.Text
decodeString = fromPlainDecoder C.decodeString

decodeStringCanonical :: Decoder s Text.Text
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

decodeTerm :: Decoder s C.Term
decodeTerm = fromPlainDecoder C.decodeTerm

peekAvailable :: Decoder s Int
peekAvailable = fromPlainDecoder C.peekAvailable

peekByteOffset :: Decoder s C.ByteOffset
peekByteOffset = fromPlainDecoder C.peekByteOffset

peekTokenType :: Decoder s C.TokenType
peekTokenType = fromPlainDecoder C.peekTokenType
