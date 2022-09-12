{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.FromVCBOR
  ( FromVCBOR (..),
    VDecoder (..),
    C.DecoderError (..),
    C.ByteOffset,
    C.DecodeAction (..),
    C.Decoder,
    C.TokenType (..),
    enforceSize,
    matchSize,
    -- fromCBORMaybe,
    decodeListWith,

    -- * Helper tools to build instances
    cborError,
    toCborError,
  )
where

import qualified Cardano.Binary as C
  ( Decoder,
    DecoderError (..),
    FromCBOR (..),
    decodeFullDecoder,
    decodeListLen,
    decodeListWith,
    decodeMapSkel,
    dropMap,
    enforceSize,
    fromCBORMaybe,
    matchSize,
  )
import qualified Cardano.Prelude as C
  ( cborError,
    toCborError,
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
    getDecodeAction,
    liftST,
    peekAvailable,
    peekByteOffset,
    peekTokenType,
  )
import Control.Monad
import Control.Monad (void, (<$!>))
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.BiMap (BiMap (..), biMapFromMap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Coders (decode, decodeMap, decodeVMap, invalidKey)
import Data.Fixed (Fixed (..), Nano, Pico)
import qualified Data.Foldable as F
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal
import Data.Primitive.Types (Prim)
import Data.Proxy
import Data.Ratio (Ratio, (%))
import Data.Reflection
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), picosecondsToDiffTime)
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits
import Numeric.Natural (Natural)

newtype VDecoder (v :: Nat) s a = VDecoder {unVDecoder :: C.Decoder s a}
  deriving (Functor, Applicative, Monad, MonadFail)

class Typeable a => FromVCBOR a where
  fromVCBOR :: KnownNat v => VDecoder v s a
  default fromVCBOR :: C.FromCBOR a => VDecoder v s a
  fromVCBOR = VDecoder C.fromCBOR

  label :: proxy a -> T.Text
  label = T.pack . show . typeRep

--------------------------------------------------------------------------------
-- Useful primitives
--------------------------------------------------------------------------------

cborError :: C.DecoderError -> VDecoder v s a
cborError = VDecoder . C.cborError

toCborError :: Either C.DecoderError a -> VDecoder v s a
toCborError = VDecoder . C.toCborError

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance FromVCBOR ()

instance FromVCBOR Bool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance FromVCBOR Integer

instance FromVCBOR Word

instance FromVCBOR Word8

instance FromVCBOR Word16

instance FromVCBOR Word32

instance FromVCBOR Word64

instance FromVCBOR Int

instance FromVCBOR Float

instance FromVCBOR Int32

instance FromVCBOR Int64

instance (Integral a, FromVCBOR a) => FromVCBOR (Ratio a) where
  fromVCBOR = do
    enforceSize "Ratio" 2
    n <- fromVCBOR
    d <- fromVCBOR
    if d <= 0
      then cborError $ C.DecoderErrorCustom "Ratio" "invalid denominator"
      else return $! n % d

instance FromVCBOR Nano where
  fromVCBOR = MkFixed <$> fromVCBOR

instance FromVCBOR Pico where
  fromVCBOR = MkFixed <$> fromVCBOR

-- | For backwards compatibility we round pico precision to micro
instance FromVCBOR NominalDiffTime where
  fromVCBOR = fromRational . (% 1e6) <$> fromVCBOR

instance FromVCBOR Natural where
  fromVCBOR = do
    !n <- fromVCBOR
    if n >= 0
      then return $! fromInteger n
      else cborError $ C.DecoderErrorCustom "Natural" "got a negative number"

instance FromVCBOR Void

--------------------------------------------------------------------------------
-- Tagged
--------------------------------------------------------------------------------

instance (Typeable s, FromVCBOR a) => FromVCBOR (Tagged s a) where
  fromVCBOR = Tagged <$> fromVCBOR

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

instance (FromVCBOR a, FromVCBOR b) => FromVCBOR (a, b) where
  fromVCBOR = do
    decodeListLenOf 2
    !x <- fromVCBOR
    !y <- fromVCBOR
    return (x, y)

instance (FromVCBOR a, FromVCBOR b, FromVCBOR c) => FromVCBOR (a, b, c) where
  fromVCBOR = do
    decodeListLenOf 3
    !x <- fromVCBOR
    !y <- fromVCBOR
    !z <- fromVCBOR
    return (x, y, z)

instance (FromVCBOR a, FromVCBOR b, FromVCBOR c, FromVCBOR d) => FromVCBOR (a, b, c, d) where
  fromVCBOR = do
    decodeListLenOf 4
    !a <- fromVCBOR
    !b <- fromVCBOR
    !c <- fromVCBOR
    !d <- fromVCBOR
    return (a, b, c, d)

instance
  (FromVCBOR a, FromVCBOR b, FromVCBOR c, FromVCBOR d, FromVCBOR e) =>
  FromVCBOR (a, b, c, d, e)
  where
  fromVCBOR = do
    decodeListLenOf 5
    !a <- fromVCBOR
    !b <- fromVCBOR
    !c <- fromVCBOR
    !d <- fromVCBOR
    !e <- fromVCBOR
    return (a, b, c, d, e)

instance
  ( FromVCBOR a,
    FromVCBOR b,
    FromVCBOR c,
    FromVCBOR d,
    FromVCBOR e,
    FromVCBOR f,
    FromVCBOR g
  ) =>
  FromVCBOR (a, b, c, d, e, f, g)
  where
  fromVCBOR = do
    decodeListLenOf 7
    !a <- fromVCBOR
    !b <- fromVCBOR
    !c <- fromVCBOR
    !d <- fromVCBOR
    !e <- fromVCBOR
    !f <- fromVCBOR
    !g <- fromVCBOR
    return (a, b, c, d, e, f, g)

instance FromVCBOR BS.ByteString

instance FromVCBOR T.Text

instance FromVCBOR BSL.ByteString where
  fromVCBOR = BSL.fromStrict <$> fromVCBOR

instance FromVCBOR SBS.ShortByteString

instance FromVCBOR a => FromVCBOR [a] where
  fromVCBOR = decodeListWith fromVCBOR

instance (FromVCBOR a, FromVCBOR b) => FromVCBOR (Either a b) where
  fromVCBOR = do
    decodeListLenOf 2
    t <- decodeWord
    case t of
      0 -> do
        !x <- fromVCBOR
        return (Left x)
      1 -> do
        !x <- fromVCBOR
        return (Right x)
      _ -> cborError $ C.DecoderErrorUnknownTag "Either" (fromIntegral t)

instance FromVCBOR a => FromVCBOR (NonEmpty a) where
  fromVCBOR =
    nonEmpty <$> fromVCBOR
      >>= toCborError . \case
        Nothing -> Left $ C.DecoderErrorEmptyList "NonEmpty"
        Just xs -> Right xs

instance FromVCBOR a => FromVCBOR (Maybe a) where
  fromVCBOR = fromCBORMaybe fromVCBOR

decodeContainerSkelWithReplicate ::
  (FromVCBOR a, KnownNat v) =>
  -- | How to get the size of the container
  VDecoder v s Int ->
  -- | replicateM for the container
  (Int -> VDecoder v s a -> VDecoder v s c) ->
  -- | concat for the container
  ([c] -> c) ->
  VDecoder v s c
decodeContainerSkelWithReplicate decodeLen replicateFun fromList' = do
  -- Look at how much data we have at the moment and use it as the limit for
  -- the size of a single call to replicateFun. We don't want to use
  -- replicateFun directly on the result of decodeLen since this might lead to
  -- DOS attack (attacker providing a huge value for length). So if it's above
  -- our limit, we'll do manual chunking and then combine the containers into
  -- one.
  sz <- decodeLen
  limit <- peekAvailable
  if sz <= limit
    then replicateFun sz fromVCBOR
    else do
      -- Take the max of limit and a fixed chunk size (note: limit can be
      -- 0). This basically means that the attacker can make us allocate a
      -- container of size 128 even though there's no actual input.
      let chunkSize = max limit 128
          (d, m) = sz `divMod` chunkSize
          buildOne s = replicateFun s fromVCBOR
      containers <- sequence $ buildOne m : replicate d (buildOne chunkSize)
      return $! fromList' containers
{-# INLINE decodeContainerSkelWithReplicate #-}

-- | Checks canonicity by comparing the new key being decoded with
--   the previous one, to enfore these are sorted the correct way.
--   See: https://tools.ietf.org/html/rfc7049#section-3.9
--   "[..]The keys in every map must be sorted lowest value to highest.[...]"
decodeMapSkel ::
  forall k a m v s.
  (Ord k, FromVCBOR k, FromVCBOR a, KnownNat v) =>
  ([(k, a)] -> m) ->
  VDecoder v s m
decodeMapSkel fromListFIFO = do
  n <- decodeMapLen
  case n of
    0 -> return (fromListFIFO [])
    _ -> do
      (firstKey, firstValue) <- decodeEntry
      fromListFIFO <$> decodeEntries (n - 1) firstKey [(firstKey, firstValue)]
  where
    -- Decode a single (k,v).
    decodeEntry :: VDecoder v s (k, a)
    decodeEntry = do
      !k <- fromVCBOR
      !v <- fromVCBOR
      return (k, v)

    -- Decode all the entries, enforcing canonicity by ensuring that the
    -- previous key is smaller than the next one.
    decodeEntries :: Int -> k -> [(k, a)] -> VDecoder v s [(k, a)]
    decodeEntries 0 _ acc = pure $ reverse acc
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

instance (Ord k, FromVCBOR k, FromVCBOR v) => FromVCBOR (Map.Map k v) where
  fromVCBOR = decodeMapSkel fromDistinctAscList

-- We stitch a `258` in from of a (Hash)Set, so that tools which
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
  (Ord a, FromVCBOR a, KnownNat v) =>
  ([a] -> c) ->
  VDecoder v s c
decodeSetSkel fromListFIFO = do
  decodeSetTag
  n <- decodeListLen
  case n of
    0 -> return (fromListFIFO [])
    _ -> do
      firstValue <- fromVCBOR
      fromListFIFO <$> decodeEntries (n - 1) firstValue [firstValue]
  where
    decodeEntries :: Int -> a -> [a] -> VDecoder v s [a]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingEntries previousValue !acc = do
      newValue <- fromVCBOR
      -- Order of values needs to be strictly increasing, because otherwise
      -- it's possible to supply lists with various amount of duplicates which
      -- will result in the same set.
      if newValue > previousValue
        then decodeEntries (remainingEntries - 1) newValue (newValue : acc)
        else cborError $ C.DecoderErrorCanonicityViolation "Set"
{-# INLINE decodeSetSkel #-}

instance (Ord a, FromVCBOR a) => FromVCBOR (Set.Set a) where
  fromVCBOR = decodeSetSkel Set.fromDistinctAscList

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: (FromVCBOR a, VG.Vector vec a, KnownNat v) => VDecoder v s (vec a)
decodeVector =
  decodeContainerSkelWithReplicate
    decodeListLen
    VG.replicateM
    VG.concat
{-# INLINE decodeVector #-}

instance (FromVCBOR a) => FromVCBOR (V.Vector a) where
  fromVCBOR = decodeVector
  {-# INLINE fromVCBOR #-}

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

instance FromVCBOR UTCTime where
  fromVCBOR = do
    enforceSize "UTCTime" 3
    year <- decodeInteger
    dayOfYear <- decodeInt
    timeOfDayPico <- decodeInteger
    return $
      UTCTime
        (fromOrdinalDate year dayOfYear)
        (picosecondsToDiffTime timeOfDayPico)

--------------------------------------------------------------------------------
-- Promoted CBORG primitives
--------------------------------------------------------------------------------

-- | Enforces that the input size is the same as the decoded one, failing in
--   case it's not
enforceSize :: T.Text -> Int -> VDecoder v s ()
enforceSize lbl requestedSize = VDecoder (C.enforceSize lbl requestedSize)

-- | Compare two sizes, failing if they are not equal
matchSize :: T.Text -> Int -> Int -> VDecoder v s ()
matchSize lbl requestedSize actualSize = VDecoder (C.matchSize lbl requestedSize actualSize)

-- | @'D.Decoder'@ for list.
decodeListWith :: VDecoder v s a -> VDecoder v s [a]
decodeListWith (VDecoder d) = VDecoder (C.decodeListWith d)

-- decodeFullDecoder = VDecoder C.decodeFullDecoder

-- decodeMapSkel = VDecoder . C.decodeMapSkel
-- dropMap = VDecoder C.dropMap

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

peekAvailable = VDecoder C.peekAvailable

peekByteOffset = VDecoder C.peekByteOffset

peekTokenType = VDecoder C.peekTokenType
