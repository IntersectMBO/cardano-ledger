{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FromVCBOR
  ( FromVCBOR (..),
    VDecoder (..),
    FromCBOR (..),
    DecoderError (..),
    C.ByteOffset,
    C.DecodeAction (..),
    C.Decoder,
    C.TokenType (..),
    enforceSize,
    matchSize,
    fromCBORMaybe,
    decodeListWith,

    -- * Helper tools to build instances
    decodeMapSkel,
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
import Data.Int (Int32, Int64)
import Data.Kind
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal
import Data.Primitive.Types (Prim)
import Data.Proxy
import Data.Ratio (Ratio, (%))
import Data.Reflection
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Typeable
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits
import Lens.Micro
import Numeric.Natural (Natural)

newtype VDecoder (v :: Nat) s a = VDecoder {unVDecoder :: C.Decoder s a}
  deriving (Functor, Applicative, Monad, MonadFail)

class Typeable a => FromVCBOR a where
  fromVCBOR :: KnownNat v => VDecoder v s a
  default fromVCBOR :: C.FromCBOR a => VDecoder v s a
  fromVCBOR = VDecoder fromCBOR

  vlabel :: proxy a -> T.Text
  vlabel = T.pack . show . typeRep

--------------------------------------------------------------------------------
-- Useful primitives
--------------------------------------------------------------------------------

cborError = VDecoder . cborError

toCborError = VDecoder . toCborError

decodeListLenOf = VDecoder . decodeListLenOf

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
    venforceSize "Ratio" 2
    n <- fromVCBOR
    d <- fromVCBOR
    if d <= 0
      then vcborError $ DecoderErrorCustom "Ratio" "invalid denominator"
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
      else vcborError $ DecoderErrorCustom "Natural" "got a negative number"

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
    vdecodeListLenOf 2
    !x <- fromVCBOR
    !y <- fromVCBOR
    return (x, y)

instance (FromVCBOR a, FromVCBOR b, FromVCBOR c) => FromVCBOR (a, b, c) where
  fromVCBOR = do
    vdecodeListLenOf 3
    !x <- fromVCBOR
    !y <- fromVCBOR
    !z <- fromVCBOR
    return (x, y, z)

instance (FromVCBOR a, FromVCBOR b, FromVCBOR c, FromVCBOR d) => FromVCBOR (a, b, c, d) where
  fromVCBOR = do
    vdecodeListLenOf 4
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
    vdecodeListLenOf 5
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
    vdecodeListLenOf 7
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
  fromVCBOR = vdecodeListWith fromVCBOR

instance (FromVCBOR a, FromVCBOR b) => FromVCBOR (Either a b) where
  fromVCBOR = do
    D.decodeListLenOf 2
    t <- D.decodeWord
    case t of
      0 -> do
        !x <- fromVCBOR
        return (Left x)
      1 -> do
        !x <- fromVCBOR
        return (Right x)
      _ -> cborError $ DecoderErrorUnknownTag "Either" (fromIntegral t)

instance FromVCBOR a => FromVCBOR (NonEmpty a) where
  fromVCBOR =
    nonEmpty <$> fromVCBOR
      >>= toCborError . \case
        Nothing -> Left $ DecoderErrorEmptyList "NonEmpty"
        Just xs -> Right xs

instance FromVCBOR a => FromVCBOR (Maybe a) where
  fromVCBOR = fromCBORMaybe fromVCBOR



    decodeFullDecoder,
    decodeListLen,
    decodeListWith,
    decodeMapSkel,
    dropMap,
    enforceSize,
    fromCBORMaybe,
    matchSize,
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
