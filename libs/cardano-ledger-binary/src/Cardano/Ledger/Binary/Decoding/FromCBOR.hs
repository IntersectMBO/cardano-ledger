{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Binary.Decoding.FromCBOR
  ( FromCBOR (..),
  )
where

import Cardano.Ledger.Binary.Decoding.Decoder
import Codec.CBOR.ByteArray (ByteArray (BA))
import Codec.CBOR.Term (Term (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Fixed (Fixed (..), Nano, Pico)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe.Strict as SMaybe
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tagged (Tagged (Tagged))
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime (..))
import Data.Typeable
import qualified Data.VMap as VMap
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Prelude hiding (decodeFloat)

-- class ToVCBOR a where
--   toVCBOR :: KnownNat v => a -> VEncoding v

class Typeable a => FromCBOR a where
  fromCBOR :: Decoder s a

  label :: proxy a -> T.Text
  label = T.pack . show . typeRep

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance FromCBOR () where
  fromCBOR = decodeNull

instance FromCBOR Bool where
  fromCBOR = decodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance FromCBOR Integer where
  fromCBOR = decodeInteger

instance FromCBOR Natural where
  fromCBOR = decodeNatural

instance FromCBOR Word where
  fromCBOR = decodeWord

instance FromCBOR Word8 where
  fromCBOR = decodeWord8

instance FromCBOR Word16 where
  fromCBOR = decodeWord16

instance FromCBOR Word32 where
  fromCBOR = decodeWord32

instance FromCBOR Word64 where
  fromCBOR = decodeWord64

instance FromCBOR Int where
  fromCBOR = decodeInt

instance FromCBOR Int8 where
  fromCBOR = decodeInt8

instance FromCBOR Int16 where
  fromCBOR = decodeInt16

instance FromCBOR Int32 where
  fromCBOR = decodeInt32

instance FromCBOR Int64 where
  fromCBOR = decodeInt64

instance FromCBOR Float where
  fromCBOR = decodeFloat

instance FromCBOR Double where
  fromCBOR = decodeDouble

instance FromCBOR Rational where
  fromCBOR = decodeRational

instance FromCBOR Nano where
  fromCBOR = MkFixed <$> fromCBOR

instance FromCBOR Pico where
  fromCBOR = MkFixed <$> fromCBOR

instance FromCBOR NominalDiffTime where
  fromCBOR = decodeNominalDiffTime

instance FromCBOR Void where
  fromCBOR = cborError DecoderErrorVoid

instance FromCBOR Term where
  fromCBOR = decodeTerm

--------------------------------------------------------------------------------
-- Tagged
--------------------------------------------------------------------------------

instance (Typeable s, FromCBOR a) => FromCBOR (Tagged s a) where
  fromCBOR = Tagged <$> fromCBOR

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

instance (FromCBOR a, FromCBOR b) => FromCBOR (a, b) where
  fromCBOR = do
    decodeListLenOf 2
    !x <- fromCBOR
    !y <- fromCBOR
    return (x, y)

instance (FromCBOR a, FromCBOR b, FromCBOR c) => FromCBOR (a, b, c) where
  fromCBOR = do
    decodeListLenOf 3
    !x <- fromCBOR
    !y <- fromCBOR
    !z <- fromCBOR
    return (x, y, z)

instance (FromCBOR a, FromCBOR b, FromCBOR c, FromCBOR d) => FromCBOR (a, b, c, d) where
  fromCBOR = do
    decodeListLenOf 4
    !a <- fromCBOR
    !b <- fromCBOR
    !c <- fromCBOR
    !d <- fromCBOR
    return (a, b, c, d)

instance
  (FromCBOR a, FromCBOR b, FromCBOR c, FromCBOR d, FromCBOR e) =>
  FromCBOR (a, b, c, d, e)
  where
  fromCBOR = do
    decodeListLenOf 5
    !a <- fromCBOR
    !b <- fromCBOR
    !c <- fromCBOR
    !d <- fromCBOR
    !e <- fromCBOR
    return (a, b, c, d, e)

instance
  ( FromCBOR a,
    FromCBOR b,
    FromCBOR c,
    FromCBOR d,
    FromCBOR e,
    FromCBOR f,
    FromCBOR g
  ) =>
  FromCBOR (a, b, c, d, e, f, g)
  where
  fromCBOR = do
    decodeListLenOf 7
    !a <- fromCBOR
    !b <- fromCBOR
    !c <- fromCBOR
    !d <- fromCBOR
    !e <- fromCBOR
    !f <- fromCBOR
    !g <- fromCBOR
    return (a, b, c, d, e, f, g)

instance FromCBOR BS.ByteString where
  fromCBOR = decodeBytes

instance FromCBOR T.Text where
  fromCBOR = decodeString

instance FromCBOR BSL.ByteString where
  fromCBOR = BSL.fromStrict <$> fromCBOR

instance FromCBOR SBS.ShortByteString where
  fromCBOR = do
    BA (Prim.ByteArray ba) <- decodeByteArray
    return $ SBS.SBS ba

instance FromCBOR a => FromCBOR [a] where
  fromCBOR = decodeList fromCBOR

instance (FromCBOR a, FromCBOR b) => FromCBOR (Either a b) where
  fromCBOR = do
    decodeListLenOf 2
    t <- decodeWord
    case t of
      0 -> do
        !x <- fromCBOR
        return (Left x)
      1 -> do
        !x <- fromCBOR
        return (Right x)
      _ -> cborError $ DecoderErrorUnknownTag "Either" (fromIntegral t)

instance FromCBOR a => FromCBOR (NonEmpty a) where
  fromCBOR = do
    ls <- fromCBOR
    case nonEmpty ls of
      Nothing -> cborError $ DecoderErrorEmptyList "NonEmpty"
      Just ne -> pure ne

instance FromCBOR a => FromCBOR (Maybe a) where
  fromCBOR = decodeMaybe fromCBOR

instance FromCBOR a => FromCBOR (SMaybe.StrictMaybe a) where
  fromCBOR = SMaybe.maybeToStrictMaybe <$> decodeMaybe fromCBOR

instance (Ord a, FromCBOR a) => FromCBOR (SSeq.StrictSeq a) where
  fromCBOR = decodeStrictSeq fromCBOR

instance (Ord a, FromCBOR a) => FromCBOR (Set.Set a) where
  fromCBOR = decodeSet fromCBOR

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (Map.Map k v) where
  fromCBOR = decodeMap fromCBOR fromCBOR

instance
  ( Ord k,
    FromCBOR k,
    FromCBOR a,
    Typeable kv,
    Typeable av,
    VMap.Vector kv k,
    VMap.Vector av a
  ) =>
  FromCBOR (VMap.VMap kv av k a)
  where
  fromCBOR = decodeVMap fromCBOR fromCBOR

instance FromCBOR a => FromCBOR (V.Vector a) where
  fromCBOR = decodeVector fromCBOR
  {-# INLINE fromCBOR #-}

instance (FromCBOR a, VP.Prim a) => FromCBOR (VP.Vector a) where
  fromCBOR = decodeVector fromCBOR
  {-# INLINE fromCBOR #-}

instance (FromCBOR a, VS.Storable a) => FromCBOR (VS.Vector a) where
  fromCBOR = decodeVector fromCBOR
  {-# INLINE fromCBOR #-}

instance (FromCBOR a, VU.Unbox a) => FromCBOR (VU.Vector a) where
  fromCBOR = decodeVector fromCBOR
  {-# INLINE fromCBOR #-}

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

instance FromCBOR UTCTime where
  fromCBOR = decodeUTCTime
