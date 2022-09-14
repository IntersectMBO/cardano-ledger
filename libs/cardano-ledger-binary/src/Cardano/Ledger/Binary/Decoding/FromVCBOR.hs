{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Binary.Decoding.FromVCBOR
  ( FromVCBOR (..),
  )
where

import Cardano.Ledger.Binary.Decoding.VDecoder
import Codec.CBOR.ByteArray (ByteArray (BA))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Fixed (Fixed (..), Nano, Pico)
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Primitive.ByteArray as Prim
import Data.Ratio (Ratio, (%))
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
import GHC.TypeLits
import Numeric.Natural (Natural)
import Prelude hiding (decodeFloat)

class Typeable a => FromVCBOR a where
  fromVCBOR :: KnownNat v => VDecoder v s a

  label :: proxy a -> T.Text
  label = T.pack . show . typeRep

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance FromVCBOR () where
  fromVCBOR = decodeNull

instance FromVCBOR Bool where
  fromVCBOR = decodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance FromVCBOR Integer where
  fromVCBOR = decodeInteger

instance FromVCBOR Word where
  fromVCBOR = decodeWord

instance FromVCBOR Word8 where
  fromVCBOR = decodeWord8

instance FromVCBOR Word16 where
  fromVCBOR = decodeWord16

instance FromVCBOR Word32 where
  fromVCBOR = decodeWord32

instance FromVCBOR Word64 where
  fromVCBOR = decodeWord64

instance FromVCBOR Int where
  fromVCBOR = decodeInt

instance FromVCBOR Float where
  fromVCBOR = decodeFloat

instance FromVCBOR Int32 where
  fromVCBOR = decodeInt32

instance FromVCBOR Int64 where
  fromVCBOR = decodeInt64

instance (Integral a, FromVCBOR a) => FromVCBOR (Ratio a) where
  fromVCBOR = do
    enforceSize "Ratio" 2
    n <- fromVCBOR
    d <- fromVCBOR
    if d <= 0
      then cborError $ DecoderErrorCustom "Ratio" "invalid denominator"
      else return $! n % d

instance FromVCBOR Nano where
  fromVCBOR = MkFixed <$> fromVCBOR

instance FromVCBOR Pico where
  fromVCBOR = MkFixed <$> fromVCBOR

instance FromVCBOR NominalDiffTime where
  fromVCBOR = decodeNominalDiffTime

instance FromVCBOR Natural where
  fromVCBOR = decodeNatural

instance FromVCBOR Void where
  fromVCBOR = cborError DecoderErrorVoid

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

instance FromVCBOR BS.ByteString where
  fromVCBOR = decodeBytes

instance FromVCBOR T.Text where
  fromVCBOR = decodeString

instance FromVCBOR BSL.ByteString where
  fromVCBOR = BSL.fromStrict <$> fromVCBOR

instance FromVCBOR SBS.ShortByteString where
  fromVCBOR = do
    BA (Prim.ByteArray ba) <- decodeByteArray
    return $ SBS.SBS ba

instance FromVCBOR a => FromVCBOR [a] where
  fromVCBOR = decodeList fromVCBOR

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
      _ -> cborError $ DecoderErrorUnknownTag "Either" (fromIntegral t)

instance FromVCBOR a => FromVCBOR (NonEmpty a) where
  fromVCBOR = do
    ls <- fromVCBOR
    case nonEmpty ls of
      Nothing -> cborError $ DecoderErrorEmptyList "NonEmpty"
      Just ne -> pure ne

instance FromVCBOR a => FromVCBOR (Maybe a) where
  fromVCBOR = fromCBORMaybe fromVCBOR

instance (Ord a, FromVCBOR a) => FromVCBOR (Set.Set a) where
  fromVCBOR = decodeSet fromVCBOR

instance (Ord k, FromVCBOR k, FromVCBOR v) => FromVCBOR (Map.Map k v) where
  fromVCBOR = decodeMap fromVCBOR fromVCBOR

instance
  ( Ord k,
    FromVCBOR k,
    FromVCBOR a,
    Typeable kv,
    Typeable av,
    VMap.Vector kv k,
    VMap.Vector av a
  ) =>
  FromVCBOR (VMap.VMap kv av k a)
  where
  fromVCBOR = decodeVMap fromVCBOR fromVCBOR

instance FromVCBOR a => FromVCBOR (V.Vector a) where
  fromVCBOR = decodeVector fromVCBOR
  {-# INLINE fromVCBOR #-}

instance (FromVCBOR a, VP.Prim a) => FromVCBOR (VP.Vector a) where
  fromVCBOR = decodeVector fromVCBOR
  {-# INLINE fromVCBOR #-}

instance (FromVCBOR a, VS.Storable a) => FromVCBOR (VS.Vector a) where
  fromVCBOR = decodeVector fromVCBOR
  {-# INLINE fromVCBOR #-}

instance (FromVCBOR a, VU.Unbox a) => FromVCBOR (VU.Vector a) where
  fromVCBOR = decodeVector fromVCBOR
  {-# INLINE fromVCBOR #-}

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

instance FromVCBOR UTCTime where
  fromVCBOR = decodeUTCTime
