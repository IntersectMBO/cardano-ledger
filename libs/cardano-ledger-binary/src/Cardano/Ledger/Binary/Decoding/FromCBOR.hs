{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.Ledger.Binary.Decoding.FromCBOR (
  FromCBOR (..),
)
where

import Cardano.Binary (DecCBOR (..))
import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm, SeedSizeDSIGN, SigDSIGN, SignKeyDSIGN, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.EcdsaSecp256k1 (EcdsaSecp256k1DSIGN)
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import Cardano.Crypto.DSIGN.SchnorrSecp256k1 (SchnorrSecp256k1DSIGN)
import Cardano.Crypto.Hash.Class (Hash, HashAlgorithm, hashFromBytes, sizeHash)
import Cardano.Crypto.KES.Class (KESAlgorithm, OptimizedKESAlgorithm, SigKES, SignKeyKES, VerKeyKES)
import Cardano.Crypto.KES.CompactSingle (CompactSingleKES)
import Cardano.Crypto.KES.CompactSum (CompactSumKES)
import Cardano.Crypto.KES.Mock (MockKES)
import Cardano.Crypto.KES.Simple (SimpleKES)
import Cardano.Crypto.KES.Single (SingleKES)
import Cardano.Crypto.KES.Sum (SumKES)
import Cardano.Crypto.VRF.Class (
  CertVRF,
  CertifiedVRF (..),
  OutputVRF (..),
  SignKeyVRF,
  VRFAlgorithm,
  VerKeyVRF,
 )
import Cardano.Crypto.VRF.Mock (MockVRF)
import qualified Cardano.Crypto.VRF.Praos as Praos
import Cardano.Crypto.VRF.Simple (SimpleVRF)
import Cardano.Ledger.Binary.Crypto
import Cardano.Ledger.Binary.Decoding.Decoder
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..), WithOrigin (..))
import Cardano.Slotting.Time (SystemStart (..))
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray, fromByteArray)
import Codec.CBOR.Term (Term (..))
import Codec.Serialise as Serialise (Serialise (decode))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
#if MIN_VERSION_bytestring(0,11,1)
import Data.ByteString.Short (ShortByteString(SBS))
#else
import Data.ByteString.Short.Internal (ShortByteString(SBS))
#endif
import Data.Fixed (Fixed (..))
import Data.IP (IPv4, IPv6)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe.Strict as SMaybe
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tagged (Tagged (Tagged))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..))
import Data.Typeable (Proxy (..), Typeable, typeRep)
import qualified Data.VMap as VMap
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (*))
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import Prelude hiding (decodeFloat)

class Typeable a => FromCBOR a where
  fromCBOR :: Decoder s a
  default fromCBOR :: DecCBOR a => Decoder s a
  fromCBOR = fromPlainDecoder decCBOR

  -- | Validate decoding of a Haskell value, without the need to actually construct
  -- it. Coule be slightly faster than `fromCBOR`, however it should respect this law:
  --
  -- > dropCBOR (proxy :: Proxy a) = () <$ (fromCBOR :: Decoder s a)
  dropCBOR :: Proxy a -> Decoder s ()
  dropCBOR _ = () <$ fromCBOR @a

  label :: Proxy a -> T.Text
  label = T.pack . show . typeRep

instance FromCBOR Version where
  fromCBOR = decodeVersion

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

deriving newtype instance Typeable p => FromCBOR (Fixed p)

instance FromCBOR Void where
  fromCBOR = cborError DecoderErrorVoid

instance FromCBOR Term where
  fromCBOR = decodeTerm

instance FromCBOR IPv4 where
  fromCBOR = decodeIPv4

instance FromCBOR IPv6 where
  fromCBOR = decodeIPv6

--------------------------------------------------------------------------------
-- Tagged
--------------------------------------------------------------------------------

instance (Typeable s, FromCBOR a) => FromCBOR (Tagged s a) where
  fromCBOR = Tagged <$> fromCBOR

  dropCBOR _ = dropCBOR (Proxy @a)

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

instance (FromCBOR a, FromCBOR b) => FromCBOR (a, b) where
  fromCBOR = do
    decodeListLenOf 2
    !x <- fromCBOR
    !y <- fromCBOR
    return (x, y)
  dropCBOR _ = decodeListLenOf 2 <* dropCBOR (Proxy @a) <* dropCBOR (Proxy @b)

instance (FromCBOR a, FromCBOR b, FromCBOR c) => FromCBOR (a, b, c) where
  fromCBOR = do
    decodeListLenOf 3
    !x <- fromCBOR
    !y <- fromCBOR
    !z <- fromCBOR
    return (x, y, z)
  dropCBOR _ =
    decodeListLenOf 3
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)

instance (FromCBOR a, FromCBOR b, FromCBOR c, FromCBOR d) => FromCBOR (a, b, c, d) where
  fromCBOR = do
    decodeListLenOf 4
    !a <- fromCBOR
    !b <- fromCBOR
    !c <- fromCBOR
    !d <- fromCBOR
    return (a, b, c, d)
  dropCBOR _ =
    decodeListLenOf 4
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
      <* dropCBOR (Proxy @d)

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
  dropCBOR _ =
    decodeListLenOf 5
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
      <* dropCBOR (Proxy @d)
      <* dropCBOR (Proxy @e)

instance
  (FromCBOR a, FromCBOR b, FromCBOR c, FromCBOR d, FromCBOR e, FromCBOR f) =>
  FromCBOR (a, b, c, d, e, f)
  where
  fromCBOR = do
    decodeListLenOf 6
    !a <- fromCBOR
    !b <- fromCBOR
    !c <- fromCBOR
    !d <- fromCBOR
    !e <- fromCBOR
    !f <- fromCBOR
    return (a, b, c, d, e, f)
  dropCBOR _ =
    decodeListLenOf 6
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
      <* dropCBOR (Proxy @d)
      <* dropCBOR (Proxy @e)
      <* dropCBOR (Proxy @f)

instance
  ( FromCBOR a
  , FromCBOR b
  , FromCBOR c
  , FromCBOR d
  , FromCBOR e
  , FromCBOR f
  , FromCBOR g
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
  dropCBOR _ =
    decodeListLenOf 7
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
      <* dropCBOR (Proxy @d)
      <* dropCBOR (Proxy @e)
      <* dropCBOR (Proxy @f)
      <* dropCBOR (Proxy @g)

instance FromCBOR BS.ByteString where
  fromCBOR = decodeBytes

instance FromCBOR T.Text where
  fromCBOR = decodeString

instance FromCBOR BSL.ByteString where
  fromCBOR = BSL.fromStrict <$> fromCBOR

instance FromCBOR ShortByteString where
  fromCBOR = do
    BA (Prim.ByteArray ba) <- decodeByteArray
    return $ SBS ba

instance FromCBOR ByteArray where
  fromCBOR = decodeByteArray

instance FromCBOR Prim.ByteArray where
  fromCBOR = unBA <$> decodeByteArray

instance FromCBOR SlicedByteArray where
  fromCBOR = fromByteArray . unBA <$> decodeByteArray

instance FromCBOR a => FromCBOR [a] where
  fromCBOR = decodeList fromCBOR

instance (FromCBOR a, FromCBOR b) => FromCBOR (Either a b) where
  fromCBOR = decodeEither (fromCBOR >>= \a -> a `seq` pure a) (fromCBOR >>= \a -> a `seq` pure a)
  dropCBOR _ = () <$ decodeEither (dropCBOR (Proxy :: Proxy a)) (dropCBOR (Proxy :: Proxy b))

instance FromCBOR a => FromCBOR (NonEmpty a) where
  fromCBOR = do
    ls <- fromCBOR
    case nonEmpty ls of
      Nothing -> cborError $ DecoderErrorEmptyList "NonEmpty"
      Just ne -> pure ne

instance FromCBOR a => FromCBOR (Maybe a) where
  fromCBOR = decodeMaybe fromCBOR
  dropCBOR _ = () <$ decodeMaybe (dropCBOR (Proxy @a))

instance FromCBOR a => FromCBOR (SMaybe.StrictMaybe a) where
  fromCBOR = SMaybe.maybeToStrictMaybe <$> decodeMaybe fromCBOR
  dropCBOR _ = () <$ decodeMaybe (dropCBOR (Proxy @a))

instance FromCBOR a => FromCBOR (SSeq.StrictSeq a) where
  fromCBOR = decodeStrictSeq fromCBOR

instance FromCBOR a => FromCBOR (Seq.Seq a) where
  fromCBOR = decodeSeq fromCBOR

instance (Ord a, FromCBOR a) => FromCBOR (Set.Set a) where
  fromCBOR = decodeSet fromCBOR

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (Map.Map k v) where
  fromCBOR = decodeMap fromCBOR fromCBOR

instance
  ( Ord k
  , FromCBOR k
  , FromCBOR a
  , Typeable kv
  , Typeable av
  , VMap.Vector kv k
  , VMap.Vector av a
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

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

instance FromCBOR (VerKeyDSIGN EcdsaSecp256k1DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance FromCBOR (SignKeyDSIGN EcdsaSecp256k1DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance FromCBOR (SigDSIGN EcdsaSecp256k1DSIGN) where
  fromCBOR = decodeSigDSIGN

instance FromCBOR (VerKeyDSIGN MockDSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance FromCBOR (SignKeyDSIGN MockDSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance FromCBOR (SigDSIGN MockDSIGN) where
  fromCBOR = decodeSigDSIGN

instance FromCBOR (VerKeyDSIGN Ed25519DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance FromCBOR (SignKeyDSIGN Ed25519DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance FromCBOR (SigDSIGN Ed25519DSIGN) where
  fromCBOR = decodeSigDSIGN

instance FromCBOR (VerKeyDSIGN Ed448DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance FromCBOR (SignKeyDSIGN Ed448DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance FromCBOR (SigDSIGN Ed448DSIGN) where
  fromCBOR = decodeSigDSIGN

instance FromCBOR (VerKeyDSIGN SchnorrSecp256k1DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance FromCBOR (SignKeyDSIGN SchnorrSecp256k1DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance FromCBOR (SigDSIGN SchnorrSecp256k1DSIGN) where
  fromCBOR = decodeSigDSIGN

--------------------------------------------------------------------------------
-- Hash
--------------------------------------------------------------------------------

instance (HashAlgorithm h, Typeable a) => FromCBOR (Hash h a) where
  fromCBOR = do
    bs <- decodeBytes
    case hashFromBytes bs of
      Just x -> return x
      Nothing ->
        fail $
          "hash bytes wrong size, expected "
            ++ show expected
            ++ " but got "
            ++ show actual
        where
          expected = sizeHash (Proxy :: Proxy h)
          actual = BS.length bs

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  FromCBOR (VerKeyKES (SimpleKES d t))
  where
  fromCBOR = decodeVerKeyKES

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  FromCBOR (SignKeyKES (SimpleKES d t))
  where
  fromCBOR = decodeSignKeyKES

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  FromCBOR (SigKES (SimpleKES d t))
  where
  fromCBOR = decodeSigKES

instance (KESAlgorithm d, HashAlgorithm h) => FromCBOR (VerKeyKES (SumKES h d)) where
  fromCBOR = decodeVerKeyKES

instance (KESAlgorithm d, HashAlgorithm h) => FromCBOR (SignKeyKES (SumKES h d)) where
  fromCBOR = decodeSignKeyKES

instance (KESAlgorithm d, HashAlgorithm h) => FromCBOR (SigKES (SumKES h d)) where
  fromCBOR = decodeSigKES

instance DSIGNAlgorithm d => FromCBOR (VerKeyKES (CompactSingleKES d)) where
  fromCBOR = decodeVerKeyKES

instance DSIGNAlgorithm d => FromCBOR (SignKeyKES (CompactSingleKES d)) where
  fromCBOR = decodeSignKeyKES

instance DSIGNAlgorithm d => FromCBOR (SigKES (CompactSingleKES d)) where
  fromCBOR = decodeSigKES

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  FromCBOR (VerKeyKES (CompactSumKES h d))
  where
  fromCBOR = decodeVerKeyKES

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  FromCBOR (SignKeyKES (CompactSumKES h d))
  where
  fromCBOR = decodeSignKeyKES

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  FromCBOR (SigKES (CompactSumKES h d))
  where
  fromCBOR = decodeSigKES

instance DSIGNAlgorithm d => FromCBOR (VerKeyKES (SingleKES d)) where
  fromCBOR = decodeVerKeyKES

instance DSIGNAlgorithm d => FromCBOR (SignKeyKES (SingleKES d)) where
  fromCBOR = decodeSignKeyKES

instance DSIGNAlgorithm d => FromCBOR (SigKES (SingleKES d)) where
  fromCBOR = decodeSigKES

instance KnownNat t => FromCBOR (VerKeyKES (MockKES t)) where
  fromCBOR = decodeVerKeyKES

instance KnownNat t => FromCBOR (SignKeyKES (MockKES t)) where
  fromCBOR = decodeSignKeyKES

instance KnownNat t => FromCBOR (SigKES (MockKES t)) where
  fromCBOR = decodeSigKES

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

instance FromCBOR (VerKeyVRF SimpleVRF) where
  fromCBOR = decodeVerKeyVRF

instance FromCBOR (SignKeyVRF SimpleVRF) where
  fromCBOR = decodeSignKeyVRF

instance FromCBOR (CertVRF SimpleVRF) where
  fromCBOR = decodeCertVRF

instance FromCBOR (VerKeyVRF MockVRF) where
  fromCBOR = decodeVerKeyVRF

instance FromCBOR (SignKeyVRF MockVRF) where
  fromCBOR = decodeSignKeyVRF

instance FromCBOR (CertVRF MockVRF) where
  fromCBOR = decodeCertVRF

instance FromCBOR Praos.Proof where
  fromCBOR = fromCBOR >>= Praos.proofFromBytes

instance FromCBOR Praos.SignKey where
  fromCBOR = fromCBOR >>= Praos.skFromBytes

instance FromCBOR Praos.VerKey where
  fromCBOR = fromCBOR >>= Praos.vkFromBytes

deriving instance FromCBOR (VerKeyVRF Praos.PraosVRF)

deriving instance FromCBOR (SignKeyVRF Praos.PraosVRF)

deriving instance FromCBOR (CertVRF Praos.PraosVRF)

deriving instance Typeable v => FromCBOR (OutputVRF v)

instance (VRFAlgorithm v, Typeable a) => FromCBOR (CertifiedVRF v a) where
  fromCBOR =
    CertifiedVRF
      <$ enforceSize "CertifiedVRF" 2
      <*> fromCBOR
      <*> decodeCertVRF

--------------------------------------------------------------------------------
-- Slotting
--------------------------------------------------------------------------------

instance FromCBOR SlotNo where
  fromCBOR = fromPlainDecoder Serialise.decode

instance (Serialise.Serialise t, Typeable t) => FromCBOR (WithOrigin t) where
  fromCBOR = fromPlainDecoder Serialise.decode

deriving instance FromCBOR EpochNo

deriving instance FromCBOR EpochSize

deriving instance FromCBOR SystemStart

instance FromCBOR BlockNo where
  fromCBOR = fromPlainDecoder decode

--------------------------------------------------------------------------------
-- Plutus
--------------------------------------------------------------------------------

instance FromCBOR PV1.Data where
  fromCBOR = fromPlainDecoder decode
