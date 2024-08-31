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

module Cardano.Ledger.Binary.Decoding.DecCBOR (
  DecCBOR (..),
  fromByronCBOR,
  decodeScriptContextFromData,
)
where

import qualified Cardano.Binary as Plain (Decoder, FromCBOR (..))
import Cardano.Crypto.DSIGN.Class (
  DSIGNAlgorithm,
  SeedSizeDSIGN,
  SigDSIGN,
  SignKeyDSIGN,
  SignedDSIGN,
  VerKeyDSIGN,
 )
import Cardano.Crypto.Hash.Class (Hash, HashAlgorithm)
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
import Cardano.Ledger.Binary.Version (Version, byronProtVer)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (
  EpochInterval (..),
  EpochNo (..),
  EpochSize (..),
  SlotNo (..),
  WithOrigin (..),
 )
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
import Data.List.NonEmpty (NonEmpty)
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
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Prelude hiding (decodeFloat)

class Typeable a => DecCBOR a where
  decCBOR :: Decoder s a
  default decCBOR :: Plain.FromCBOR a => Decoder s a
  decCBOR = fromPlainDecoder Plain.fromCBOR
  {-# INLINE decCBOR #-}

  -- | Validate decoding of a Haskell value, without the need to actually construct
  -- it. Could be slightly faster than `decCBOR`, however it should respect this law:
  --
  -- > dropCBOR (proxy :: Proxy a) = () <$ (decCBOR :: Decoder s a)
  dropCBOR :: Proxy a -> Decoder s ()
  dropCBOR _ = () <$ decCBOR @a

  label :: Proxy a -> T.Text
  label = T.pack . show . typeRep

instance DecCBOR Version where
  decCBOR = decodeVersion
  {-# INLINE decCBOR #-}

-- | Convert a versioned `DecCBOR` instance to a plain `Plain.Decoder` using Byron protocol
-- version.
fromByronCBOR :: DecCBOR a => Plain.Decoder s a
fromByronCBOR = toPlainDecoder byronProtVer decCBOR
{-# INLINE fromByronCBOR #-}

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance DecCBOR () where
  decCBOR = decodeNull
  {-# INLINE decCBOR #-}

instance DecCBOR Bool where
  decCBOR = decodeBool
  {-# INLINE decCBOR #-}

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance DecCBOR Integer where
  decCBOR = decodeInteger
  {-# INLINE decCBOR #-}

instance DecCBOR Natural where
  decCBOR = decodeNatural
  {-# INLINE decCBOR #-}

instance DecCBOR Word where
  decCBOR = decodeWord
  {-# INLINE decCBOR #-}

instance DecCBOR Word8 where
  decCBOR = decodeWord8
  {-# INLINE decCBOR #-}

instance DecCBOR Word16 where
  decCBOR = decodeWord16
  {-# INLINE decCBOR #-}

instance DecCBOR Word32 where
  decCBOR = decodeWord32
  {-# INLINE decCBOR #-}

instance DecCBOR Word64 where
  decCBOR = decodeWord64
  {-# INLINE decCBOR #-}

instance DecCBOR Int where
  decCBOR = decodeInt
  {-# INLINE decCBOR #-}

instance DecCBOR Int8 where
  decCBOR = decodeInt8
  {-# INLINE decCBOR #-}

instance DecCBOR Int16 where
  decCBOR = decodeInt16
  {-# INLINE decCBOR #-}

instance DecCBOR Int32 where
  decCBOR = decodeInt32
  {-# INLINE decCBOR #-}

instance DecCBOR Int64 where
  decCBOR = decodeInt64
  {-# INLINE decCBOR #-}

instance DecCBOR Float where
  decCBOR = decodeFloat
  {-# INLINE decCBOR #-}

instance DecCBOR Double where
  decCBOR = decodeDouble
  {-# INLINE decCBOR #-}

instance DecCBOR Rational where
  decCBOR = decodeRational
  {-# INLINE decCBOR #-}

deriving newtype instance Typeable p => DecCBOR (Fixed p)

instance DecCBOR Void where
  decCBOR = cborError DecoderErrorVoid

instance DecCBOR Term where
  decCBOR = decodeTerm
  {-# INLINE decCBOR #-}

instance DecCBOR IPv4 where
  decCBOR = decodeIPv4
  {-# INLINE decCBOR #-}

instance DecCBOR IPv6 where
  decCBOR = decodeIPv6
  {-# INLINE decCBOR #-}

--------------------------------------------------------------------------------
-- Tagged
--------------------------------------------------------------------------------

instance (Typeable s, DecCBOR a) => DecCBOR (Tagged s a) where
  decCBOR = Tagged <$> decCBOR
  {-# INLINE decCBOR #-}
  dropCBOR _ = dropCBOR (Proxy @a)

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

instance (DecCBOR a, DecCBOR b) => DecCBOR (a, b) where
  decCBOR = do
    decodeListLenOf 2
    !x <- decCBOR
    !y <- decCBOR
    return (x, y)
  dropCBOR _ = decodeListLenOf 2 <* dropCBOR (Proxy @a) <* dropCBOR (Proxy @b)
  {-# INLINE decCBOR #-}

instance (DecCBOR a, DecCBOR b, DecCBOR c) => DecCBOR (a, b, c) where
  decCBOR = do
    decodeListLenOf 3
    !x <- decCBOR
    !y <- decCBOR
    !z <- decCBOR
    return (x, y, z)
  dropCBOR _ =
    decodeListLenOf 3
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
  {-# INLINE decCBOR #-}

instance (DecCBOR a, DecCBOR b, DecCBOR c, DecCBOR d) => DecCBOR (a, b, c, d) where
  decCBOR = do
    decodeListLenOf 4
    !a <- decCBOR
    !b <- decCBOR
    !c <- decCBOR
    !d <- decCBOR
    return (a, b, c, d)
  dropCBOR _ =
    decodeListLenOf 4
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
      <* dropCBOR (Proxy @d)
  {-# INLINE decCBOR #-}

instance
  (DecCBOR a, DecCBOR b, DecCBOR c, DecCBOR d, DecCBOR e) =>
  DecCBOR (a, b, c, d, e)
  where
  decCBOR = do
    decodeListLenOf 5
    !a <- decCBOR
    !b <- decCBOR
    !c <- decCBOR
    !d <- decCBOR
    !e <- decCBOR
    return (a, b, c, d, e)
  dropCBOR _ =
    decodeListLenOf 5
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
      <* dropCBOR (Proxy @d)
      <* dropCBOR (Proxy @e)
  {-# INLINE decCBOR #-}

instance
  (DecCBOR a, DecCBOR b, DecCBOR c, DecCBOR d, DecCBOR e, DecCBOR f) =>
  DecCBOR (a, b, c, d, e, f)
  where
  decCBOR = do
    decodeListLenOf 6
    !a <- decCBOR
    !b <- decCBOR
    !c <- decCBOR
    !d <- decCBOR
    !e <- decCBOR
    !f <- decCBOR
    return (a, b, c, d, e, f)
  dropCBOR _ =
    decodeListLenOf 6
      <* dropCBOR (Proxy @a)
      <* dropCBOR (Proxy @b)
      <* dropCBOR (Proxy @c)
      <* dropCBOR (Proxy @d)
      <* dropCBOR (Proxy @e)
      <* dropCBOR (Proxy @f)
  {-# INLINE decCBOR #-}

instance
  ( DecCBOR a
  , DecCBOR b
  , DecCBOR c
  , DecCBOR d
  , DecCBOR e
  , DecCBOR f
  , DecCBOR g
  ) =>
  DecCBOR (a, b, c, d, e, f, g)
  where
  decCBOR = do
    decodeListLenOf 7
    !a <- decCBOR
    !b <- decCBOR
    !c <- decCBOR
    !d <- decCBOR
    !e <- decCBOR
    !f <- decCBOR
    !g <- decCBOR
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
  {-# INLINE decCBOR #-}

instance DecCBOR BS.ByteString where
  decCBOR = decodeBytes
  {-# INLINE decCBOR #-}

instance DecCBOR T.Text where
  decCBOR = decodeString
  {-# INLINE decCBOR #-}

instance DecCBOR BSL.ByteString where
  decCBOR = BSL.fromStrict <$> decCBOR
  {-# INLINE decCBOR #-}

instance DecCBOR ShortByteString where
  decCBOR = do
    BA (Prim.ByteArray ba) <- decodeByteArray
    return $ SBS ba
  {-# INLINE decCBOR #-}

instance DecCBOR ByteArray where
  decCBOR = decodeByteArray
  {-# INLINE decCBOR #-}

instance DecCBOR Prim.ByteArray where
  decCBOR = unBA <$> decodeByteArray
  {-# INLINE decCBOR #-}

instance DecCBOR SlicedByteArray where
  decCBOR = fromByteArray . unBA <$> decodeByteArray
  {-# INLINE decCBOR #-}

instance DecCBOR a => DecCBOR [a] where
  decCBOR = decodeList decCBOR
  {-# INLINE decCBOR #-}

instance (DecCBOR a, DecCBOR b) => DecCBOR (Either a b) where
  decCBOR = decodeEither (decCBOR >>= \a -> a `seq` pure a) (decCBOR >>= \a -> a `seq` pure a)
  {-# INLINE decCBOR #-}
  dropCBOR _ = () <$ decodeEither (dropCBOR (Proxy :: Proxy a)) (dropCBOR (Proxy :: Proxy b))

instance DecCBOR a => DecCBOR (NonEmpty a) where
  decCBOR = decodeNonEmptyList decCBOR
  {-# INLINE decCBOR #-}

instance DecCBOR a => DecCBOR (Maybe a) where
  decCBOR = decodeMaybe decCBOR
  {-# INLINE decCBOR #-}
  dropCBOR _ = () <$ decodeMaybe (dropCBOR (Proxy @a))

instance DecCBOR a => DecCBOR (SMaybe.StrictMaybe a) where
  decCBOR = decodeStrictMaybe decCBOR
  {-# INLINE decCBOR #-}
  dropCBOR _ = () <$ decodeStrictMaybe (dropCBOR (Proxy @a))

instance DecCBOR a => DecCBOR (SSeq.StrictSeq a) where
  decCBOR = decodeStrictSeq decCBOR
  {-# INLINE decCBOR #-}

instance DecCBOR a => DecCBOR (Seq.Seq a) where
  decCBOR = decodeSeq decCBOR
  {-# INLINE decCBOR #-}

instance (Ord a, DecCBOR a) => DecCBOR (Set.Set a) where
  decCBOR = decodeSet decCBOR
  {-# INLINE decCBOR #-}

instance (Ord k, DecCBOR k, DecCBOR v) => DecCBOR (Map.Map k v) where
  decCBOR = decodeMap decCBOR decCBOR
  {-# INLINE decCBOR #-}

instance
  ( Ord k
  , DecCBOR k
  , DecCBOR a
  , Typeable kv
  , Typeable av
  , VMap.Vector kv k
  , VMap.Vector av a
  ) =>
  DecCBOR (VMap.VMap kv av k a)
  where
  decCBOR = decodeVMap decCBOR decCBOR
  {-# INLINE decCBOR #-}

instance DecCBOR a => DecCBOR (V.Vector a) where
  decCBOR = decodeVector decCBOR
  {-# INLINE decCBOR #-}

instance (DecCBOR a, VP.Prim a) => DecCBOR (VP.Vector a) where
  decCBOR = decodeVector decCBOR
  {-# INLINE decCBOR #-}

instance (DecCBOR a, VS.Storable a) => DecCBOR (VS.Vector a) where
  decCBOR = decodeVector decCBOR
  {-# INLINE decCBOR #-}

instance (DecCBOR a, VU.Unbox a) => DecCBOR (VU.Vector a) where
  decCBOR = decodeVector decCBOR
  {-# INLINE decCBOR #-}

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

instance DecCBOR UTCTime where
  decCBOR = decodeUTCTime
  {-# INLINE decCBOR #-}

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

instance DSIGNAlgorithm v => DecCBOR (VerKeyDSIGN v) where
  decCBOR = decodeVerKeyDSIGN
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm v => DecCBOR (SignKeyDSIGN v) where
  decCBOR = decodeSignKeyDSIGN
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm v => DecCBOR (SigDSIGN v) where
  decCBOR = decodeSigDSIGN
  {-# INLINE decCBOR #-}

instance (DSIGNAlgorithm v, Typeable a) => DecCBOR (SignedDSIGN v a) where
  decCBOR = decodeSignedDSIGN
  {-# INLINE decCBOR #-}

--------------------------------------------------------------------------------
-- Hash
--------------------------------------------------------------------------------

instance (HashAlgorithm h, Typeable a) => DecCBOR (Hash h a)

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  DecCBOR (VerKeyKES (SimpleKES d t))
  where
  decCBOR = decodeVerKeyKES
  {-# INLINE decCBOR #-}

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  DecCBOR (SignKeyKES (SimpleKES d t))
  where
  decCBOR = decodeSignKeyKES
  {-# INLINE decCBOR #-}

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  DecCBOR (SigKES (SimpleKES d t))
  where
  decCBOR = decodeSigKES
  {-# INLINE decCBOR #-}

instance (KESAlgorithm d, HashAlgorithm h) => DecCBOR (VerKeyKES (SumKES h d)) where
  decCBOR = decodeVerKeyKES
  {-# INLINE decCBOR #-}

instance (KESAlgorithm d, HashAlgorithm h) => DecCBOR (SignKeyKES (SumKES h d)) where
  decCBOR = decodeSignKeyKES
  {-# INLINE decCBOR #-}

instance (KESAlgorithm d, HashAlgorithm h) => DecCBOR (SigKES (SumKES h d)) where
  decCBOR = decodeSigKES
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm d => DecCBOR (VerKeyKES (CompactSingleKES d)) where
  decCBOR = decodeVerKeyKES
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm d => DecCBOR (SignKeyKES (CompactSingleKES d)) where
  decCBOR = decodeSignKeyKES
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm d => DecCBOR (SigKES (CompactSingleKES d)) where
  decCBOR = decodeSigKES
  {-# INLINE decCBOR #-}

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  DecCBOR (VerKeyKES (CompactSumKES h d))
  where
  decCBOR = decodeVerKeyKES
  {-# INLINE decCBOR #-}

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  DecCBOR (SignKeyKES (CompactSumKES h d))
  where
  decCBOR = decodeSignKeyKES
  {-# INLINE decCBOR #-}

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  DecCBOR (SigKES (CompactSumKES h d))
  where
  decCBOR = decodeSigKES
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm d => DecCBOR (VerKeyKES (SingleKES d)) where
  decCBOR = decodeVerKeyKES
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm d => DecCBOR (SignKeyKES (SingleKES d)) where
  decCBOR = decodeSignKeyKES
  {-# INLINE decCBOR #-}

instance DSIGNAlgorithm d => DecCBOR (SigKES (SingleKES d)) where
  decCBOR = decodeSigKES
  {-# INLINE decCBOR #-}

instance KnownNat t => DecCBOR (VerKeyKES (MockKES t)) where
  decCBOR = decodeVerKeyKES
  {-# INLINE decCBOR #-}

instance KnownNat t => DecCBOR (SignKeyKES (MockKES t)) where
  decCBOR = decodeSignKeyKES
  {-# INLINE decCBOR #-}

instance KnownNat t => DecCBOR (SigKES (MockKES t)) where
  decCBOR = decodeSigKES
  {-# INLINE decCBOR #-}

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

instance DecCBOR (VerKeyVRF SimpleVRF) where
  decCBOR = decodeVerKeyVRF
  {-# INLINE decCBOR #-}

instance DecCBOR (SignKeyVRF SimpleVRF) where
  decCBOR = decodeSignKeyVRF
  {-# INLINE decCBOR #-}

instance DecCBOR (CertVRF SimpleVRF) where
  decCBOR = decodeCertVRF
  {-# INLINE decCBOR #-}

instance DecCBOR (VerKeyVRF MockVRF) where
  decCBOR = decodeVerKeyVRF
  {-# INLINE decCBOR #-}

instance DecCBOR (SignKeyVRF MockVRF) where
  decCBOR = decodeSignKeyVRF
  {-# INLINE decCBOR #-}

instance DecCBOR (CertVRF MockVRF) where
  decCBOR = decodeCertVRF
  {-# INLINE decCBOR #-}

instance DecCBOR Praos.Proof where
  decCBOR = decCBOR >>= Praos.proofFromBytes
  {-# INLINE decCBOR #-}

instance DecCBOR Praos.SignKey where
  decCBOR = decCBOR >>= Praos.skFromBytes
  {-# INLINE decCBOR #-}

instance DecCBOR Praos.VerKey where
  decCBOR = decCBOR >>= Praos.vkFromBytes
  {-# INLINE decCBOR #-}

deriving instance DecCBOR (VerKeyVRF Praos.PraosVRF)

deriving instance DecCBOR (SignKeyVRF Praos.PraosVRF)

deriving instance DecCBOR (CertVRF Praos.PraosVRF)

deriving instance Typeable v => DecCBOR (OutputVRF v)

instance (VRFAlgorithm v, Typeable a) => DecCBOR (CertifiedVRF v a) where
  decCBOR =
    CertifiedVRF
      <$ enforceSize "CertifiedVRF" 2
      <*> decCBOR
      <*> decodeCertVRF
  {-# INLINE decCBOR #-}

--------------------------------------------------------------------------------
-- Slotting
--------------------------------------------------------------------------------

instance DecCBOR SlotNo where
  decCBOR = fromPlainDecoder Serialise.decode
  {-# INLINE decCBOR #-}

instance (Serialise.Serialise t, Typeable t) => DecCBOR (WithOrigin t) where
  decCBOR = fromPlainDecoder Serialise.decode
  {-# INLINE decCBOR #-}

deriving instance DecCBOR EpochNo

deriving instance DecCBOR EpochSize

deriving instance DecCBOR SystemStart

instance DecCBOR BlockNo where
  decCBOR = fromPlainDecoder decode
  {-# INLINE decCBOR #-}

deriving instance DecCBOR EpochInterval

--------------------------------------------------------------------------------
-- Plutus
--------------------------------------------------------------------------------

instance DecCBOR PV1.Data where
  decCBOR = fromPlainDecoder decode
  {-# INLINE decCBOR #-}

instance DecCBOR PV1.ScriptContext where
  decCBOR = decCBOR >>= decodeScriptContextFromData

instance DecCBOR PV2.ScriptContext where
  decCBOR = decCBOR >>= decodeScriptContextFromData

instance DecCBOR PV3.ScriptContext where
  decCBOR = decCBOR >>= decodeScriptContextFromData

decodeScriptContextFromData :: (PV3.FromData a, MonadFail m) => PV3.Data -> m a
decodeScriptContextFromData scriptContextData =
  case PV3.fromData scriptContextData of
    Nothing -> fail $ "ScriptContext cannot be decoded from Data: " <> show scriptContextData
    Just scriptContext -> pure scriptContext
