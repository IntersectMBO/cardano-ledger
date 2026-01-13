{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Binary.Encoding.EncCBOR (
  EncCBOR (..),
  PreEncoded (..),
  toByronCBOR,
) where

import Cardano.Crypto.DSIGN.Class (
  DSIGNAlgorithm,
  SigDSIGN,
  SignKeyDSIGN,
  SignedDSIGN,
  VerKeyDSIGN,
 )
import Cardano.Crypto.Hash.Class (
  Hash (..),
  HashAlgorithm,
 )
import Cardano.Crypto.KES.Class (
  KESAlgorithm,
  SigKES,
  VerKeyKES,
 )
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
import Cardano.Ledger.Binary.Encoding.Encoder
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
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (SBA), fromByteArray)
import qualified Codec.CBOR.Encoding as C (Encoding (..))
import Codec.CBOR.Term (Term (..))
import qualified Codec.Serialise as Serialise (Serialise (encode))
import Control.Category (Category ((.)))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Short as SBS (length)
#if MIN_VERSION_bytestring(0,11,1)
import Data.ByteString.Short (ShortByteString(SBS))
#else
import Data.ByteString.Short.Internal (ShortByteString(SBS))
#endif
import qualified Cardano.Binary as Plain (Encoding, ToCBOR (..))
import Cardano.Ledger.Binary.Network (IPv4, IPv6)
import Data.Fixed (Fixed (..))
import Data.Foldable (toList)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe.Strict as SMaybe
import qualified Data.Primitive.ByteArray as Prim (ByteArray (..))
import Data.Ratio (Ratio)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..))
import qualified Data.VMap as VMap
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Prelude hiding (encodeFloat, (.))

class EncCBOR a where
  encCBOR :: a -> Encoding
  default encCBOR :: Plain.ToCBOR a => a -> Encoding
  encCBOR = fromPlainEncoding . Plain.toCBOR

newtype PreEncoded = PreEncoded {unPreEncoded :: BS.ByteString}

instance EncCBOR PreEncoded where
  encCBOR = encodePreEncoded . unPreEncoded

instance EncCBOR Version where
  encCBOR = encodeVersion

-- | Convert a versioned `EncCBOR` instance to a plain `Plain.Encoding` using Byron
-- protocol version.
toByronCBOR :: EncCBOR a => a -> Plain.Encoding
toByronCBOR = toPlainEncoding byronProtVer . encCBOR

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance EncCBOR () where
  encCBOR = const encodeNull

instance EncCBOR Bool where
  encCBOR = encodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance EncCBOR Integer where
  encCBOR = encodeInteger

instance EncCBOR Word where
  encCBOR = encodeWord

instance EncCBOR Word8 where
  encCBOR = encodeWord8

instance EncCBOR Word16 where
  encCBOR = encodeWord16

instance EncCBOR Word32 where
  encCBOR = encodeWord32

instance EncCBOR Word64 where
  encCBOR = encodeWord64

instance EncCBOR Int where
  encCBOR = encodeInt

instance EncCBOR Int8 where
  encCBOR = encodeInt8

instance EncCBOR Int16 where
  encCBOR = encodeInt16

instance EncCBOR Int32 where
  encCBOR = encodeInt32

instance EncCBOR Int64 where
  encCBOR = encodeInt64

instance EncCBOR Float where
  encCBOR = encodeFloat

instance EncCBOR Double where
  encCBOR = encodeDouble

instance EncCBOR a => EncCBOR (Ratio a) where
  encCBOR = encodeRatio encCBOR

deriving newtype instance EncCBOR (Fixed p)

instance EncCBOR Natural where
  encCBOR = encCBOR . toInteger

instance EncCBOR Void where
  encCBOR = absurd

instance EncCBOR IPv4 where
  encCBOR = encodeIPv4

instance EncCBOR IPv6 where
  encCBOR = encodeIPv6

--------------------------------------------------------------------------------
-- CBOR
--------------------------------------------------------------------------------

instance EncCBOR Term where
  encCBOR = encodeTerm

instance EncCBOR Encoding where
  encCBOR = id

instance EncCBOR C.Encoding where
  encCBOR = fromPlainEncoding

instance EncCBOR (Tokens -> Tokens) where
  encCBOR t = fromPlainEncoding (C.Encoding t)

--------------------------------------------------------------------------------
-- Tagged
--------------------------------------------------------------------------------

instance EncCBOR a => EncCBOR (Tagged s a) where
  encCBOR (Tagged a) = encCBOR a

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

instance (EncCBOR a, EncCBOR b) => EncCBOR (a, b) where
  encCBOR (a, b) = encodeListLen 2 <> encCBOR a <> encCBOR b

instance (EncCBOR a, EncCBOR b, EncCBOR c) => EncCBOR (a, b, c) where
  encCBOR (a, b, c) = encodeListLen 3 <> encCBOR a <> encCBOR b <> encCBOR c

instance (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d) => EncCBOR (a, b, c, d) where
  encCBOR (a, b, c, d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance
  (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d, EncCBOR e) =>
  EncCBOR (a, b, c, d, e)
  where
  encCBOR (a, b, c, d, e) =
    encodeListLen 5
      <> encCBOR a
      <> encCBOR b
      <> encCBOR c
      <> encCBOR d
      <> encCBOR e

instance
  (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d, EncCBOR e, EncCBOR f) =>
  EncCBOR (a, b, c, d, e, f)
  where
  encCBOR (a, b, c, d, e, f) =
    encodeListLen 6
      <> encCBOR a
      <> encCBOR b
      <> encCBOR c
      <> encCBOR d
      <> encCBOR e
      <> encCBOR f

instance
  (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d, EncCBOR e, EncCBOR f, EncCBOR g) =>
  EncCBOR (a, b, c, d, e, f, g)
  where
  encCBOR (a, b, c, d, e, f, g) =
    encodeListLen 7
      <> encCBOR a
      <> encCBOR b
      <> encCBOR c
      <> encCBOR d
      <> encCBOR e
      <> encCBOR f
      <> encCBOR g

instance EncCBOR BS.ByteString where
  encCBOR = encodeBytes

instance EncCBOR Text.Text where
  encCBOR = encodeString

instance EncCBOR ByteArray where
  encCBOR = encCBOR . unBA
  {-# INLINE encCBOR #-}

instance EncCBOR Prim.ByteArray where
  encCBOR = encodeByteArray . fromByteArray
  {-# INLINE encCBOR #-}

instance EncCBOR SlicedByteArray where
  encCBOR = encodeByteArray
  {-# INLINE encCBOR #-}

instance EncCBOR ShortByteString where
  encCBOR sbs@(SBS ba) =
    encodeByteArray $ SBA (Prim.ByteArray ba) 0 (SBS.length sbs)

instance EncCBOR BS.Lazy.ByteString where
  encCBOR = encCBOR . BS.Lazy.toStrict

instance EncCBOR a => EncCBOR [a] where
  encCBOR = encodeList encCBOR

instance (EncCBOR a, EncCBOR b) => EncCBOR (Either a b) where
  encCBOR (Left x) = encodeListLen 2 <> encodeWord 0 <> encCBOR x
  encCBOR (Right x) = encodeListLen 2 <> encodeWord 1 <> encCBOR x

instance EncCBOR a => EncCBOR (NonEmpty a) where
  encCBOR = encCBOR . toList

instance EncCBOR a => EncCBOR (Maybe a) where
  encCBOR = encodeMaybe encCBOR

instance EncCBOR a => EncCBOR (SMaybe.StrictMaybe a) where
  encCBOR = encodeStrictMaybe encCBOR

instance (EncCBOR k, EncCBOR v) => EncCBOR (Map.Map k v) where
  encCBOR = encodeMap encCBOR encCBOR

instance EncCBOR a => EncCBOR (Set.Set a) where
  encCBOR = encodeSet encCBOR

instance EncCBOR a => EncCBOR (Seq.Seq a) where
  encCBOR = encodeSeq encCBOR

instance EncCBOR a => EncCBOR (SSeq.StrictSeq a) where
  encCBOR = encodeStrictSeq encCBOR

instance
  (EncCBOR k, EncCBOR v, VMap.Vector kv k, VMap.Vector vv v) =>
  EncCBOR (VMap.VMap kv vv k v)
  where
  encCBOR = encodeVMap encCBOR encCBOR

instance EncCBOR a => EncCBOR (V.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}

instance (EncCBOR a, VP.Prim a) => EncCBOR (VP.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}

instance (EncCBOR a, VS.Storable a) => EncCBOR (VS.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}

instance (EncCBOR a, VU.Unbox a) => EncCBOR (VU.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

instance EncCBOR UTCTime where
  encCBOR = encodeUTCTime

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

instance DSIGNAlgorithm v => EncCBOR (VerKeyDSIGN v) where
  encCBOR = encodeVerKeyDSIGN

instance DSIGNAlgorithm v => EncCBOR (SignKeyDSIGN v) where
  encCBOR = encodeSignKeyDSIGN

instance DSIGNAlgorithm v => EncCBOR (SigDSIGN v) where
  encCBOR = encodeSigDSIGN

instance DSIGNAlgorithm v => EncCBOR (SignedDSIGN v a) where
  encCBOR = encodeSignedDSIGN

--------------------------------------------------------------------------------
-- Hash
--------------------------------------------------------------------------------

instance HashAlgorithm h => EncCBOR (Hash h a) where
  encCBOR (UnsafeHash h) = encCBOR h

instance KESAlgorithm k => EncCBOR (VerKeyKES k) where
  encCBOR = encodeVerKeyKES

instance KESAlgorithm k => EncCBOR (SigKES k) where
  encCBOR = encodeSigKES

instance EncCBOR (VerKeyVRF SimpleVRF) where
  encCBOR = encodeVerKeyVRF

instance EncCBOR (SignKeyVRF SimpleVRF) where
  encCBOR = encodeSignKeyVRF

instance EncCBOR (CertVRF SimpleVRF) where
  encCBOR = encodeCertVRF

instance EncCBOR (VerKeyVRF MockVRF) where
  encCBOR = encodeVerKeyVRF

instance EncCBOR (SignKeyVRF MockVRF) where
  encCBOR = encodeSignKeyVRF

instance EncCBOR (CertVRF MockVRF) where
  encCBOR = encodeCertVRF

deriving instance EncCBOR (OutputVRF v)

instance VRFAlgorithm v => EncCBOR (CertifiedVRF v a) where
  encCBOR cvrf =
    encodeListLen 2
      <> encCBOR (certifiedOutput cvrf)
      <> encodeCertVRF (certifiedProof cvrf)

instance EncCBOR Praos.Proof where
  encCBOR = encCBOR . Praos.proofBytes

instance EncCBOR Praos.SignKey where
  encCBOR = encCBOR . Praos.skBytes

instance EncCBOR Praos.VerKey where
  encCBOR = encCBOR . Praos.vkBytes

deriving instance EncCBOR (VerKeyVRF Praos.PraosVRF)

deriving instance EncCBOR (SignKeyVRF Praos.PraosVRF)

deriving instance EncCBOR (CertVRF Praos.PraosVRF)

--------------------------------------------------------------------------------
-- Slotting
--------------------------------------------------------------------------------

-- TODO: Remove usage of 'serialise' package
instance EncCBOR SlotNo where
  encCBOR = fromPlainEncoding . Serialise.encode

instance Serialise.Serialise t => EncCBOR (WithOrigin t) where
  encCBOR = fromPlainEncoding . Serialise.encode

deriving instance EncCBOR EpochNo

deriving instance EncCBOR EpochSize

deriving instance EncCBOR SystemStart

instance EncCBOR BlockNo where
  encCBOR = fromPlainEncoding . Serialise.encode

deriving instance EncCBOR EpochInterval

--------------------------------------------------------------------------------
-- Plutus
--------------------------------------------------------------------------------

instance Plain.ToCBOR PV1.Data where
  toCBOR = Serialise.encode

instance EncCBOR PV1.Data

instance EncCBOR PV1.ScriptContext where
  encCBOR = encCBOR . PV3.toData

instance EncCBOR PV2.ScriptContext where
  encCBOR = encCBOR . PV3.toData

instance EncCBOR PV3.ScriptContext where
  encCBOR = encCBOR . PV3.toData
