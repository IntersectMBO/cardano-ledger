{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Fake implementation of VRF, where the random value isn't random but given
-- by the creator.
module Test.Cardano.Protocol.Crypto.VRF.Fake (
  NatNonce (..),
  FakeVRF,
  VerKeyVRF (..),
  SignKeyVRF (..),
  WithResult (..),
)
where

import Cardano.Crypto.Hash
import Cardano.Crypto.Seed (runMonadRandomWithSeed)
import Cardano.Crypto.Util
import Cardano.Crypto.VRF.Class hiding (
  decodeCertVRF,
  decodeSignKeyVRF,
  decodeVerKeyVRF,
  encodeCertVRF,
  encodeSignKeyVRF,
  encodeVerKeyVRF,
 )
import Cardano.Ledger.BaseTypes (Seed, shelleyProtVer)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), hashWithEncoder)
import Cardano.Ledger.Binary.Crypto (
  decodeCertVRF,
  decodeSignKeyVRF,
  decodeVerKeyVRF,
  encodeCertVRF,
  encodeSignKeyVRF,
  encodeVerKeyVRF,
 )
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural

-- | We provide our own nonces to 'mkBlock', which we then wish to recover as
-- the output of the VRF functions. In general, however, we just derive them
-- from a natural. Since the nonce is a hash, we do not want to recover it to
-- find a preimage. In testing, therefore, we just wrap the raw natural, which
-- we then encode into the fake VRF implementation.
newtype NatNonce = NatNonce Natural

data FakeVRF

-- | A class for seeds which sneakily contain the certified output we wish to
-- "randomly" derive from them.
class EncCBOR (Payload a) => SneakilyContainResult a where
  type Payload a
  sneakilyExtractResult :: a -> SignKeyVRF FakeVRF -> OutputVRF FakeVRF
  unsneakilyExtractPayload :: a -> Payload a

data WithResult a = WithResult !a !Word64
  deriving (Eq, Show)

instance EncCBOR a => SneakilyContainResult (WithResult a) where
  type Payload (WithResult a) = a

  -- Note that this instance completely ignores the key.
  sneakilyExtractResult (WithResult _ nat) _ =
    -- Fill in the word64 as the low 8 bytes of a 16 byte string
    OutputVRF (toBytes (BS.word64BE 0 <> BS.word64BE nat))
    where
      toBytes = LBS.toStrict . BS.toLazyByteString

  unsneakilyExtractPayload (WithResult p _) = p

-- | An instance to allow this to be used in the way of `Mock` where no result
-- has been provided.
instance SneakilyContainResult Seed where
  type Payload Seed = Seed
  sneakilyExtractResult s sk =
    OutputVRF
      . hashToBytes
      . hashWithEncoder @Blake2b_224 shelleyProtVer id
      $ encCBOR s <> encCBOR sk
  unsneakilyExtractPayload = id

instance VRFAlgorithm FakeVRF where
  algorithmNameVRF _ = "fakeVRF"
  seedSizeVRF _ = 8

  type Signable FakeVRF = SneakilyContainResult

  newtype VerKeyVRF FakeVRF = VerKeyFakeVRF Word64
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, NoThunks)
  newtype SignKeyVRF FakeVRF = SignKeyFakeVRF Word64
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, NoThunks)

  data CertVRF FakeVRF = CertFakeVRF !Word64 !Word16 !(OutputVRF FakeVRF)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NoThunks)

  genKeyVRF seed = SignKeyFakeVRF $ runMonadRandomWithSeed seed getRandomWord64
  deriveVerKeyVRF (SignKeyFakeVRF n) = VerKeyFakeVRF n
  evalVRF () a sk = evalFakeVRF a sk

  -- This implementation of 'verifyVRF' checks the real proof, which is contained
  -- in the certificate, but ignores the produced value, and insteads returns
  -- the output which is stored in the 'CertFakeVRF'.
  verifyVRF () (VerKeyFakeVRF n) a (OutputVRF _, CertFakeVRF _ proof _)
    | proof == recomputedProof = True
    | otherwise = False
    where
      (OutputVRF recomputedProofBytes, _) = evalFakeVRF a (SignKeyFakeVRF n)
      recomputedProof = fromIntegral . bytesToNatural $ recomputedProofBytes

  sizeVerKeyVRF _ = 8
  sizeSignKeyVRF _ = 8
  sizeCertVRF _ = 26
  sizeOutputVRF _ = sizeHash (Proxy :: Proxy Blake2b_224)

  rawSerialiseVerKeyVRF (VerKeyFakeVRF k) = writeBinaryWord64 k
  rawSerialiseSignKeyVRF (SignKeyFakeVRF k) = writeBinaryWord64 k
  rawSerialiseCertVRF (CertFakeVRF k s (OutputVRF b)) =
    writeBinaryWord64 k <> writeBinaryWord16 s <> b

  rawDeserialiseVerKeyVRF bs
    | [kb] <- splitsAt [8] bs
    , let k = readBinaryWord64 kb =
        Just $! VerKeyFakeVRF k
    | otherwise =
        Nothing

  rawDeserialiseSignKeyVRF bs
    | [kb] <- splitsAt [8] bs
    , let k = readBinaryWord64 kb =
        Just $! SignKeyFakeVRF k
    | otherwise =
        Nothing

  rawDeserialiseCertVRF bs
    | [kb, smb, xs] <- splitsAt [8, 2, 16] bs
    , let k = readBinaryWord64 kb
    , let s = readBinaryWord16 smb =
        Just $! CertFakeVRF k s (OutputVRF xs)
    | otherwise =
        Nothing

evalFakeVRF ::
  SneakilyContainResult a =>
  a ->
  SignKeyVRF FakeVRF ->
  (OutputVRF FakeVRF, CertVRF FakeVRF)
evalFakeVRF a sk@(SignKeyFakeVRF n) =
  let y = sneakilyExtractResult a sk
      p = unsneakilyExtractPayload a
      proof =
        fromIntegral
          . bytesToNatural
          . hashToBytes
          . hashWithEncoder @Blake2b_224 shelleyProtVer id
          $ encCBOR p <> encCBOR sk
   in (y, CertFakeVRF n proof y)

instance DecCBOR (VerKeyVRF FakeVRF) where
  decCBOR = decodeVerKeyVRF

instance EncCBOR (VerKeyVRF FakeVRF) where
  encCBOR = encodeVerKeyVRF

instance DecCBOR (SignKeyVRF FakeVRF) where
  decCBOR = decodeSignKeyVRF

instance EncCBOR (SignKeyVRF FakeVRF) where
  encCBOR = encodeSignKeyVRF

instance DecCBOR (CertVRF FakeVRF) where
  decCBOR = decodeCertVRF

instance EncCBOR (CertVRF FakeVRF) where
  encCBOR = encodeCertVRF

readBinaryWord16 :: ByteString -> Word16
readBinaryWord16 =
  BS.foldl' (\acc w8 -> unsafeShiftL acc 8 + fromIntegral w8) 0

writeBinaryWord16 :: Word16 -> ByteString
writeBinaryWord16 =
  BS.reverse
    . fst
    . BS.unfoldrN 2 (\w -> Just (fromIntegral w, unsafeShiftR w 8))
