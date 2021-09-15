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
module Test.Cardano.Crypto.VRF.Fake
  ( FakeVRF,
    VerKeyVRF (..),
    SignKeyVRF (..),
    WithResult (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.Hash
import Cardano.Crypto.Seed (runMonadRandomWithSeed)
import Cardano.Crypto.Util
import Cardano.Crypto.VRF.Class
import Cardano.Ledger.BaseTypes (Seed)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data FakeVRF

-- | A class for seeds which sneakily contain the certified output we wish to
-- "randomly" derive from them.
class ToCBOR (Payload a) => SneakilyContainResult a where
  type Payload a
  sneakilyExtractResult :: a -> SignKeyVRF FakeVRF -> OutputVRF FakeVRF
  unsneakilyExtractPayload :: a -> Payload a

data WithResult a = WithResult !a !Word64
  deriving (Eq, Show)

instance ToCBOR a => SneakilyContainResult (WithResult a) where
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
      . hashWithSerialiser @Blake2b_224 id
      $ toCBOR s <> toCBOR sk
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

  data CertVRF FakeVRF = CertFakeVRF !Word64 !Word16
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NoThunks)

  genKeyVRF seed = SignKeyFakeVRF $ runMonadRandomWithSeed seed getRandomWord64
  deriveVerKeyVRF (SignKeyFakeVRF n) = VerKeyFakeVRF n
  evalVRF () a sk = evalVRF' a sk

  -- This implementation of `verifyVRF` checks the real result, which is hidden
  -- in the certificate, but ignores the produced value, which is set to be the
  -- result of the sneaking.
  verifyVRF () (VerKeyFakeVRF n) a c = snd (evalVRF' a (SignKeyFakeVRF n)) == snd c

  sizeVerKeyVRF _ = 8
  sizeSignKeyVRF _ = 8
  sizeCertVRF _ = 10
  sizeOutputVRF _ = sizeHash (Proxy :: Proxy Blake2b_224)

  rawSerialiseVerKeyVRF (VerKeyFakeVRF k) = writeBinaryWord64 k
  rawSerialiseSignKeyVRF (SignKeyFakeVRF k) = writeBinaryWord64 k
  rawSerialiseCertVRF (CertFakeVRF k s) =
    writeBinaryWord64 k <> writeBinaryWord16 s

  rawDeserialiseVerKeyVRF bs
    | [kb] <- splitsAt [8] bs,
      let k = readBinaryWord64 kb =
      Just $! VerKeyFakeVRF k
    | otherwise =
      Nothing

  rawDeserialiseSignKeyVRF bs
    | [kb] <- splitsAt [8] bs,
      let k = readBinaryWord64 kb =
      Just $! SignKeyFakeVRF k
    | otherwise =
      Nothing

  rawDeserialiseCertVRF bs
    | [kb, smb] <- splitsAt [8, 2] bs,
      let k = readBinaryWord64 kb,
      let s = readBinaryWord16 smb =
      Just $! CertFakeVRF k s
    | otherwise =
      Nothing

evalVRF' ::
  SneakilyContainResult a =>
  a ->
  SignKeyVRF FakeVRF ->
  (OutputVRF FakeVRF, CertVRF FakeVRF)
evalVRF' a sk@(SignKeyFakeVRF n) =
  let y = sneakilyExtractResult a sk
      p = unsneakilyExtractPayload a
      realValue =
        fromIntegral . bytesToNatural . hashToBytes
          . hashWithSerialiser @Blake2b_224 id
          $ toCBOR p <> toCBOR sk
   in (y, CertFakeVRF n realValue)

instance FromCBOR (VerKeyVRF FakeVRF) where
  fromCBOR = decodeVerKeyVRF

instance ToCBOR (VerKeyVRF FakeVRF) where
  toCBOR = encodeVerKeyVRF

instance FromCBOR (SignKeyVRF FakeVRF) where
  fromCBOR = decodeSignKeyVRF

instance ToCBOR (SignKeyVRF FakeVRF) where
  toCBOR = encodeSignKeyVRF

instance FromCBOR (CertVRF FakeVRF) where
  fromCBOR = decodeCertVRF

instance ToCBOR (CertVRF FakeVRF) where
  toCBOR = encodeCertVRF

readBinaryWord16 :: ByteString -> Word16
readBinaryWord16 =
  BS.foldl' (\acc w8 -> unsafeShiftL acc 8 + fromIntegral w8) 0

writeBinaryWord16 :: Word16 -> ByteString
writeBinaryWord16 =
  BS.reverse . fst
    . BS.unfoldrN 2 (\w -> Just (fromIntegral w, unsafeShiftR w 8))
