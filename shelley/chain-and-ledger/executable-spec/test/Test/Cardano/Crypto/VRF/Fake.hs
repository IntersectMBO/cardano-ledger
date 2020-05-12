{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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
import Cardano.Prelude (NoUnexpectedThunks, UseIsNormalForm (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Seed)

data FakeVRF

-- | A class for seeds which sneakily contain the certified natural we wish to
-- "randomly" derive from them.
class ToCBOR (Payload a) => SneakilyContainResult a where
  type Payload a
  sneakilyExtractResult :: a -> Natural
  unsneakilyExtractPayload :: a -> Payload a

data WithResult a = WithResult !a !Word64
  deriving (Eq, Show)

instance ToCBOR a => SneakilyContainResult (WithResult a) where
  type Payload (WithResult a) = a
  sneakilyExtractResult (WithResult _ nat) = fromIntegral nat
  unsneakilyExtractPayload (WithResult p _) = p

-- | An instance to allow this to be used in the way of `Mock` where no result
-- has been provided. Though note that the key isn't used at all here.
instance SneakilyContainResult Seed where
  type Payload Seed = Seed
  sneakilyExtractResult = fromHash . hashWithSerialiser @MD5 id . toCBOR
  unsneakilyExtractPayload = id

instance VRFAlgorithm FakeVRF where
  algorithmNameVRF _ = "fakeVRF"
  seedSizeVRF _ = 8

  type Signable FakeVRF = SneakilyContainResult

  newtype VerKeyVRF FakeVRF = VerKeyFakeVRF Word64
    deriving (Show, Eq, Ord, Generic, NoUnexpectedThunks)
  newtype SignKeyVRF FakeVRF = SignKeyFakeVRF Word64
    deriving (Show, Eq, Ord, Generic, NoUnexpectedThunks)

  data CertVRF FakeVRF = CertFakeVRF Word64 Word16
    deriving (Show, Eq, Ord, Generic)
    deriving (NoUnexpectedThunks) via UseIsNormalForm (CertVRF FakeVRF)

  maxVRF _ = 2 ^ (8 * sizeHash (Proxy :: Proxy MD5)) - 1
  genKeyVRF seed = SignKeyFakeVRF $ runMonadRandomWithSeed seed getRandomWord64
  deriveVerKeyVRF (SignKeyFakeVRF n) = VerKeyFakeVRF n
  evalVRF () a sk = return $ evalVRF' a sk

  -- This implementation of `verifyVRF` checks the real result, which is hidden
  -- in the certificate, but ignores the produced value, which is set to be the
  -- result of the sneaking.
  verifyVRF () (VerKeyFakeVRF n) a c = snd (evalVRF' a (SignKeyFakeVRF n)) == snd c

  sizeVerKeyVRF _ = 8
  sizeSignKeyVRF _ = 8
  sizeCertVRF _ = 16

  rawSerialiseVerKeyVRF (VerKeyFakeVRF k) = writeBinaryWord64 k
  rawSerialiseSignKeyVRF (SignKeyFakeVRF k) = writeBinaryWord64 k
  rawSerialiseCertVRF (CertFakeVRF k s) =
    writeBinaryWord64 k <> writeBinaryWord64 (fromIntegral s)

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
    | [kb, smb] <- splitsAt [8, 8] bs,
      let k = readBinaryWord64 kb,
      let s = fromIntegral $ readBinaryWord64 smb =
      Just $! CertFakeVRF k s
    | otherwise =
      Nothing

evalVRF' ::
  SneakilyContainResult a =>
  a ->
  SignKeyVRF FakeVRF ->
  (Natural, CertVRF FakeVRF)
evalVRF' a (SignKeyFakeVRF n) =
  let y = sneakilyExtractResult a
      p = unsneakilyExtractPayload a
      realValue = fromIntegral . fromHash . hashWithSerialiser @MD5 id $ toCBOR p
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
