{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- | Fake implementation of VRF, where the random value isn't random but given
-- by the creator.
module Cardano.Crypto.VRF.Fake
  ( FakeVRF
  , VerKeyVRF (..)
  , SignKeyVRF (..)
  , WithResult(..)
  ) where

import BaseTypes (Seed)
import Cardano.Binary (FromCBOR(..), ToCBOR (..), encodeListLen, enforceSize)
import Cardano.Crypto.Hash
import Cardano.Crypto.Util (nonNegIntR)
import Cardano.Crypto.VRF.Class
import Cardano.Prelude (NoUnexpectedThunks, UseIsNormalForm(..))
import Data.Data (Data)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data FakeVRF
  deriving Data

-- | A class for seeds which sneakily contain the certified natural we wish to
-- "randomly" derive from them.
class ToCBOR (Payload a) => SneakilyContainResult a where
  type Payload a
  sneakilyExtractResult :: a -> Natural
  unsneakilyExtractPayload :: a -> Payload a

data WithResult a = WithResult !a !Natural
  deriving (Eq, Show)

instance ToCBOR a => SneakilyContainResult (WithResult a) where
  type Payload (WithResult a) = a
  sneakilyExtractResult (WithResult _ nat) = nat
  unsneakilyExtractPayload (WithResult p _) = p

-- | An instance to allow this to be used in the way of `Mock` where no result
-- has been provided. Though note that the key isn't used at all here.
instance SneakilyContainResult Seed where
  type Payload Seed = Seed
  sneakilyExtractResult = fromHash . hashWithSerialiser @MD5 id . toCBOR
  unsneakilyExtractPayload = id

instance VRFAlgorithm FakeVRF where

  type Signable FakeVRF = SneakilyContainResult

  newtype VerKeyVRF FakeVRF = VerKeyFakeVRF Int
    deriving (Show, Eq, Ord, Generic, ToCBOR, FromCBOR, NoUnexpectedThunks)
  newtype SignKeyVRF FakeVRF = SignKeyFakeVRF Int
    deriving (Show, Eq, Ord, Generic, ToCBOR, FromCBOR, NoUnexpectedThunks)
  data CertVRF FakeVRF = CertFakeVRF Int Natural
    deriving (Show, Eq, Ord, Generic)
    deriving NoUnexpectedThunks via UseIsNormalForm (CertVRF FakeVRF)

  maxVRF _ = 2 ^ (8 * byteCount (Proxy :: Proxy MD5)) - 1
  genKeyVRF = SignKeyFakeVRF <$> nonNegIntR
  deriveVerKeyVRF (SignKeyFakeVRF n) = VerKeyFakeVRF n
  evalVRF a sk = return $ evalVRF' a sk
  -- This implementation of `verifyVRF` checks the real result, which is hidden
  -- in the certificate, but ignores the produced value, which is set to be the
  -- result of the sneaking.
  verifyVRF (VerKeyFakeVRF n) a c = snd (evalVRF' a (SignKeyFakeVRF n)) == snd c
  encodeVerKeyVRF = toCBOR
  decodeVerKeyVRF = fromCBOR

evalVRF' :: SneakilyContainResult a => a -> SignKeyVRF FakeVRF -> (Natural, CertVRF FakeVRF)
evalVRF' a (SignKeyFakeVRF n) =
  let y = sneakilyExtractResult a
      p = unsneakilyExtractPayload a
      realValue = fromHash . hashWithSerialiser @MD5 id $ toCBOR p
  in (y, CertFakeVRF n realValue)

instance FromCBOR (CertVRF FakeVRF) where
  fromCBOR = do
    enforceSize "FakeVRFCert should have two entries" 2
    CertFakeVRF <$> fromCBOR <*> fromCBOR

instance ToCBOR (CertVRF FakeVRF) where
  toCBOR (CertFakeVRF i n) = encodeListLen 2 <> toCBOR i <> toCBOR n
