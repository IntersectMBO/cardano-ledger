{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Chain.Delegation.Payload (
  APayload (..),
  Payload,
  unsafePayload,
)
where

import qualified Cardano.Chain.Delegation.Certificate as Delegation
import Cardano.Ledger.Binary (
  Annotated (..),
  ByteSpan,
  DecCBOR (..),
  Decoded (..),
  EncCBOR (..),
  annotatedDecoder,
 )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint, int)
import Formatting.Buildable (Buildable (..))

-- | The delegation 'Payload' contains a list of delegation 'Certificate's
data APayload a = UnsafeAPayload
  { getPayload :: [Delegation.ACertificate a]
  , getAnnotation :: a
  }
  deriving (Show, Eq, Generic, Functor)
  deriving anyclass (NFData)

type Payload = APayload ()

unsafePayload :: [Delegation.Certificate] -> Payload
unsafePayload sks = UnsafeAPayload sks ()

instance Decoded (APayload ByteString) where
  type BaseType (APayload ByteString) = Payload
  recoverBytes = getAnnotation

instance Buildable (APayload a) where
  build (UnsafeAPayload psks _) =
    bprint
      ("proxy signing keys (" . int . " items): " . listJson . "\n")
      (length psks)
      psks

-- Used for debugging purposes only
instance ToJSON a => ToJSON (APayload a)

instance EncCBOR Payload where
  encCBOR = encCBOR . getPayload

instance DecCBOR Payload where
  decCBOR = void <$> decCBOR @(APayload ByteSpan)

instance DecCBOR (APayload ByteSpan) where
  decCBOR = do
    (Annotated p a) <- annotatedDecoder decCBOR
    pure (UnsafeAPayload p a)
