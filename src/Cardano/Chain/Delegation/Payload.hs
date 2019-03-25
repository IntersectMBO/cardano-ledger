{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Chain.Delegation.Payload
  ( APayload(..)
  , Payload
  , decodeAPayload
  , unsafePayload
  )
where

import Cardano.Prelude

import Formatting (bprint, int)
import Formatting.Buildable (Buildable(..))

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , ByteSpan
  , Decoded(..)
  , Decoder
  , annotatedDecoder
  , decodeListWith
  )
import qualified Cardano.Chain.Delegation.Certificate as Delegation
import Cardano.Crypto
  ( decodeAProxyVerificationKey )


-- | 'Payload' is put into 'MainBlock' and is a set of heavyweight proxy signing
--   keys. List of psk issuers should be unique also.
data APayload a = UnsafeAPayload
  { getPayload    :: [Delegation.ACertificate a]
  , getAnnotation :: a
  } deriving (Show, Eq, Generic, Functor)

instance (NFData a) => NFData (APayload a) where

type Payload = APayload ()

unsafePayload :: [Delegation.Certificate] -> Payload
unsafePayload sks = UnsafeAPayload sks ()

instance Decoded (APayload ByteString) where
  type BaseType (APayload ByteString)  = Payload
  recoverBytes = getAnnotation

instance Buildable (APayload a) where
  build (UnsafeAPayload psks _) = bprint
    ("proxy signing keys (" . int . " items): " . listJson . "\n")
    (length psks)
    psks

instance Bi (APayload ()) where
  encode = encode . getPayload
  decode = void <$> decodeAPayload

decodeAPayload :: Decoder s (APayload ByteSpan)
decodeAPayload = do
  (Annotated p a) <- annotatedDecoder
    (decodeListWith decodeAProxyVerificationKey)
  pure (UnsafeAPayload p a)
