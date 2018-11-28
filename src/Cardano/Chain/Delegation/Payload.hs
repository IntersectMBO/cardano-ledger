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
  , PayloadError(..)
  , checkPayload
  , decodeAPayload
  , unsafePayload
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import Formatting (bprint, int, stext)
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
import Cardano.Chain.Delegation.Certificate (ACertificate, Certificate)
import Cardano.Crypto
  (ProtocolMagic, decodeAProxySecretKey, validateProxySecretKey)


-- | 'Payload' is put into 'MainBlock' and is a set of heavyweight proxy signing
--   keys. List of psk issuers should be unique also.
data APayload a = UnsafeAPayload
  { getPayload    :: [ACertificate a]
  , getAnnotation :: a
  } deriving (Show, Eq, Generic, Functor)

instance (NFData a) => NFData (APayload a) where

type Payload = APayload ()

unsafePayload :: [Certificate] -> Payload
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
  (Annotated p a) <- annotatedDecoder (decodeListWith decodeAProxySecretKey)
  pure (UnsafeAPayload p a)

data PayloadError = PayloadPSKError Text

instance Buildable PayloadError where
  build = \case
    PayloadPSKError err -> bprint
      ( "ProxySecretKey invalid when checing Delegation.Payload.\n Error: "
      . stext
      )
      err

checkPayload
  :: MonadError PayloadError m => ProtocolMagic -> APayload ByteString -> m ()
checkPayload protocolMagic payload = forM_
  (getPayload payload)
  (liftEither . first PayloadPSKError . validateProxySecretKey protocolMagic)
  -- unless (allDistinct $ map pskIssuerPk proxySKs) $
  --     throwError "Some of block's PSKs have the same issuer, which is prohibited"
