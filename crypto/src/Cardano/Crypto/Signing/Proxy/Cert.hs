{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Crypto.Signing.Proxy.Cert
  ( ProxyCert(..)
  , safeCreateProxyCert
  , verifyProxyCert
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Formatting.Buildable (Buildable)
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary (Annotated, Decoded(..), FromCBOR, ToCBOR, serialize')
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.VerificationKey (VerificationKey(..))
import Cardano.Crypto.Signing.Safe (SafeSigner)
import Cardano.Crypto.Signing.Signature
  (Signature(..), safeSign, verifySignatureDecoded)
import Cardano.Crypto.Signing.Tag (SignTag(..))


newtype ProxyCert w = ProxyCert
  { unProxyCert :: Signature w
  } deriving ( Eq
             , Ord
             , Show
             , NFData
             , FromCBOR
             , ToCBOR
             , Buildable
             , Aeson.FromJSON
             , Aeson.ToJSON
             )

instance Monad m => ToJSON m (ProxyCert w) where
  toJSON = toJSON . unProxyCert

instance (Typeable x, MonadError SchemaError m) => FromJSON m (ProxyCert x) where
  fromJSON = fmap ProxyCert . fromJSON

-- | Proxy certificate creation from signing key of issuer, verification key of
--   delegate and the message space ω.
safeCreateProxyCert
  :: ToCBOR w => ProtocolMagicId -> SafeSigner -> VerificationKey -> w -> ProxyCert w
safeCreateProxyCert pm ss (VerificationKey delegateVK) o = coerce sig
 where
  sig = safeSign pm SignProxyVK ss
    $ mconcat ["00", CC.unXPub delegateVK, serialize' o]

-- | Checks if certificate is valid, given issuer vk, delegate vk and ω
--
--   We serialize' the annotated 'ByteString' as the Byron release signed that
--   way
verifyProxyCert
  :: (Decoded (f ByteString), Functor f)
  => Annotated ProtocolMagicId ByteString
  -> VerificationKey
  -> VerificationKey
  -> f ByteString
  -> ProxyCert (BaseType (f ByteString))
  -> Bool
verifyProxyCert pm issuerVK (VerificationKey delegateVK) o cert =
  verifySignatureDecoded
    pm
    SignProxyVK
    issuerVK
    (serialize' . mappend ("00" <> CC.unXPub delegateVK) <$> o)
    (coerce cert)
