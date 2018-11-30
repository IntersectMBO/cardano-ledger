{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Cardano.Binary.Class (Bi, Decoded(..), serialize')
import Cardano.Crypto.ProtocolMagic (ProtocolMagic)
import Cardano.Crypto.Signing.PublicKey (PublicKey(..))
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
             , Bi
             , Buildable
             , Aeson.FromJSON
             , Aeson.ToJSON
             )

instance Monad m => ToJSON m (ProxyCert w) where
  toJSON = toJSON . unProxyCert

instance (Typeable x, MonadError SchemaError m) => FromJSON m (ProxyCert x) where
  fromJSON = fmap ProxyCert . fromJSON

-- | Proxy certificate creation from secret key of issuer, public key of
--   delegate and the message space ω.
safeCreateProxyCert
  :: Bi w => ProtocolMagic -> SafeSigner -> PublicKey -> w -> ProxyCert w
safeCreateProxyCert pm ss (PublicKey delegatePk) o = coerce sig
 where
  sig = safeSign pm SignProxySK ss
    $ mconcat ["00", CC.unXPub delegatePk, serialize' o]

-- | Checks if certificate is valid, given issuer pk, delegate pk and ω
--
--   We serialize' the annotated 'ByteString' as the Byron release signed that
--   way
verifyProxyCert
  :: (Decoded (f ByteString), Functor f)
  => ProtocolMagic
  -> PublicKey
  -> PublicKey
  -> f ByteString
  -> ProxyCert (BaseType (f ByteString))
  -> Bool
verifyProxyCert pm issuerPk (PublicKey delegatePk) o cert =
  verifySignatureDecoded
    pm
    SignProxySK
    issuerPk
    (serialize' . mappend ("00" <> CC.unXPub delegatePk) <$> o)
    (coerce cert)
