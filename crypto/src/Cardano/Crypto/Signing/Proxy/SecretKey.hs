{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Crypto.Signing.Proxy.SecretKey
  ( AProxySecretKey(..)
  , ProxySecretKey

  -- * Constructors
  , createPsk
  , unsafeProxySecretKey

  -- * Accessors
  , pskOmega

  -- * Decoder
  , decodeAProxySecretKey

  -- * Predicates
  , isSelfSignedPsk

  -- * Validators
  , validateProxySecretKey
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , ByteSpan
  , Decoder
  , decodeAnnotated
  , encodeListLen
  , enforceSize
  )
import Cardano.Crypto.ProtocolMagic (ProtocolMagic)
import Cardano.Crypto.Signing.Proxy.Cert
  (ProxyCert, safeCreateProxyCert, verifyProxyCert)
import Cardano.Crypto.Signing.PublicKey (PublicKey)
import Cardano.Crypto.Signing.Safe (SafeSigner, safeToPublic)


-- | Convenient wrapper for secret key, that's basically ω plus
-- certificate.
data AProxySecretKey w a = UnsafeAProxySecretKey
  { aPskOmega     :: Annotated w a
  , pskIssuerPk   :: PublicKey
  , pskDelegatePk :: PublicKey
  , pskCert       :: ProxyCert w
  } deriving (Eq, Ord, Show, Generic, Functor)
    deriving anyclass NFData

type ProxySecretKey w = AProxySecretKey w ()

unsafeProxySecretKey
  :: w -> PublicKey -> PublicKey -> ProxyCert w -> ProxySecretKey w
unsafeProxySecretKey w = UnsafeAProxySecretKey (Annotated w ())

pskOmega :: AProxySecretKey w a -> w
pskOmega = unAnnotated . aPskOmega

instance B.Buildable w => B.Buildable (AProxySecretKey w a) where
  build (UnsafeAProxySecretKey w iPk dPk _) = bprint
    ("ProxySk { w = " . build . ", iPk = " . build . ", dPk = " . build . " }")
    (unAnnotated w)
    iPk
    dPk

instance Bi w => Bi (ProxySecretKey w) where
  encode psk =
    encodeListLen 4
      <> encode (pskOmega psk)
      <> encode (pskIssuerPk psk)
      <> encode (pskDelegatePk psk)
      <> encode (pskCert psk)

  decode =  void <$> decodeAProxySecretKey

decodeAProxySecretKey :: Bi w => Decoder s (AProxySecretKey w ByteSpan)
decodeAProxySecretKey = do
  enforceSize "ProxySecretKey" 4
  UnsafeAProxySecretKey <$> decodeAnnotated <*> decode <*> decode <*> decode

instance ToJSON w => ToJSON (AProxySecretKey w ()) where
  toJSON psk = object
    [ "pskOmega" .= toJSON (pskOmega psk)
    , "pskIssuerPk" .= toJSON (pskIssuerPk psk)
    , "pskDelegatePk" .= toJSON (pskDelegatePk psk)
    , "pskCert" .= toJSON (pskCert psk)
    ]

instance FromJSON w => FromJSON (AProxySecretKey w ()) where
    parseJSON = withObject "ProxySecretKey" $ \v -> unsafeProxySecretKey
        <$> v .: "pskOmega"
        <*> v .: "pskIssuerPk"
        <*> v .: "pskDelegatePk"
        <*> v .: "pskCert"

-- | Creates proxy secret key
createPsk
  :: Bi w => ProtocolMagic -> SafeSigner -> PublicKey -> w -> ProxySecretKey w
createPsk pm ss delegatePk w = UnsafeAProxySecretKey
  { aPskOmega     = Annotated w ()
  , pskIssuerPk   = safeToPublic ss
  , pskDelegatePk = delegatePk
  , pskCert       = safeCreateProxyCert pm ss delegatePk w
  }

-- | Checks if delegate and issuer fields of proxy secret key are equal
isSelfSignedPsk :: ProxySecretKey w -> Bool
isSelfSignedPsk psk = pskIssuerPk psk == pskDelegatePk psk

-- | Return the key if it's valid, and throw an error otherwise
--
--   TODO: Make this unnecessary by performing validation in the decoder
validateProxySecretKey
  :: MonadError Text m => ProtocolMagic -> AProxySecretKey w ByteString -> m ()
validateProxySecretKey pm psk =
  verifyProxyCert
      pm
      (pskIssuerPk psk)
      (pskDelegatePk psk)
      (aPskOmega psk)
      (pskCert psk)
    `orThrowError` "a ProxySecretKey has an invalid signature"
