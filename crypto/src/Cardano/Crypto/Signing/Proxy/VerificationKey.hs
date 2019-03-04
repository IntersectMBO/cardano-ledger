{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Crypto.Signing.Proxy.VerificationKey
  ( AProxyVerificationKey(..)
  , ProxyVerificationKey

  -- * Constructors
  , createPsk
  , unsafeProxyVerificationKey

  -- * Accessors
  , pskOmega

  -- * Decoder
  , decodeAProxyVerificationKey

  -- * Predicates
  , isSelfSignedPsk

  -- * Validators
  , validateProxyVerificationKey
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
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.Proxy.Cert
  (ProxyCert, safeCreateProxyCert, verifyProxyCert)
import Cardano.Crypto.Signing.PublicKey (PublicKey)
import Cardano.Crypto.Signing.Safe (SafeSigner, safeToPublic)


-- | Convenient wrapper for secret key, that's basically ω plus
-- certificate.
data AProxyVerificationKey w a = UnsafeAProxyVerificationKey
  { aPskOmega     :: Annotated w a
  , pskIssuerPk   :: PublicKey
  , pskDelegatePk :: PublicKey
  , pskCert       :: ProxyCert w
  } deriving (Eq, Ord, Show, Generic, Functor)
    deriving anyclass NFData

type ProxyVerificationKey w = AProxyVerificationKey w ()

unsafeProxyVerificationKey
  :: w -> PublicKey -> PublicKey -> ProxyCert w -> ProxyVerificationKey w
unsafeProxyVerificationKey w = UnsafeAProxyVerificationKey (Annotated w ())

pskOmega :: AProxyVerificationKey w a -> w
pskOmega = unAnnotated . aPskOmega

instance B.Buildable w => B.Buildable (AProxyVerificationKey w a) where
  build (UnsafeAProxyVerificationKey w iPk dPk _) = bprint
    ("ProxyVk { w = " . build . ", iPk = " . build . ", dPk = " . build . " }")
    (unAnnotated w)
    iPk
    dPk

instance Bi w => Bi (ProxyVerificationKey w) where
  encode psk =
    encodeListLen 4
      <> encode (pskOmega psk)
      <> encode (pskIssuerPk psk)
      <> encode (pskDelegatePk psk)
      <> encode (pskCert psk)

  decode =  void <$> decodeAProxyVerificationKey

decodeAProxyVerificationKey :: Bi w => Decoder s (AProxyVerificationKey w ByteSpan)
decodeAProxyVerificationKey = do
  enforceSize "ProxyVerificationKey" 4
  UnsafeAProxyVerificationKey <$> decodeAnnotated <*> decode <*> decode <*> decode

instance ToJSON w => ToJSON (AProxyVerificationKey w ()) where
  toJSON psk = object
    [ "pskOmega" .= toJSON (pskOmega psk)
    , "pskIssuerPk" .= toJSON (pskIssuerPk psk)
    , "pskDelegatePk" .= toJSON (pskDelegatePk psk)
    , "pskCert" .= toJSON (pskCert psk)
    ]

instance FromJSON w => FromJSON (AProxyVerificationKey w ()) where
    parseJSON = withObject "ProxyVerificationKey" $ \v -> unsafeProxyVerificationKey
        <$> v .: "pskOmega"
        <*> v .: "pskIssuerPk"
        <*> v .: "pskDelegatePk"
        <*> v .: "pskCert"

-- | Creates proxy secret key
createPsk
  :: Bi w => ProtocolMagicId -> SafeSigner -> PublicKey -> w -> ProxyVerificationKey w
createPsk pm ss delegatePk w = UnsafeAProxyVerificationKey
  { aPskOmega     = Annotated w ()
  , pskIssuerPk   = safeToPublic ss
  , pskDelegatePk = delegatePk
  , pskCert       = safeCreateProxyCert pm ss delegatePk w
  }

-- | Checks if delegate and issuer fields of proxy secret key are equal
isSelfSignedPsk :: ProxyVerificationKey w -> Bool
isSelfSignedPsk psk = pskIssuerPk psk == pskDelegatePk psk

-- | Return the key if it's valid, and throw an error otherwise
--
--   TODO: Make this unnecessary by performing validation in the decoder
validateProxyVerificationKey
  :: MonadError Text m
  => ProtocolMagicId
  -> AProxyVerificationKey w ByteString
  -> m ()
validateProxyVerificationKey pm psk =
  verifyProxyCert
      pm
      (pskIssuerPk psk)
      (pskDelegatePk psk)
      (aPskOmega psk)
      (pskCert psk)
    `orThrowError` "a ProxyVerificationKey has an invalid signature"
