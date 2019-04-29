{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Crypto.Signing.Proxy.VerificationKey
  ( AProxyVerificationKey(..)
  , ProxyVerificationKey

  -- * Constructors
  , createPsk
  , unsafeProxyVerificationKey

  -- * Accessors
  , pskOmega

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

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , FromCBOR(..)
  , ToCBOR(..)
  , fromCBORAnnotated
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

instance ToCBOR w => ToCBOR (ProxyVerificationKey w) where
  toCBOR psk =
    encodeListLen 4
      <> toCBOR (pskOmega psk)
      <> toCBOR (pskIssuerPk psk)
      <> toCBOR (pskDelegatePk psk)
      <> toCBOR (pskCert psk)

instance FromCBOR w => FromCBOR (ProxyVerificationKey w) where
  fromCBOR = void <$> fromCBOR @(AProxyVerificationKey w ByteSpan)

instance FromCBOR w => FromCBOR (AProxyVerificationKey w ByteSpan) where
  fromCBOR = do
    enforceSize "ProxyVerificationKey" 4
    UnsafeAProxyVerificationKey
      <$> fromCBORAnnotated
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

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
  :: ToCBOR w
  => ProtocolMagicId
  -> SafeSigner
  -> PublicKey
  -> w
  -> ProxyVerificationKey w
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
validateProxyVerificationKey
  :: MonadError Text m
  => Annotated ProtocolMagicId ByteString
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
