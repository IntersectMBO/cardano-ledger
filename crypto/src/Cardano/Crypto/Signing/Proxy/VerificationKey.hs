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
import Cardano.Crypto.Signing.VerificationKey (VerificationKey)
import Cardano.Crypto.Signing.Safe (SafeSigner, safeToVerification)


-- | Convenient wrapper for signing key, that's basically ω plus
-- certificate.
data AProxyVerificationKey w a = UnsafeAProxyVerificationKey
  { aPskOmega     :: Annotated w a
  , pskIssuerVK   :: VerificationKey
  , pskDelegateVK :: VerificationKey
  , pskCert       :: ProxyCert w
  } deriving (Eq, Ord, Show, Generic, Functor)
    deriving anyclass NFData

type ProxyVerificationKey w = AProxyVerificationKey w ()

unsafeProxyVerificationKey
  :: w -> VerificationKey -> VerificationKey -> ProxyCert w -> ProxyVerificationKey w
unsafeProxyVerificationKey w = UnsafeAProxyVerificationKey (Annotated w ())

pskOmega :: AProxyVerificationKey w a -> w
pskOmega = unAnnotated . aPskOmega

instance B.Buildable w => B.Buildable (AProxyVerificationKey w a) where
  build (UnsafeAProxyVerificationKey w iVK dVK _) = bprint
    ("ProxyVk { w = " . build . ", iVK = " . build . ", dVK = " . build . " }")
    (unAnnotated w)
    iVK
    dVK

instance ToCBOR w => ToCBOR (ProxyVerificationKey w) where
  toCBOR psk =
    encodeListLen 4
      <> toCBOR (pskOmega psk)
      <> toCBOR (pskIssuerVK psk)
      <> toCBOR (pskDelegateVK psk)
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
    , "pskIssuerPk" .= toJSON (pskIssuerVK psk)
    , "pskDelegatePk" .= toJSON (pskDelegateVK psk)
    , "pskCert" .= toJSON (pskCert psk)
    ]

instance FromJSON w => FromJSON (AProxyVerificationKey w ()) where
    parseJSON = withObject "ProxyVerificationKey" $ \v -> unsafeProxyVerificationKey
        <$> v .: "pskOmega"
        <*> v .: "pskIssuerPk"
        <*> v .: "pskDelegatePk"
        <*> v .: "pskCert"

-- | Creates proxy signing key
createPsk
  :: ToCBOR w
  => ProtocolMagicId
  -> SafeSigner
  -> VerificationKey
  -> w
  -> ProxyVerificationKey w
createPsk pm ss delegateVK w = UnsafeAProxyVerificationKey
  { aPskOmega     = Annotated w ()
  , pskIssuerVK   = safeToVerification ss
  , pskDelegateVK = delegateVK
  , pskCert       = safeCreateProxyCert pm ss delegateVK w
  }

-- | Checks if delegate and issuer fields of proxy signing key are equal
isSelfSignedPsk :: ProxyVerificationKey w -> Bool
isSelfSignedPsk psk = pskIssuerVK psk == pskDelegateVK psk

-- | Return the key if it's valid, and throw an error otherwise
validateProxyVerificationKey
  :: MonadError Text m
  => Annotated ProtocolMagicId ByteString
  -> AProxyVerificationKey w ByteString
  -> m ()
validateProxyVerificationKey pm psk =
  verifyProxyCert
      pm
      (pskIssuerVK psk)
      (pskDelegateVK psk)
      (aPskOmega psk)
      (pskCert psk)
    `orThrowError` "a ProxyVerificationKey has an invalid signature"
