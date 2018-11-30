{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Chain.Delegation.Certificate
  ( Certificate
  , ACertificate
  )
where

import Cardano.Prelude

import Text.JSON.Canonical
  (FromJSON(..), Int54, JSValue(..), ToJSON(..), fromJSField, mkObject)

import Cardano.Chain.Slotting (EpochIndex)
import Cardano.Crypto (AProxySecretKey(..), pskOmega, unsafeProxySecretKey)


-- | A delegation certificate is a `ProxySecretKey` tagged with an `EpochIndex`
type Certificate = ACertificate ()

type ACertificate a = AProxySecretKey EpochIndex a

instance Monad m => ToJSON m Certificate where
  toJSON psk = mkObject
    -- omega is encoded as a number, because in genesis we always set it to 0
    [ ("omega", pure (JSNum . fromIntegral $ pskOmega psk))
    , ("issuerPk"  , toJSON $ pskIssuerPk psk)
    , ("delegatePk", toJSON $ pskDelegatePk psk)
    , ("cert"      , toJSON $ pskCert psk)
    ]

instance MonadError SchemaError m => FromJSON m Certificate where
  fromJSON obj =
    unsafeProxySecretKey
      <$> (fromIntegral @Int54 <$> fromJSField obj "omega")
      <*> fromJSField obj "issuerPk"
      <*> fromJSField obj "delegatePk"
      <*> fromJSField obj "cert"
