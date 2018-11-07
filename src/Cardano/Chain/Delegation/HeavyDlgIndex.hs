{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Cardano.Chain.Delegation.HeavyDlgIndex
  ( HeavyDlgIndex(..)
  , ProxySigHeavy
  , ProxySKHeavy
  , AProxySKHeavy
  , ProxySKBlockInfo
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import qualified Data.Aeson as Aeson (FromJSON(..), ToJSON(..))
import Formatting (bprint, build)
import Formatting.Buildable (Buildable(..))
import Text.JSON.Canonical
  (FromJSON(..), Int54, JSValue(..), ToJSON(..), fromJSField, mkObject)

import Cardano.Binary.Class (Bi(..))
import Cardano.Chain.Slotting (EpochIndex)
import Cardano.Crypto
  ( AProxySecretKey(..)
  , ProxySignature
  , PublicKey
  , pskOmega
  , unsafeProxySecretKey
  )


-- | Witness for heavy delegation signature -- epoch in which certificate starts
--   being active. It is needed for replay attack prevention (index should match
--   epoch of the block PSK is announced in).
newtype HeavyDlgIndex = HeavyDlgIndex
  { getHeavyDlgIndex :: EpochIndex
  } deriving (Show, Eq, Ord, Generic, NFData)

instance Buildable HeavyDlgIndex where
  build (HeavyDlgIndex i) = bprint Formatting.build i

instance Bi HeavyDlgIndex where
  encode = encode . getHeavyDlgIndex
  decode = HeavyDlgIndex <$> decode

instance Aeson.FromJSON HeavyDlgIndex where
  parseJSON v = HeavyDlgIndex <$> Aeson.parseJSON v

instance Aeson.ToJSON HeavyDlgIndex where
  toJSON = Aeson.toJSON . getHeavyDlgIndex

-- | Simple proxy signature without ttl/epoch index constraints
type ProxySigHeavy a = ProxySignature HeavyDlgIndex a

-- | Heavy delegation PSK
type ProxySKHeavy = AProxySKHeavy ()

-- | Heavy delegation PSK
type AProxySKHeavy a = AProxySecretKey HeavyDlgIndex a

instance Monad m => ToJSON m ProxySKHeavy where
  toJSON psk = mkObject
    -- omega is encoded as a number, because in genesis we always set it to 0
    [ ("omega", pure (JSNum . fromIntegral . getHeavyDlgIndex $ pskOmega psk))
    , ("issuerPk"  , toJSON $ pskIssuerPk psk)
    , ("delegatePk", toJSON $ pskDelegatePk psk)
    , ("cert"      , toJSON $ pskCert psk)
    ]

instance MonadError SchemaError m => FromJSON m ProxySKHeavy where
  fromJSON obj =
    unsafeProxySecretKey
      <$> (HeavyDlgIndex . fromIntegral @Int54 <$> fromJSField obj "omega")
      <*> fromJSField obj "issuerPk"
      <*> fromJSField obj "delegatePk"
      <*> fromJSField obj "cert"

-- | Heavyweight PSK with real leader public key (because heavyweight psks have
--   redelegation feature, so pskIssuerPk hPsk /= leader in general case). This
--   is used to create a block header only.
type ProxySKBlockInfo = Maybe (ProxySKHeavy, PublicKey)
