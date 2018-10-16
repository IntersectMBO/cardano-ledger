{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Delegation.Payload
       ( Payload (..)
       , checkPayload
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError)
import           Formatting (bprint, int, (%))
import           Formatting.Buildable (Buildable (..))

import           Cardano.Binary.Class (Bi (..))
import           Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKHeavy)
import           Cardano.Crypto (ProtocolMagic, validateProxySecretKey)


-- | 'Payload' is put into 'MainBlock' and is a set of heavyweight proxy signing
--   keys. List of psk issuers should be unique also.
newtype Payload = UnsafePayload
  { getPayload :: [ProxySKHeavy]
  } deriving (Show, Eq, Generic, NFData)

instance Buildable Payload where
  build (UnsafePayload psks) = bprint
    ("proxy signing keys (" % int % " items): " % listJson % "\n")
    (length psks)
    psks

instance Bi Payload where
  encode = encode . getPayload
  decode = UnsafePayload <$> decode

checkPayload :: MonadError Text m => ProtocolMagic -> Payload -> m ()
checkPayload protocolMagic (UnsafePayload proxySKs) =
  -- unless (allDistinct $ map pskIssuerPk proxySKs) $
  --     throwError "Some of block's PSKs have the same issuer, which is prohibited"
  forM_ proxySKs (validateProxySecretKey protocolMagic)
