{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Delegation.Payload
  ( Payload(..)
  , PayloadError(..)
  , checkPayload
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError, liftEither)
import Formatting (bprint, int, stext)
import Formatting.Buildable (Buildable(..))

import Cardano.Binary.Class (Bi(..))
import Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKHeavy)
import Cardano.Crypto (ProtocolMagic, validateProxySecretKey)


-- | 'Payload' is put into 'MainBlock' and is a set of heavyweight proxy signing
--   keys. List of psk issuers should be unique also.
newtype Payload = UnsafePayload
  { getPayload :: [ProxySKHeavy]
  } deriving (Show, Eq, Generic, NFData)

instance Buildable Payload where
  build (UnsafePayload psks) = bprint
    ("proxy signing keys (" . int . " items): " . listJson . "\n")
    (length psks)
    psks

instance Bi Payload where
  encode = encode . getPayload
  decode = UnsafePayload <$> decode

data PayloadError = PayloadPSKError Text

instance Buildable PayloadError where
  build = \case
    PayloadPSKError err -> bprint
      ( "ProxySecretKey invalid when checing Delegation.Payload.\n Error: "
      . stext
      )
      err

checkPayload :: MonadError PayloadError m => ProtocolMagic -> Payload -> m ()
checkPayload protocolMagic (UnsafePayload proxySKs) = forM_
  proxySKs
  (liftEither . first PayloadPSKError . validateProxySecretKey protocolMagic)
  -- unless (allDistinct $ map pskIssuerPk proxySKs) $
  --     throwError "Some of block's PSKs have the same issuer, which is prohibited"
