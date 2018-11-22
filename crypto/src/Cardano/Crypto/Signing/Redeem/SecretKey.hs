{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Cardano.Crypto.Signing.Redeem.SecretKey
  ( RedeemSecretKey(..)
  , redeemToPublic
  )
where

import Cardano.Prelude

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Formatting (bprint)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi)
import Cardano.Crypto.Signing.Redeem.PublicKey
  (RedeemPublicKey(..), redeemPkB64F)


-- | Wrapper around 'Ed25519.SecretKey'.
newtype RedeemSecretKey =
  RedeemSecretKey Ed25519.SecretKey
  deriving (Eq, Ord, Show, Generic, NFData)

deriving instance Bi RedeemSecretKey

instance B.Buildable RedeemSecretKey where
  build = bprint ("redeem_sec_of_pk:" . redeemPkB64F) . redeemToPublic

-- | Public key derivation function.
redeemToPublic :: RedeemSecretKey -> RedeemPublicKey
redeemToPublic (RedeemSecretKey k) = RedeemPublicKey (Ed25519.toPublic k)
