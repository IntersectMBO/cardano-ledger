{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Crypto.Signing.Redeem.SigningKey
  ( RedeemSigningKey (..),
    redeemToVerification,
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Crypto.Signing.Redeem.VerificationKey
  ( RedeemVerificationKey (..),
    redeemVKB64F,
  )
import Cardano.Prelude
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Formatting (bprint)
import qualified Formatting.Buildable as B
import NoThunks.Class (InspectHeap (..), NoThunks (..))

-- | Wrapper around 'Ed25519.SecretKey'.
newtype RedeemSigningKey
  = RedeemSigningKey Ed25519.SecretKey
  deriving (Eq, Show, Generic, NFData, FromCBOR, ToCBOR)
  deriving (NoThunks) via InspectHeap RedeemSigningKey

-- Note that there is deliberately no Ord instance. The crypto libraries
-- encourage using key /hashes/ not keys for things like sets, map etc.

instance B.Buildable RedeemSigningKey where
  build = bprint ("redeem_sec_of_vk:" . redeemVKB64F) . redeemToVerification

-- | Verification key derivation function.
redeemToVerification :: RedeemSigningKey -> RedeemVerificationKey
redeemToVerification (RedeemSigningKey k) = RedeemVerificationKey (Ed25519.toPublic k)
