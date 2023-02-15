{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Cardano.Crypto.Signing.Redeem.SigningKey (
  RedeemSigningKey (..),
  redeemToVerification,
)
where

import Cardano.Crypto.Signing.Redeem.VerificationKey (
  RedeemVerificationKey (..),
  redeemVKB64F,
 )
import Cardano.Ledger.Binary (
  DecCBOR,
  EncCBOR,
  FromCBOR (..),
  ToCBOR (..),
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Formatting (bprint)
import qualified Formatting.Buildable as B
import NoThunks.Class (InspectHeap (..), NoThunks (..))

-- | Wrapper around 'Ed25519.SecretKey'.
type RedeemSigningKey :: Type
newtype RedeemSigningKey
  = RedeemSigningKey Ed25519.SecretKey
  deriving (Eq, Show, Generic, NFData, DecCBOR, EncCBOR)
  deriving (NoThunks) via InspectHeap RedeemSigningKey

instance ToCBOR RedeemSigningKey where
  toCBOR = toByronCBOR

instance FromCBOR RedeemSigningKey where
  fromCBOR = fromByronCBOR

-- Note that there is deliberately no Ord instance. The crypto libraries
-- encourage using key /hashes/ not keys for things like sets, map etc.

instance B.Buildable RedeemSigningKey where
  build = bprint ("redeem_sec_of_vk:" . redeemVKB64F) . redeemToVerification

-- | Verification key derivation function.
redeemToVerification :: RedeemSigningKey -> RedeemVerificationKey
redeemToVerification (RedeemSigningKey k) = RedeemVerificationKey (Ed25519.toPublic k)
