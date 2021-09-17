{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Crypto.Signing.Redeem.Signature
  ( RedeemSignature (..),
    redeemSign,
    redeemSignRaw,
    verifyRedeemSig,
    verifyRedeemSigDecoded,
    verifyRedeemSigRaw,
  )
where

import Cardano.Binary
  ( Annotated,
    Decoded (..),
    FromCBOR,
    Raw,
    ToCBOR,
    serialize',
  )
import Cardano.Crypto.Orphans ()
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.Redeem.SigningKey (RedeemSigningKey (..))
import Cardano.Crypto.Signing.Redeem.VerificationKey (RedeemVerificationKey (..))
import Cardano.Crypto.Signing.Tag (SignTag, signTag, signTagDecoded)
import Cardano.Prelude
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Coerce (coerce)
import qualified Formatting.Buildable as B (Buildable (..))

-- | Wrapper around 'Ed25519.Signature'
newtype RedeemSignature a
  = RedeemSignature Ed25519.Signature
  deriving (Eq, Show, Generic, NFData, FromCBOR, ToCBOR)

-- Note that there is deliberately no Ord instance. The crypto libraries
-- encourage using key /hashes/ not keys for things like sets, map etc.

instance B.Buildable (RedeemSignature a) where
  build _ = "<redeem signature>"

deriveJSON defaultOptions ''RedeemSignature

-- | Encode something with 'ToCBOR' and sign it
redeemSign ::
  ToCBOR a =>
  ProtocolMagicId ->
  SignTag ->
  RedeemSigningKey ->
  a ->
  RedeemSignature a
redeemSign pm tag k = coerce . redeemSignRaw pm (Just tag) k . serialize'

-- | Alias for constructor
redeemSignRaw ::
  ProtocolMagicId ->
  Maybe SignTag ->
  RedeemSigningKey ->
  ByteString ->
  RedeemSignature Raw
redeemSignRaw pm mbTag (RedeemSigningKey k) x =
  RedeemSignature $ Ed25519.sign k (Ed25519.toPublic k) $ tag <> x
  where
    tag = maybe mempty (signTag pm) mbTag

-- | Verify a redeem signature
verifyRedeemSig ::
  ToCBOR a =>
  ProtocolMagicId ->
  SignTag ->
  RedeemVerificationKey ->
  a ->
  RedeemSignature a ->
  Bool
verifyRedeemSig pm tag k x s =
  verifyRedeemSigRaw k (signTag pm tag <> serialize' x) (coerce s)

verifyRedeemSigDecoded ::
  Decoded t =>
  Annotated ProtocolMagicId ByteString ->
  SignTag ->
  RedeemVerificationKey ->
  t ->
  RedeemSignature (BaseType t) ->
  Bool
verifyRedeemSigDecoded pm tag k x s =
  verifyRedeemSigRaw k (signTagDecoded pm tag <> recoverBytes x) (coerce s)

-- | Verify raw 'ByteString'
verifyRedeemSigRaw ::
  RedeemVerificationKey ->
  ByteString ->
  RedeemSignature Raw ->
  Bool
verifyRedeemSigRaw (RedeemVerificationKey k) x (RedeemSignature s) =
  Ed25519.verify k x s
