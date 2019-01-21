{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Crypto.Signing.Redeem.Signature
  ( RedeemSignature(..)
  , redeemSign
  , redeemSignRaw
  , verifyRedeemSig
  , verifyRedeemSigDecoded
  , verifyRedeemSigRaw
  )
where

import Cardano.Prelude

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Coerce (coerce)
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary.Class (Bi, Decoded(..), Raw, serialize')
import Cardano.Crypto.Orphans ()
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.Redeem.PublicKey (RedeemPublicKey(..))
import Cardano.Crypto.Signing.Redeem.SecretKey (RedeemSecretKey(..))
import Cardano.Crypto.Signing.Tag (SignTag, signTag)


-- | Wrapper around 'Ed25519.Signature'
newtype RedeemSignature a =
  RedeemSignature Ed25519.Signature
  deriving (Eq, Ord, Show, Generic, NFData, Bi)

instance B.Buildable (RedeemSignature a) where
  build _ = "<redeem signature>"

deriveJSON defaultOptions ''RedeemSignature

-- | Encode something with 'Bi' and sign it
redeemSign
  :: Bi a
  => ProtocolMagicId
  -> SignTag
  -> RedeemSecretKey
  -> a
  -> RedeemSignature a
redeemSign pm tag k = coerce . redeemSignRaw pm (Just tag) k . serialize'

-- | Alias for constructor
redeemSignRaw
  :: ProtocolMagicId
  -> Maybe SignTag
  -> RedeemSecretKey
  -> ByteString
  -> RedeemSignature Raw
redeemSignRaw pm mbTag (RedeemSecretKey k) x =
  RedeemSignature $ Ed25519.sign k (Ed25519.toPublic k) $ tag <> x
  where tag = maybe mempty (signTag pm) mbTag

-- | Verify a redeem signature
verifyRedeemSig
  :: Bi a
  => ProtocolMagicId
  -> SignTag
  -> RedeemPublicKey
  -> a
  -> RedeemSignature a
  -> Bool
verifyRedeemSig pm tag k x s =
  verifyRedeemSigRaw pm (Just tag) k (serialize' x) (coerce s)

verifyRedeemSigDecoded
  :: Decoded t
  => ProtocolMagicId
  -> SignTag
  -> RedeemPublicKey
  -> t
  -> RedeemSignature (BaseType t)
  -> Bool
verifyRedeemSigDecoded pm tag k x s =
  verifyRedeemSigRaw pm (Just tag) k (recoverBytes x) (coerce s)

-- | Verify raw 'ByteString'
verifyRedeemSigRaw
  :: ProtocolMagicId
  -> Maybe SignTag
  -> RedeemPublicKey
  -> ByteString
  -> RedeemSignature Raw
  -> Bool
verifyRedeemSigRaw pm mbTag (RedeemPublicKey k) x (RedeemSignature s) =
  Ed25519.verify k (tag <> x) s
  where tag = maybe mempty (signTag pm) mbTag
