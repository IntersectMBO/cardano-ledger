{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Crypto.Signing.Signature
  ( -- * Signature
    Signature (..),
    toCBORXSignature,
    fromCBORXSignature,
    fullSignatureHexF,
    parseFullSignature,

    -- * Signing
    sign,
    signEncoded,
    signRaw,
    safeSign,
    safeSignRaw,

    -- * Verification
    verifySignature,
    verifySignatureDecoded,
    verifySignatureRaw,
  )
where

import Cardano.Binary
  ( Annotated (..),
    Decoded (..),
    Decoder,
    Encoding,
    FromCBOR (..),
    Raw,
    ToCBOR (..),
    serialize',
    serializeEncoding,
  )
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.Safe
  ( PassPhrase (..),
    SafeSigner (..),
  )
import Cardano.Crypto.Signing.SigningKey (SigningKey (..))
import Cardano.Crypto.Signing.Tag (SignTag (..), signTag, signTagDecoded)
import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..))
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Text.Encoding as T
import Formatting (Format, bprint, formatToString, later, sformat, shown, stext)
import qualified Formatting.Buildable as B
import NoThunks.Class (InspectHeap (..), NoThunks (..))
import Text.JSON.Canonical (JSValue (..), toJSString)
import qualified Text.JSON.Canonical as TJC (FromJSON (..), ToJSON (..))

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

-- | Wrapper around 'CC.XSignature'
newtype Signature a
  = Signature CC.XSignature
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
  deriving (NoThunks) via InspectHeap CC.XSignature

instance B.Buildable (Signature a) where
  build _ = "<signature>"

instance FromJSON (Signature w) where
  parseJSON v = parseJSON v >>= toAesonError . parseFullSignature

instance ToJSON (Signature w) where
  toJSON = toJSON . sformat fullSignatureHexF

instance Monad m => TJC.ToJSON m (Signature w) where
  toJSON = pure . JSString . toJSString . formatToString fullSignatureHexF

instance (Typeable x, MonadError SchemaError m) => TJC.FromJSON m (Signature x) where
  fromJSON = parseJSString parseFullSignature

-- | Formatter for 'Signature' to show it in hex.
fullSignatureHexF :: Format r (Signature a -> r)
fullSignatureHexF =
  later $ \(Signature x) -> base16Builder . CC.unXSignature $ x

data SignatureParseError
  = SignatureParseBase16Error ByteString
  | SignatureParseXSignatureError Text
  deriving (Eq, Show)

instance B.Buildable SignatureParseError where
  build = \case
    SignatureParseBase16Error bs ->
      bprint
        ("Failed to parse base 16 while parsing Signature.\n Error: " . shown)
        bs
    SignatureParseXSignatureError err ->
      bprint
        ( "Failed to construct XSignature while parsing Signature.\n Error: "
            . stext
        )
        err

-- | Parse 'Signature' from base16 encoded string.
parseFullSignature :: Text -> Either SignatureParseError (Signature a)
parseFullSignature s = do
  let bs = T.encodeUtf8 s
  b <- first (const (SignatureParseBase16Error bs)) $ B16.decode bs
  Signature <$> first (SignatureParseXSignatureError . toS) (CC.xsignature b)

toCBORXSignature :: CC.XSignature -> Encoding
toCBORXSignature a = toCBOR $ CC.unXSignature a

fromCBORXSignature :: Decoder s CC.XSignature
fromCBORXSignature = toCborError . CC.xsignature =<< fromCBOR

instance Typeable a => ToCBOR (Signature a) where
  toCBOR (Signature a) = toCBORXSignature a
  encodedSizeExpr _ _ = 66

instance Typeable a => FromCBOR (Signature a) where
  fromCBOR = fmap Signature fromCBORXSignature

--------------------------------------------------------------------------------
-- Signing
--------------------------------------------------------------------------------

-- | Encode something with 'ToCBOR' and sign it
sign ::
  ToCBOR a =>
  ProtocolMagicId ->
  -- | See docs for 'SignTag'
  SignTag ->
  SigningKey ->
  a ->
  Signature a
sign pm tag sk = signEncoded pm tag sk . toCBOR

-- | Like 'sign' but without the 'ToCBOR' constraint
signEncoded ::
  ProtocolMagicId -> SignTag -> SigningKey -> Encoding -> Signature a
signEncoded pm tag sk = coerce . signRaw pm (Just tag) sk . BSL.toStrict . serializeEncoding

-- | Sign a 'Raw' bytestring
signRaw ::
  ProtocolMagicId ->
  -- | See docs for 'SignTag'. Unlike in 'sign', we allow no tag to be provided
  --   just in case you need to sign /exactly/ the bytestring you provided.
  Maybe SignTag ->
  SigningKey ->
  ByteString ->
  Signature Raw
signRaw pm mTag (SigningKey sk) x =
  Signature
    (CC.sign (mempty :: ScrubbedBytes) sk (tag <> x))
  where
    tag = maybe mempty (signTag pm) mTag

safeSign ::
  ToCBOR a => ProtocolMagicId -> SignTag -> SafeSigner -> a -> Signature a
safeSign pm t ss = coerce . safeSignRaw pm (Just t) ss . serialize'

safeSignRaw ::
  ProtocolMagicId ->
  Maybe SignTag ->
  SafeSigner ->
  ByteString ->
  Signature Raw
safeSignRaw pm mbTag (SafeSigner (SigningKey sk) (PassPhrase pp)) x =
  Signature (CC.sign pp sk (tag <> x))
  where
    tag = maybe mempty (signTag pm) mbTag

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

-- | Verify a signature
verifySignature ::
  (a -> Encoding) ->
  ProtocolMagicId ->
  SignTag ->
  VerificationKey ->
  a ->
  Signature a ->
  Bool
verifySignature toEnc pm tag vk x sig =
  verifySignatureRaw vk (signTag pm tag <> (BSL.toStrict . serializeEncoding $ toEnc x)) (coerce sig)

-- | Verify a signature
verifySignatureDecoded ::
  Decoded t =>
  Annotated ProtocolMagicId ByteString ->
  SignTag ->
  VerificationKey ->
  t ->
  Signature (BaseType t) ->
  Bool
verifySignatureDecoded pm tag vk x sig =
  verifySignatureRaw vk (signTagDecoded pm tag <> recoverBytes x) (coerce sig)

-- | Verify 'Raw' signature
verifySignatureRaw ::
  VerificationKey ->
  ByteString ->
  Signature Raw ->
  Bool
verifySignatureRaw (VerificationKey k) x (Signature sig) = CC.verify k x sig
