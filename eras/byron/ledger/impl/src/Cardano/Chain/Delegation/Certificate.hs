{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Chain.Delegation.Certificate
  ( -- * Certificate
    Certificate,
    ACertificate (..),
    CertificateId,

    -- * Certificate Constructors
    signCertificate,
    unsafeCertificate,

    -- * Certificate Accessor
    epoch,
    recoverCertificateId,

    -- * Certificate Predicate
    isValid,
  )
where

import Cardano.Binary
  ( Annotated (Annotated, unAnnotated),
    ByteSpan,
    Decoded (..),
    FromCBOR (..),
    ToCBOR (..),
    annotatedDecoder,
    encodeListLen,
    enforceSize,
    fromCBORAnnotated,
    serialize',
  )
import Cardano.Chain.Slotting (EpochNumber)
import Cardano.Crypto
  ( Hash,
    ProtocolMagicId,
    SafeSigner,
    SignTag (SignCertificate),
    Signature,
    VerificationKey (unVerificationKey),
    hashDecoded,
    safeSign,
    safeToVerification,
    verifySignatureDecoded,
  )
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical
  ( FromJSON (..),
    Int54,
    JSValue (..),
    ToJSON (..),
    fromJSField,
    mkObject,
  )

--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

-- | A delegation certificate identifier (the 'Hash' of a 'Certificate').
type CertificateId = Hash Certificate

type Certificate = ACertificate ()

-- | Delegation certificate allowing the @delegateVK@ to sign blocks on behalf
--   of @issuerVK@
--
--   Each delegator can publish at most one 'Certificate' per 'EpochNumber', and
--   that 'EpochNumber' must correspond to the current or next 'EpochNumber' at
--   the time of publishing
data ACertificate a = UnsafeACertificate
  { -- | The epoch from which the delegation is valid
    aEpoch :: !(Annotated EpochNumber a),
    -- | The issuer of the certificate, who delegates their right to sign blocks
    issuerVK :: !VerificationKey,
    -- | The delegate, who gains the right to sign blocks
    delegateVK :: !VerificationKey,
    -- | The signature that proves the certificate was issued by @issuerVK@
    signature :: !(Signature EpochNumber),
    annotation :: !a
  }
  deriving (Eq, Ord, Show, Generic, Functor)
  deriving anyclass (NFData, NoThunks)

-- Used for debugging purposes only
instance Aeson.ToJSON a => Aeson.ToJSON (ACertificate a)

--------------------------------------------------------------------------------
-- Certificate Constructors
--------------------------------------------------------------------------------

-- | Create a 'Certificate', signing it with the provided safe signer.
signCertificate ::
  ProtocolMagicId ->
  VerificationKey ->
  EpochNumber ->
  SafeSigner ->
  Certificate
signCertificate protocolMagicId delegateVK epochNumber safeSigner =
  UnsafeACertificate
    { aEpoch = Annotated epochNumber (),
      issuerVK = safeToVerification safeSigner,
      delegateVK = delegateVK,
      signature = coerce sig,
      annotation = ()
    }
  where
    sig =
      safeSign protocolMagicId SignCertificate safeSigner $
        mconcat
          [ "00",
            CC.unXPub (unVerificationKey delegateVK),
            serialize' epochNumber
          ]

-- | Create a certificate using the provided signature.
unsafeCertificate ::
  EpochNumber ->
  -- | The issuer of the certificate. See 'UnsafeACertificate'.
  VerificationKey ->
  -- | The delegate of the certificate. See 'UnsafeACertificate'.
  VerificationKey ->
  Signature EpochNumber ->
  Certificate
unsafeCertificate e ivk dvk sig = UnsafeACertificate (Annotated e ()) ivk dvk sig ()

--------------------------------------------------------------------------------
-- Certificate Accessor
--------------------------------------------------------------------------------

epoch :: ACertificate a -> EpochNumber
epoch = unAnnotated . aEpoch

recoverCertificateId :: ACertificate ByteString -> CertificateId
recoverCertificateId = hashDecoded

--------------------------------------------------------------------------------
-- Certificate Predicate
--------------------------------------------------------------------------------

-- | A 'Certificate' is valid if the 'Signature' is valid
isValid ::
  Annotated ProtocolMagicId ByteString ->
  ACertificate ByteString ->
  Bool
isValid pm UnsafeACertificate {aEpoch, issuerVK, delegateVK, signature} =
  verifySignatureDecoded
    pm
    SignCertificate
    issuerVK
    ( serialize'
        . mappend ("00" <> CC.unXPub (unVerificationKey delegateVK))
        <$> aEpoch
    )
    signature

--------------------------------------------------------------------------------
-- Certificate Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Certificate where
  toCBOR cert =
    encodeListLen 4
      <> toCBOR (epoch cert)
      <> toCBOR (issuerVK cert)
      <> toCBOR (delegateVK cert)
      <> toCBOR (signature cert)

  encodedSizeExpr size cert =
    1
      + encodedSizeExpr size (epoch <$> cert)
      + encodedSizeExpr size (issuerVK <$> cert)
      + encodedSizeExpr size (delegateVK <$> cert)
      + encodedSizeExpr size (signature <$> cert)

instance FromCBOR Certificate where
  fromCBOR = void <$> fromCBOR @(ACertificate ByteSpan)

instance FromCBOR (ACertificate ByteSpan) where
  fromCBOR = do
    Annotated (e, ivk, dvk, sig) byteSpan <- annotatedDecoder $ do
      enforceSize "Delegation.Certificate" 4
      (,,,)
        <$> fromCBORAnnotated
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
    pure $ UnsafeACertificate e ivk dvk sig byteSpan

instance Decoded (ACertificate ByteString) where
  type BaseType (ACertificate ByteString) = Certificate
  recoverBytes = annotation

--------------------------------------------------------------------------------
-- Certificate Formatting
--------------------------------------------------------------------------------

instance B.Buildable (ACertificate a) where
  build (UnsafeACertificate e iVK dVK _ _) =
    bprint
      ( "Delegation.Certificate { w = " . build
          . ", iVK = "
          . build
          . ", dVK = "
          . build
          . " }"
      )
      (unAnnotated e)
      iVK
      dVK

--------------------------------------------------------------------------------
-- Certificate Canonical JSON
--------------------------------------------------------------------------------

instance Monad m => ToJSON m Certificate where
  toJSON cert =
    mkObject
      -- omega is encoded as a number, because in genesis we always set it to 0
      [ ("omega", pure (JSNum . fromIntegral $ epoch cert)),
        ("issuerPk", toJSON $ issuerVK cert),
        ("delegatePk", toJSON $ delegateVK cert),
        ("cert", toJSON $ signature cert)
      ]

instance MonadError SchemaError m => FromJSON m Certificate where
  fromJSON obj =
    unsafeCertificate
      <$> (fromIntegral @Int54 <$> fromJSField obj "omega")
      <*> fromJSField obj "issuerPk"
      <*> fromJSField obj "delegatePk"
      <*> fromJSField obj "cert"
