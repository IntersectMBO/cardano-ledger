{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Chain.Delegation.Certificate
  (
  -- * Certificate
    Certificate(UnsafeCertificate, epoch, issuerVK, delegateVK, signature, serialize)
  , CertificateId

  -- * Certificate Constructors
  , signCertificate

  -- * Certificate Accessor
  , certificateId

  -- * Certificate Predicate
  , isValid
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Data.Coerce (coerce)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical
  (FromJSON(..), Int54, JSValue(..), ToJSON(..), fromJSField, mkObject)

import Cardano.Binary
  ( Annotated(Annotated, unAnnotated)
  , FromCBOR(..)
  , ToCBOR(..)
  , FromCBORAnnotated (..)
  , encodeListLen
  , enforceSize
  , serialize'
  , encodePreEncoded
  , serializeEncoding'
  , withSlice'
  )
import Cardano.Chain.Slotting (EpochNumber)
import Cardano.Crypto
  ( Hash
  , ProtocolMagicId
  , SafeSigner
  , SignTag(SignCertificate)
  , Signature
  , VerificationKey(unVerificationKey)
  , hash
  , safeSign
  , safeToVerification
  , verifySignatureDecoded
  )


--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

-- | A delegation certificate identifier (the 'Hash' of a 'Certificate').
type CertificateId = Hash Certificate

-- | Delegation certificate allowing the @delegateVK@ to sign blocks on behalf
--   of @issuerVK@
--
--   Each delegator can publish at most one 'Certificate' per 'EpochNumber', and
--   that 'EpochNumber' must correspond to the current or next 'EpochNumber' at
--   the time of publishing
data Certificate = UnsafeCertificate'
  { aEpoch     :: !(Annotated EpochNumber ByteString)
  -- ^ The epoch from which the delegation is valid
  , issuerVK'  :: !VerificationKey
  -- ^ The issuer of the certificate, who delegates their right to sign blocks
  , delegateVK':: !VerificationKey
  -- ^ The delegate, who gains the right to sign blocks
  , signature' :: !(Signature EpochNumber)
  -- ^ The signature that proves the certificate was issued by @issuerVK@
  , serialize  :: ByteString
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)
    deriving NoUnexpectedThunks via AllowThunksIn '["serialize"] Certificate

--------------------------------------------------------------------------------
-- Certificate Constructors
--------------------------------------------------------------------------------

-- | Create a certificate using the provided signature.
{-# COMPLETE UnsafeCertificate #-}
pattern UnsafeCertificate
  :: EpochNumber
  -> VerificationKey
  -- ^ The issuer of the certificate.
  -> VerificationKey
  -- ^ The delegate of the certificate.
  -> Signature EpochNumber
  -> Certificate
pattern UnsafeCertificate { epoch, issuerVK, delegateVK, signature } <-
    UnsafeCertificate' (unAnnotated -> epoch) issuerVK delegateVK signature _
  where
  UnsafeCertificate epochNumber ivk dvk sig =
    let serialize =  serializeEncoding' $ encodeListLen 4
          <> encodePreEncoded epochNumberBytes
          <> toCBOR ivk
          <> toCBOR dvk
          <> toCBOR sig
        epochNumberBytes = serialize' epochNumber
        aEpoch = Annotated epochNumber epochNumberBytes
    in UnsafeCertificate' aEpoch ivk dvk sig serialize

-- | Create a 'Certificate', signing it with the provided safe signer.
signCertificate
  :: ProtocolMagicId
  -> VerificationKey
  -> EpochNumber
  -> SafeSigner
  -> Certificate
signCertificate protocolMagicId dvk epochNumber safeSigner =
  UnsafeCertificate epochNumber ivk dvk (coerce sig)
  where
  ivk   = safeToVerification safeSigner
  sig = safeSign protocolMagicId SignCertificate safeSigner
    $ mconcat [ "00"
              , CC.unXPub (unVerificationKey dvk)
              , serialize' epochNumber]


--------------------------------------------------------------------------------
-- Certificate Accessor
--------------------------------------------------------------------------------

certificateId :: Certificate -> CertificateId
certificateId = hash

--------------------------------------------------------------------------------
-- Certificate Predicate
--------------------------------------------------------------------------------

-- | A 'Certificate' is valid if the 'Signature' is valid
isValid
  :: ProtocolMagicId
  -> Certificate
  -> Bool
isValid pm cert =
  verifySignatureDecoded
    pm
    SignCertificate
    (issuerVK cert)
    (   serialize'
    .   mappend ("00" <> CC.unXPub (unVerificationKey $ delegateVK cert))
    <$> (aEpoch cert)
    )
    (signature cert)


--------------------------------------------------------------------------------
-- Certificate Binary Serialization
--------------------------------------------------------------------------------

instance ToCBOR Certificate where
  toCBOR = encodePreEncoded . serialize

instance FromCBORAnnotated Certificate where
  fromCBORAnnotated' = withSlice' $
    UnsafeCertificate' <$ lift (enforceSize "Delegation.Certificate" 4)
      <*> fromCBORAnnotated'
      <*> lift fromCBOR
      <*> lift fromCBOR
      <*> lift fromCBOR


--------------------------------------------------------------------------------
-- Certificate Formatting
--------------------------------------------------------------------------------

instance B.Buildable Certificate where
  build (UnsafeCertificate e iVK dVK _) = bprint
    ( "Delegation.Certificate { w = " . build
    . ", iVK = " . build
    . ", dVK = " . build
    . " }"
    )
    e
    iVK
    dVK


--------------------------------------------------------------------------------
-- Certificate Canonical JSON
--------------------------------------------------------------------------------

instance Monad m => ToJSON m Certificate where
  toJSON cert = mkObject
    -- omega is encoded as a number, because in genesis we always set it to 0
    [ ("omega", pure (JSNum . fromIntegral $ epoch cert))
    , ("issuerPk"  , toJSON $ issuerVK cert)
    , ("delegatePk", toJSON $ delegateVK cert)
    , ("cert"      , toJSON $ signature cert)
    ]

instance MonadError SchemaError m => FromJSON m Certificate where
  fromJSON obj =
    UnsafeCertificate
      <$> (fromIntegral @Int54 <$> fromJSField obj "omega")
      <*> fromJSField obj "issuerPk"
      <*> fromJSField obj "delegatePk"
      <*> fromJSField obj "cert"
