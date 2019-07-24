{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Chain.Delegation.Certificate
  (
  -- * Certificate
    Certificate
  , ACertificate(..)

  -- * Certificate Constructors
  , mkCertificate
  , unsafeCertificate

  -- * Certificate Accessor
  , epoch

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
  ( Annotated(..)
  , ByteSpan
  , FromCBOR(..)
  , ToCBOR(..)
  , fromCBORAnnotated
  , encodeListLen
  , enforceSize
  , serialize'
  )
import Cardano.Chain.Slotting (EpochNumber)
import Cardano.Crypto
  ( ProtocolMagicId
  , SafeSigner
  , SignTag(SignCertificate)
  , Signature
  , VerificationKey(unVerificationKey)
  , safeSign
  , safeToVerification
  , verifySignatureDecoded
  )


--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

type Certificate = ACertificate ()

-- | Delegation certificate allowing the @delegateVK@ to sign blocks on behalf
--   of @issuerVK@
--
--   Each delegator can publish at most one 'Certificate' per 'EpochNumber', and
--   that 'EpochNumber' must correspond to the current or next 'EpochNumber' at
--   the time of publishing
data ACertificate a = UnsafeACertificate
  { aEpoch     :: Annotated EpochNumber a
  -- ^ The epoch from which the delegation is valid
  , issuerVK   :: VerificationKey
  -- ^ The issuer of the certificate, who delegates their right to sign blocks
  , delegateVK :: VerificationKey
  -- ^ The delegate, who gains the right to sign blocks
  , signature  :: Signature EpochNumber
  -- ^ The signature that proves the certificate was issued by @issuerVK@
  } deriving (Eq, Ord, Show, Generic, Functor)
    deriving anyclass NFData


--------------------------------------------------------------------------------
-- Certificate Constructors
--------------------------------------------------------------------------------

-- | Create a valid 'Certificate'
mkCertificate
  :: ProtocolMagicId
  -> SafeSigner
  -> VerificationKey
  -> EpochNumber
  -> Certificate
mkCertificate pm ss delegateVK e = UnsafeACertificate
  { aEpoch     = Annotated e ()
  , issuerVK   = safeToVerification ss
  , delegateVK = delegateVK
  , signature  = coerce sig
  }
 where
  sig = safeSign pm SignCertificate ss
    $ mconcat ["00", CC.unXPub (unVerificationKey delegateVK), serialize' e]

unsafeCertificate
  :: EpochNumber
  -> VerificationKey
  -- ^ The issuer of the certificate. See 'UnsafeACertificate'.
  -> VerificationKey
  -- ^ The delegate of the certificate. See 'UnsafeACertificate'.
  -> Signature EpochNumber
  -> Certificate
unsafeCertificate e = UnsafeACertificate (Annotated e ())


--------------------------------------------------------------------------------
-- Certificate Accessor
--------------------------------------------------------------------------------

epoch :: ACertificate a -> EpochNumber
epoch = unAnnotated . aEpoch


--------------------------------------------------------------------------------
-- Certificate Predicate
--------------------------------------------------------------------------------

-- | A 'Certificate' is valid if the 'Signature' is valid
isValid
  :: Annotated ProtocolMagicId ByteString
  -> ACertificate ByteString
  -> Bool
isValid pm UnsafeACertificate { aEpoch, issuerVK, delegateVK, signature } =
  verifySignatureDecoded
    pm
    SignCertificate
    issuerVK
    (   serialize'
    .   mappend ("00" <> CC.unXPub (unVerificationKey delegateVK))
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

instance FromCBOR Certificate where
  fromCBOR = void <$> fromCBOR @(ACertificate ByteSpan)

instance FromCBOR (ACertificate ByteSpan) where
  fromCBOR = do
    enforceSize "Delegation.Certificate" 4
    UnsafeACertificate
      <$> fromCBORAnnotated
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR


--------------------------------------------------------------------------------
-- Certificate Formatting
--------------------------------------------------------------------------------

instance B.Buildable (ACertificate a) where
  build (UnsafeACertificate e iVK dVK _) = bprint
    ( "Delegation.Certificate { w = " . build
    . ", iVK = " . build
    . ", dVK = " . build
    . " }"
    )
    (unAnnotated e)
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
    unsafeCertificate
      <$> (fromIntegral @Int54 <$> fromJSField obj "omega")
      <*> fromJSField obj "issuerPk"
      <*> fromJSField obj "delegatePk"
      <*> fromJSField obj "cert"
