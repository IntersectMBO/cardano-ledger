{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Crypto.Signing.Proxy.Signature
  ( AProxySignature(..)
  , ProxySignature
  , validateProxySignature
  , proxySign
  , proxyVerify
  , proxyVerifyDecoded
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Data.ByteArray (ScrubbedBytes)
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated
  , ByteSpan
  , Decoded(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  , serialize'
  )
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.Proxy.VerificationKey
  ( AProxyVerificationKey(..)
  , ProxyVerificationKey
  , pskOmega
  , validateProxyVerificationKey
  )
import Cardano.Crypto.Signing.VerificationKey (VerificationKey(..))
import Cardano.Crypto.Signing.SigningKey (SigningKey(..), toVerification)
import Cardano.Crypto.Signing.Signature (fromCBORXSignature, toCBORXSignature)
import Cardano.Crypto.Signing.Tag (SignTag, signTag, signTagDecoded)


-- | Delegate signature made with certificate-based permission. @w@ stays for
--   message type used in proxy (ω in the implementation notes), @a@ for type of
--   message signed.
--
--   We add whole psk as a field because otherwise we can't verify sig in
--   heavyweight psk transitive delegation: i → x → d, we have psk from x to d,
--   slot leader is i.
type ProxySignature w s = AProxySignature w s ()

data AProxySignature w s a = AProxySignature
  { psigPsk :: AProxyVerificationKey w a
  , psigSig :: CC.XSignature
  } deriving (Eq, Ord, Show, Generic, Functor)
    deriving anyclass NFData


instance B.Buildable w => B.Buildable (AProxySignature w s a) where
  build psig = bprint ("Proxy signature { psk = " . build . " }") (psigPsk psig)

instance (Typeable s, ToCBOR w) => ToCBOR (ProxySignature w s) where
  toCBOR psig =
    encodeListLen 2 <> toCBOR (psigPsk psig) <> toCBORXSignature (psigSig psig)

instance (Typeable s, FromCBOR w) => FromCBOR (ProxySignature w s) where
  fromCBOR = void <$> fromCBOR @(AProxySignature w s ByteSpan)

instance (Typeable s, FromCBOR w) => FromCBOR (AProxySignature w s ByteSpan) where
  fromCBOR =
    AProxySignature
      <$  enforceSize "ProxySignature" 2
      <*> fromCBOR
      <*> fromCBORXSignature

validateProxySignature
  :: MonadError Text m
  => Annotated ProtocolMagicId ByteString
  -> AProxySignature w a ByteString
  -> m ()
validateProxySignature pm psig = validateProxyVerificationKey pm (psigPsk psig)


-- | Make a proxy delegate signature with help of certificate. If the delegate
--   signing key passed doesn't pair with delegate verification key in certificate
--   inside, we panic. Please check this condition outside of this function.
proxySign
  :: ToCBOR a
  => ProtocolMagicId
  -> SignTag
  -> SigningKey
  -> ProxyVerificationKey w
  -> a
  -> ProxySignature w a
proxySign pm t sk@(SigningKey delegateSk) psk m
  | toVerification sk /= pskDelegateVK psk = panic $ sformat
    ( "proxySign called with irrelevant certificate "
    . "(psk delegateVK: "
    . build
    . ", real delegate vk: "
    . build
    . ")"
    )
    (pskDelegateVK psk)
    (toVerification sk)
  | otherwise = AProxySignature {psigPsk = psk, psigSig = sigma}
 where
  VerificationKey issuerVK = pskIssuerVK psk
  -- It's safe to put the tag after issuerVK because `CC.unXPub issuerVK` always
  -- takes 64 bytes
  sigma = CC.sign (mempty :: ScrubbedBytes) delegateSk
    $ mconcat ["01", CC.unXPub issuerVK, signTag pm t, serialize' m]

-- | Verify delegated signature given issuer's vk, signature, message
--   space predicate and message itself.
proxyVerifyDecoded
  :: Decoded t
  => Annotated ProtocolMagicId ByteString
  -> SignTag
  -> (w -> Bool)
  -> t
  -> ProxySignature w (BaseType t)
  -> Bool
proxyVerifyDecoded pm t omegaPred m psig = predCorrect && sigValid
 where
  psk         = psigPsk psig
  VerificationKey issuerVK        = pskIssuerVK psk
  VerificationKey pdDelegateVKRaw = pskDelegateVK psk
  predCorrect = omegaPred (pskOmega psk)
  sigValid    = CC.verify
    pdDelegateVKRaw
    (mconcat ["01", CC.unXPub issuerVK, signTagDecoded pm t, recoverBytes m])
    (psigSig psig)

-- | Verify delegated signature given issuer's vk, signature, message space
--   predicate and message
proxyVerify
  :: ToCBOR a
  => ProtocolMagicId
  -> SignTag
  -> (w -> Bool)
  -> a
  -> ProxySignature w a
  -> Bool
proxyVerify pm t omegaPred m psig = predCorrect && sigValid
 where
  psk         = psigPsk psig
  VerificationKey issuerVK        = pskIssuerVK psk
  VerificationKey pdDelegateVKRaw = pskDelegateVK psk
  predCorrect = omegaPred (pskOmega psk)
  sigValid    = CC.verify
    pdDelegateVKRaw
    (mconcat ["01", CC.unXPub issuerVK, signTag pm t, serialize' m])
    (psigSig psig)
