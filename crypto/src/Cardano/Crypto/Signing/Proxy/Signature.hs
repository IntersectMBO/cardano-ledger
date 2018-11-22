{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
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

import Cardano.Binary.Class
  ( Bi(..)
  , ByteSpan
  , Decoded(..)
  , Decoder
  , encodeListLen
  , enforceSize
  , serialize'
  )
import Cardano.Crypto.ProtocolMagic (ProtocolMagic)
import Cardano.Crypto.Signing.Proxy.SecretKey
  ( AProxySecretKey(..)
  , ProxySecretKey
  , decodeAProxySecretKey
  , pskOmega
  , validateProxySecretKey
  )
import Cardano.Crypto.Signing.PublicKey (PublicKey(..))
import Cardano.Crypto.Signing.SecretKey (SecretKey(..), toPublic)
import Cardano.Crypto.Signing.Signature (decodeXSignature, encodeXSignature)
import Cardano.Crypto.Signing.Tag (SignTag, signTag)


-- | Delegate signature made with certificate-based permission. @w@ stays for
--   message type used in proxy (ω in the implementation notes), @a@ for type of
--   message signed.
--
--   We add whole psk as a field because otherwise we can't verify sig in
--   heavyweight psk transitive delegation: i → x → d, we have psk from x to d,
--   slot leader is i.
type ProxySignature w s = AProxySignature w s ()

data AProxySignature w s a = AProxySignature
  { psigPsk :: AProxySecretKey w a
  , psigSig :: CC.XSignature
  } deriving (Eq, Ord, Show, Generic, Functor)
    deriving anyclass NFData


instance B.Buildable w => B.Buildable (AProxySignature w s a) where
  build psig = bprint ("Proxy signature { psk = " . build . " }") (psigPsk psig)

instance (Typeable s, Bi w) => Bi (ProxySignature w s) where
  encode psig =
    encodeListLen 2 <> encode (psigPsk psig) <> encodeXSignature (psigSig psig)

  decode = void <$> decodeAProxySignature


decodeAProxySignature :: Bi w => Decoder s (AProxySignature w a ByteSpan)
decodeAProxySignature =
  AProxySignature
    <$  enforceSize "ProxySignature" 2
    <*> decodeAProxySecretKey
    <*> decodeXSignature


validateProxySignature
  :: (MonadError Text m)
  => ProtocolMagic
  -> AProxySignature w a ByteString
  -> m ()
validateProxySignature pm psig = validateProxySecretKey pm (psigPsk psig)


-- | Make a proxy delegate signature with help of certificate. If the delegate
--   secret key passed doesn't pair with delegate public key in certificate
--   inside, we panic. Please check this condition outside of this function.
proxySign
  :: Bi a
  => ProtocolMagic
  -> SignTag
  -> SecretKey
  -> ProxySecretKey w
  -> a
  -> ProxySignature w a
proxySign pm t sk@(SecretKey delegateSk) psk m
  | toPublic sk /= pskDelegatePk psk = panic $ sformat
    ( "proxySign called with irrelevant certificate "
    . "(psk delegatePk: "
    . build
    . ", real delegate pk: "
    . build
    . ")"
    )
    (pskDelegatePk psk)
    (toPublic sk)
  | otherwise = AProxySignature {psigPsk = psk, psigSig = sigma}
 where
  PublicKey issuerPk = pskIssuerPk psk
  -- It's safe to put the tag after issuerPk because `CC.unXPub issuerPk` always
  -- takes 64 bytes
  sigma              = CC.sign (mempty :: ScrubbedBytes) delegateSk
    $ mconcat ["01", CC.unXPub issuerPk, signTag pm t, serialize' m]

-- | Verify delegated signature given issuer's pk, signature, message
--   space predicate and message itself.
proxyVerifyDecoded
  :: Decoded t
  => ProtocolMagic
  -> SignTag
  -> ProxySignature w (BaseType t)
  -> (w -> Bool)
  -> t
  -> Bool
proxyVerifyDecoded pm t psig omegaPred m = predCorrect && sigValid
 where
  psk                       = psigPsk psig
  PublicKey issuerPk        = pskIssuerPk psk
  PublicKey pdDelegatePkRaw = pskDelegatePk psk
  predCorrect               = omegaPred (pskOmega psk)
  sigValid                  = CC.verify
    pdDelegatePkRaw
    (mconcat ["01", CC.unXPub issuerPk, signTag pm t, recoverBytes m])
    (psigSig psig)

-- | Verify delegated signature given issuer's pk, signature, message space
--   predicate and message
proxyVerify
  :: Bi a
  => ProtocolMagic
  -> SignTag
  -> ProxySignature w a
  -> (w -> Bool)
  -> a
  -> Bool
proxyVerify pm t psig omegaPred m = predCorrect && sigValid
 where
  psk                       = psigPsk psig
  PublicKey issuerPk        = pskIssuerPk psk
  PublicKey pdDelegatePkRaw = pskDelegatePk psk
  predCorrect               = omegaPred (pskOmega psk)
  sigValid                  = CC.verify
    pdDelegatePkRaw
    (mconcat ["01", CC.unXPub issuerPk, signTag pm t, serialize' m])
    (psigSig psig)
