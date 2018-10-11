{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for verifying signatures.
--
--   TODO: the "Cardano.Crypto.Signing" hierarchy looks like a mess and should
--   be redesigned. When this is done, we likely won't need this module to be
--   separated from other modules, but right now we do need it in order to avoid
--   circular dependencies. — @neongreen

module Cardano.Crypto.Signing.Check
       ( checkSig
       , checkSigRaw
       , verifyProxyCert
       , validateProxySecretKey
       , validateProxySignature
       ) where

import           Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import           Control.Monad.Except (MonadError, throwError)
import           Data.Coerce (coerce)

import           Cardano.Binary.Class (Bi, Raw)
import qualified Cardano.Binary.Class as Bi
import           Cardano.Crypto.ProtocolMagic (ProtocolMagic)
import           Cardano.Crypto.Signing.Tag (SignTag (..), signTag)
import           Cardano.Crypto.Signing.Types.Signing (ProxyCert (..),
                     ProxySecretKey (..), ProxySignature (..), PublicKey (..),
                     Signature (..))


-- CHECK: @checkSig
-- | Verify a signature
-- #verifyRaw
checkSig
  :: Bi a => ProtocolMagic -> SignTag -> PublicKey -> a -> Signature a -> Bool
checkSig pm t k x s = checkSigRaw pm (Just t) k (Bi.serialize' x) (coerce s)

-- CHECK: @checkSigRaw
-- | Verify raw 'ByteString'
checkSigRaw
  :: ProtocolMagic
  -> Maybe SignTag
  -> PublicKey
  -> ByteString
  -> Signature Raw
  -> Bool
checkSigRaw pm mbTag (PublicKey k) x (Signature s) = CC.verify k (tag <> x) s
  where tag = maybe mempty (signTag pm) mbTag

-- | Checks if certificate is valid, given issuer pk, delegate pk and ω
verifyProxyCert
  :: Bi w
  => ProtocolMagic
  -> PublicKey
  -> PublicKey
  -> w
  -> ProxyCert w
  -> Bool
verifyProxyCert pm issuerPk (PublicKey delegatePk) o (ProxyCert sig) = checkSig
  pm
  SignProxySK
  issuerPk
  (mconcat ["00", CC.unXPub delegatePk, Bi.serialize' o])
  (Signature sig)

-- | Return the key if it's valid, and throw an error otherwise
validateProxySecretKey
  :: (MonadError Text m, Bi w) => ProtocolMagic -> ProxySecretKey w -> m ()
validateProxySecretKey pm psk =
  if verifyProxyCert
      pm
      (pskIssuerPk psk)
      (pskDelegatePk psk)
      (pskOmega psk)
      (pskCert psk)
    then pure ()
    else throwError "a ProxySecretKey has an invalid signature"

validateProxySignature
  :: (MonadError Text m, Bi w) => ProtocolMagic -> ProxySignature w a -> m ()
validateProxySignature pm psig = validateProxySecretKey pm (psigPsk psig)
