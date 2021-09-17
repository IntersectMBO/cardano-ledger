module Cardano.Crypto.Signing.Redeem.KeyGen
  ( redeemKeyGen,
    redeemDeterministicKeyGen,
  )
where

import Cardano.Crypto.Signing.Redeem.SigningKey (RedeemSigningKey (..))
import Cardano.Crypto.Signing.Redeem.VerificationKey (RedeemVerificationKey (..))
import Cardano.Prelude
import Control.Monad (fail)
import Crypto.Error (maybeCryptoError)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random (MonadRandom)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

-- | Generate a key pair. It's recommended to run it with 'runSecureRandom' from
--   "Cardano.Crypto.Random" because the OpenSSL generator is probably safer
--   than the default IO generator.
redeemKeyGen :: MonadRandom m => m (RedeemVerificationKey, RedeemSigningKey)
redeemKeyGen = do
  sk <- Ed25519.generateSecretKey
  return (RedeemVerificationKey $ Ed25519.toPublic sk, RedeemSigningKey sk)

-- | Create key pair deterministically from 32 bytes
redeemDeterministicKeyGen ::
  BS.ByteString -> Maybe (RedeemVerificationKey, RedeemSigningKey)
redeemDeterministicKeyGen seed =
  case maybeCryptoError $ Ed25519.secretKey (BA.convert seed :: BA.Bytes) of
    Just r -> Just (RedeemVerificationKey $ Ed25519.toPublic r, RedeemSigningKey r)
    Nothing ->
      fail "Cardano.Crypto.Signing.Redeem.hs redeemDeterministicKeyGen failed"
