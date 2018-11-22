module Cardano.Crypto.Signing.Redeem.KeyGen
  ( redeemKeyGen
  , redeemDeterministicKeyGen
  )
where

import Cardano.Prelude

import Control.Monad (fail)
import Crypto.Error (maybeCryptoError)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random (MonadRandom)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

import Cardano.Crypto.Signing.Redeem.PublicKey (RedeemPublicKey(..))
import Cardano.Crypto.Signing.Redeem.SecretKey (RedeemSecretKey(..))


-- | Generate a key pair. It's recommended to run it with 'runSecureRandom' from
--   "Cardano.Crypto.Random" because the OpenSSL generator is probably safer
--   than the default IO generator.
redeemKeyGen :: MonadRandom m => m (RedeemPublicKey, RedeemSecretKey)
redeemKeyGen = do
  sk <- Ed25519.generateSecretKey
  return (RedeemPublicKey $ Ed25519.toPublic sk, RedeemSecretKey sk)

-- | Create key pair deterministically from 32 bytes
redeemDeterministicKeyGen
  :: BS.ByteString -> Maybe (RedeemPublicKey, RedeemSecretKey)
redeemDeterministicKeyGen seed =
  case maybeCryptoError $ Ed25519.secretKey (BA.convert seed :: BA.Bytes) of
    Just r -> Just (RedeemPublicKey $ Ed25519.toPublic r, RedeemSecretKey r)
    Nothing ->
      fail "Cardano.Crypto.Signing.Redeem.hs redeemDeterministicKeyGen failed"
