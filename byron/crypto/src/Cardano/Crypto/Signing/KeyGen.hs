module Cardano.Crypto.Signing.KeyGen
  ( keyGen,
    deterministicKeyGen,
  )
where

import Cardano.Crypto.Signing.SigningKey (SigningKey (..))
import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..))
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteString as BS

-- TODO: this is just a placeholder for actual (not ready yet) derivation
-- of keypair from seed in cardano-crypto API
createKeypairFromSeed :: BS.ByteString -> (CC.XPub, CC.XPrv)
createKeypairFromSeed seed =
  let prv = CC.generate seed (mempty :: ScrubbedBytes) in (CC.toXPub prv, prv)

-- | Generate a key pair. It's recommended to run it with 'runSecureRandom'
--   from "Cardano.Crypto.Random" because the OpenSSL generator is probably safer
--   than the default IO generator.
keyGen :: MonadRandom m => m (VerificationKey, SigningKey)
keyGen = do
  seed <- getRandomBytes 32
  let (vk, sk) = createKeypairFromSeed seed
  return (VerificationKey vk, SigningKey sk)

-- | Create key pair deterministically from 32 bytes.
deterministicKeyGen :: BS.ByteString -> (VerificationKey, SigningKey)
deterministicKeyGen seed =
  bimap VerificationKey SigningKey (createKeypairFromSeed seed)
