module Cardano.Crypto.Signing.Safe.KeyGen
  ( safeDeterministicKeyGen
  , safeKeyGen
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.ByteString as BS

import Cardano.Crypto.Hashing (hash)
import qualified Cardano.Crypto.Scrypt as S
import Cardano.Crypto.Signing.VerificationKey (VerificationKey(..))
import Cardano.Crypto.Signing.Safe.EncryptedSigningKey
  (EncryptedSigningKey(..), mkEncSecretWithSaltUnsafe)
import Cardano.Crypto.Signing.Safe.PassPhrase (PassPhrase(..))


safeCreateKeypairFromSeed :: BS.ByteString -> PassPhrase -> (CC.XPub, CC.XPrv)
safeCreateKeypairFromSeed seed (PassPhrase pp) =
  let prv = CC.generate seed pp in (CC.toXPub prv, prv)

-- NB. It's recommended to run it with 'runSecureRandom' from
-- "Cardano.Crypto.Random" because the OpenSSL generator is probably safer than
-- the default IO generator.
safeKeyGen :: (MonadRandom m) => PassPhrase -> m (VerificationKey, EncryptedSigningKey)
safeKeyGen pp = do
  seed <- getRandomBytes 32
  pure $ safeDeterministicKeyGen seed pp

safeDeterministicKeyGen
  :: BS.ByteString -> PassPhrase -> (VerificationKey, EncryptedSigningKey)
safeDeterministicKeyGen seed pp = bimap
  VerificationKey
  (mkEncSecretWithSaltUnsafe (S.mkSalt (hash seed)) pp)
  (safeCreateKeypairFromSeed seed pp)
