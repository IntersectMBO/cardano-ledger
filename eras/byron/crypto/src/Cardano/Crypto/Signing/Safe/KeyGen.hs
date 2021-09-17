module Cardano.Crypto.Signing.Safe.KeyGen
  ( safeDeterministicKeyGen,
    safeKeyGen,
  )
where

import Cardano.Crypto.Signing.Safe.PassPhrase (PassPhrase (..))
import Cardano.Crypto.Signing.SigningKey (SigningKey (..))
import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..))
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.ByteString as BS

safeCreateKeypairFromSeed :: BS.ByteString -> PassPhrase -> (CC.XPub, CC.XPrv)
safeCreateKeypairFromSeed seed (PassPhrase pp) =
  let prv = CC.generate seed pp in (CC.toXPub prv, prv)

-- NB. It's recommended to run it with 'runSecureRandom' from
-- "Cardano.Crypto.Random" because the OpenSSL generator is probably safer than
-- the default IO generator.
safeKeyGen :: (MonadRandom m) => PassPhrase -> m (VerificationKey, SigningKey)
safeKeyGen pp = do
  seed <- getRandomBytes 32
  pure $ safeDeterministicKeyGen seed pp

safeDeterministicKeyGen ::
  BS.ByteString -> PassPhrase -> (VerificationKey, SigningKey)
safeDeterministicKeyGen seed pp =
  bimap VerificationKey SigningKey (safeCreateKeypairFromSeed seed pp)
