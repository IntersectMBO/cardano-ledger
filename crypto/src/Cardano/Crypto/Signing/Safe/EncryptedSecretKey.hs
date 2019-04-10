{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Crypto.Signing.Safe.EncryptedSecretKey
  ( EncryptedSecretKey(..)
  , mkEncSecretUnsafe
  , mkEncSecretWithSaltUnsafe
  , encToSecret
  , encToPublic
  , noPassEncrypt
  , checkPassMatches
  , changeEncPassphrase
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Crypto.Random (MonadRandom)
import Data.Default (Default(..))
import Formatting.Buildable (Buildable(..))
import qualified Prelude

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import qualified Cardano.Crypto.Scrypt as S
import Cardano.Crypto.Signing.PublicKey (PublicKey(..))
import Cardano.Crypto.Signing.Safe.PassPhrase (PassPhrase, emptyPassphrase)
import Cardano.Crypto.Signing.SecretKey
  (SecretKey(..), fromCBORXPrv, toCBORXPrv, toPublic)


-- | Encrypted HD secret key
data EncryptedSecretKey = EncryptedSecretKey
  { eskPayload :: !CC.XPrv
  -- ^ Secret key itself, encrypted with passphrase.
  , eskHash    :: !S.EncryptedPass
  -- ^ Hash of passphrase used for key creation.
  }

-- We don't have the @Eq CC.XPrv@ instance here because
-- it is a security issue to compare secret keys. But it
-- can be derived in tests where it doesn't matter.
deriving instance Eq CC.XPrv => Eq EncryptedSecretKey

instance Show EncryptedSecretKey where
  show _ = "<encrypted key>"

instance Buildable EncryptedSecretKey where
  build _ = "<encrypted key>"

instance ToCBOR EncryptedSecretKey where
  toCBOR (EncryptedSecretKey sk pph) =
    encodeListLen 2 <> toCBORXPrv sk <> toCBOR pph

instance FromCBOR EncryptedSecretKey where
  fromCBOR =
    EncryptedSecretKey
      <$  enforceSize "EncryptedSecretKey" 2
      <*> fromCBORXPrv
      <*> fromCBOR

-- | Parameters used to evaluate hash of passphrase.
passScryptParam :: S.ScryptParams
passScryptParam = fromMaybe (panic "Bad passphrase scrypt parameters")
  $ S.mkScryptParams def { S.spHashLen = 32 }  -- maximal passphrase length

-- | Wrap raw secret key, attaching hash to it
--
--   Hash is evaluated using given salt.
--   This function assumes that passphrase matches with secret key.
mkEncSecretWithSaltUnsafe
  :: S.Salt -> PassPhrase -> CC.XPrv -> EncryptedSecretKey
mkEncSecretWithSaltUnsafe salt pp payload =
  EncryptedSecretKey payload $ S.encryptPassWithSalt passScryptParam salt pp

-- | Wrap raw secret key, attachind hash to it.
--   Hash is evaluated using generated salt.
--   This function assumes that passphrase matches with secret key.
mkEncSecretUnsafe
  :: (MonadRandom m) => PassPhrase -> CC.XPrv -> m EncryptedSecretKey
mkEncSecretUnsafe pp payload =
  EncryptedSecretKey payload <$> S.encryptPass passScryptParam pp

-- | Generate a secret key from encrypted secret key.
encToSecret :: EncryptedSecretKey -> SecretKey
encToSecret (EncryptedSecretKey sk _) = SecretKey sk

-- | Generate a public key using an encrypted secret key and passphrase
encToPublic :: EncryptedSecretKey -> PublicKey
encToPublic = toPublic . encToSecret

-- | Re-wrap unencrypted secret key as an encrypted one.
--   NB: for testing purposes only
noPassEncrypt :: SecretKey -> EncryptedSecretKey
noPassEncrypt (SecretKey k) =
  mkEncSecretWithSaltUnsafe S.emptySalt emptyPassphrase k

-- Here with types to avoid module import cycles:
checkPassMatches :: (Alternative f) => PassPhrase -> EncryptedSecretKey -> f ()
checkPassMatches pp (EncryptedSecretKey _ pph) =
  guard (S.verifyPass passScryptParam pp pph)

-- | Regenerates secret key with new passphrase.
--   Note: This operation keeps corresponding public key and derived (child)
--   keys unchanged.
changeEncPassphrase
  :: MonadRandom m
  => PassPhrase
  -> PassPhrase
  -> EncryptedSecretKey
  -> m (Maybe EncryptedSecretKey)
changeEncPassphrase oldPass newPass esk@(EncryptedSecretKey sk _)
  | isJust $ checkPassMatches oldPass esk = Just
  <$> mkEncSecretUnsafe newPass (CC.xPrvChangePass oldPass newPass sk)
  | otherwise = return Nothing
