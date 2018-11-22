module Cardano.Crypto.Signing.Safe.SafeSigner
  ( SafeSigner(..)
  , noPassSafeSigner
  , safeToPublic
  , withSafeSigners
  , withSafeSigner
  )
where

import Cardano.Prelude

import Cardano.Crypto.Signing.PublicKey (PublicKey(..))
import Cardano.Crypto.Signing.SecretKey (SecretKey(..))
import Cardano.Crypto.Signing.Safe.EncryptedSecretKey
  (EncryptedSecretKey, noPassEncrypt, encToPublic, checkPassMatches)
import Cardano.Crypto.Signing.Safe.PassPhrase (PassPhrase, emptyPassphrase)


-- | SafeSigner datatype to encapsulate sensitive data
data SafeSigner =
  SafeSigner EncryptedSecretKey PassPhrase
  deriving (Show)

noPassSafeSigner :: SecretKey -> SafeSigner
noPassSafeSigner sk = SafeSigner (noPassEncrypt sk) emptyPassphrase

safeToPublic :: SafeSigner -> PublicKey
safeToPublic (SafeSigner sk _) = encToPublic sk

-- | We can make SafeSigner only inside IO bracket, so we can manually cleanup
--   all IO buffers we use to store passphrase (when we'll actually use them)
withSafeSigners
  :: (Monad m, Traversable t)
  => t EncryptedSecretKey
  -> m PassPhrase
  -> (t SafeSigner -> m a)
  -> m a
withSafeSigners sks ppGetter action = do
  pp <- ppGetter
  let mss = map (\sk -> SafeSigner sk pp) sks
  action mss

withSafeSigner
  :: (Monad m)
  => EncryptedSecretKey
  -> m PassPhrase
  -> (Maybe SafeSigner -> m a)
  -> m a
withSafeSigner sk ppGetter action = do
  pp <- ppGetter
  withSafeSigners (Identity sk) (pure pp)
    $ action
    . (checkPassMatches pp sk $>)
    . runIdentity

-- | This function is like @withSafeSigner@ but doesn't check @checkPassMatches@
-- withSafeSignerUnsafe
--   :: (Monad m)
--   => EncryptedSecretKey
--   -> m PassPhrase
--   -> (SafeSigner -> m a)
--   -> m a
-- withSafeSignerUnsafe sk ppGetter action = do
--   pp <- ppGetter
--   withSafeSigners (Identity sk) (pure pp) $ action . runIdentity
