module Cardano.Crypto.Signing.Safe.SafeSigner
  ( SafeSigner(..)
  , noPassSafeSigner
  , safeToVerification
  , withSafeSigners
  , withSafeSigner
  )
where

import Cardano.Prelude

import Cardano.Crypto.Signing.VerificationKey (VerificationKey(..))
import Cardano.Crypto.Signing.SigningKey (SigningKey(..))
import Cardano.Crypto.Signing.Safe.EncryptedSigningKey
  (EncryptedSigningKey, noPassEncrypt, encToVerification, checkPassMatches)
import Cardano.Crypto.Signing.Safe.PassPhrase (PassPhrase, emptyPassphrase)


-- | SafeSigner datatype to encapsulate sensitive data
data SafeSigner =
  SafeSigner EncryptedSigningKey PassPhrase
  deriving (Show)

noPassSafeSigner :: SigningKey -> SafeSigner
noPassSafeSigner sk = SafeSigner (noPassEncrypt sk) emptyPassphrase

safeToVerification :: SafeSigner -> VerificationKey
safeToVerification (SafeSigner sk _) = encToVerification sk

-- | We can make SafeSigner only inside IO bracket, so we can manually cleanup
--   all IO buffers we use to store passphrase (when we'll actually use them)
withSafeSigners
  :: (Monad m, Traversable t)
  => t EncryptedSigningKey
  -> m PassPhrase
  -> (t SafeSigner -> m a)
  -> m a
withSafeSigners sks ppGetter action = do
  pp <- ppGetter
  let mss = map (\sk -> SafeSigner sk pp) sks
  action mss

withSafeSigner
  :: (Monad m)
  => EncryptedSigningKey
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
--   => EncryptedSigningKey
--   -> m PassPhrase
--   -> (SafeSigner -> m a)
--   -> m a
-- withSafeSignerUnsafe sk ppGetter action = do
--   pp <- ppGetter
--   withSafeSigners (Identity sk) (pure pp) $ action . runIdentity
