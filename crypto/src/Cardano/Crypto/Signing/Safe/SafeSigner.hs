module Cardano.Crypto.Signing.Safe.SafeSigner
  ( SafeSigner(..)
  , noPassSafeSigner
  , safeToVerification
  )
where

import Cardano.Prelude

import Cardano.Crypto.Signing.VerificationKey (VerificationKey(..))
import Cardano.Crypto.Signing.SigningKey (SigningKey(..))
import Cardano.Crypto.Signing.Safe.EncryptedSigningKey
  (EncryptedSigningKey, noPassEncrypt, encToVerification)
import Cardano.Crypto.Signing.Safe.PassPhrase (PassPhrase, emptyPassphrase)


-- | SafeSigner datatype to encapsulate sensitive data
data SafeSigner =
  SafeSigner EncryptedSigningKey PassPhrase
  deriving (Show)

noPassSafeSigner :: SigningKey -> SafeSigner
noPassSafeSigner sk = SafeSigner (noPassEncrypt sk) emptyPassphrase

safeToVerification :: SafeSigner -> VerificationKey
safeToVerification (SafeSigner sk _) = encToVerification sk

