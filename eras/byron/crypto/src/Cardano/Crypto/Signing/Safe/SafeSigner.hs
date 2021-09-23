module Cardano.Crypto.Signing.Safe.SafeSigner
  ( SafeSigner (..),
    noPassSafeSigner,
    safeToVerification,
  )
where

import Cardano.Crypto.Signing.Safe.PassPhrase (PassPhrase, emptyPassphrase)
import Cardano.Crypto.Signing.SigningKey (SigningKey (..), toVerification)
import Cardano.Crypto.Signing.VerificationKey (VerificationKey (..))
import Cardano.Prelude

-- | SafeSigner datatype to encapsulate sensitive data
data SafeSigner = SafeSigner !SigningKey !PassPhrase
  deriving (Show)

noPassSafeSigner :: SigningKey -> SafeSigner
noPassSafeSigner sk = SafeSigner sk emptyPassphrase

safeToVerification :: SafeSigner -> VerificationKey
safeToVerification (SafeSigner sk _) = toVerification sk
