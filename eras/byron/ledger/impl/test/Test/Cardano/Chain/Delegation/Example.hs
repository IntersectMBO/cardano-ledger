module Test.Cardano.Chain.Delegation.Example
  ( exampleCertificates,
  )
where

import Cardano.Chain.Delegation (Certificate, signCertificate)
import Cardano.Chain.Slotting (EpochNumber (..))
import Cardano.Crypto (ProtocolMagicId (..))
import Cardano.Prelude
import Data.List (zipWith4)
import Test.Cardano.Crypto.Example (exampleVerificationKeys, staticSafeSigners)

staticProtocolMagics :: [ProtocolMagicId]
staticProtocolMagics = ProtocolMagicId <$> [0 .. 5]

exampleCertificates :: [Certificate]
exampleCertificates =
  zipWith4
    signCertificate
    staticProtocolMagics
    (exampleVerificationKeys 1 6)
    exampleEpochIndices
    staticSafeSigners
  where
    exampleEpochIndices = EpochNumber <$> [5, 1, 3, 27, 99, 247]
