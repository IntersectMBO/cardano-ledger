module Test.Cardano.Chain.Delegation.Example
  ( exampleCertificates
  )
where

import Cardano.Prelude

import Data.List (zipWith4)

import Cardano.Chain.Delegation (Certificate)
import Cardano.Chain.Slotting (EpochIndex(..))
import Cardano.Crypto (ProtocolMagicId(..), createPsk)

import Test.Cardano.Crypto.Example (exampleVerificationKeys, staticSafeSigners)


staticProtocolMagics :: [ProtocolMagicId]
staticProtocolMagics = ProtocolMagicId <$> [0 .. 5]

exampleCertificates :: [Certificate]
exampleCertificates = zipWith4
  createPsk
  staticProtocolMagics
  staticSafeSigners
  (exampleVerificationKeys 1 6)
  exampleEpochIndices
  where exampleEpochIndices = EpochIndex <$> [5, 1, 3, 27, 99, 247]
