module Test.Cardano.Chain.Delegation.Example
  ( exampleCertificates
  , exampleUndo
  )
where

import Cardano.Prelude

import Data.List (zipWith4)
import qualified Data.Set as Set

import Cardano.Chain.Delegation (Certificate, Undo(..))
import Cardano.Chain.Slotting (EpochIndex(..))
import Cardano.Crypto (ProtocolMagic(..), createPsk)

import Test.Cardano.Chain.Common.Example (exampleStakeholderId)
import Test.Cardano.Crypto.Example (examplePublicKeys, staticSafeSigners)


staticProtocolMagics :: [ProtocolMagic]
staticProtocolMagics = map ProtocolMagic [0 .. 5]

exampleCertificates :: [Certificate]
exampleCertificates = zipWith4
  createPsk
  staticProtocolMagics
  staticSafeSigners
  (examplePublicKeys 1 6)
  exampleEpochIndices
  where exampleEpochIndices = EpochIndex <$> [5, 1, 3, 27, 99, 247]

exampleUndo :: Undo
exampleUndo = Undo
  { duPsks            = exampleCertificates
  , duPrevEpochPosted = Set.singleton exampleStakeholderId
  }
