module Test.Cardano.Chain.Delegation.Example
  ( exampleLightDlgIndices
  , exampleProxySKBlockInfo
  , exampleUndo
  , staticHeavyDlgIndexes
  , staticProxySKHeavys
  )
where

import Cardano.Prelude

import Data.List (zipWith4, (!!))
import qualified Data.Set as Set

import Cardano.Chain.Delegation
  ( HeavyDlgIndex(..)
  , LightDlgIndices(..)
  , ProxySKBlockInfo
  , ProxySKHeavy
  , Undo(..)
  )
import Cardano.Chain.Slotting (EpochIndex(..))
import Cardano.Crypto (ProtocolMagic(..), safeCreatePsk)

import Test.Cardano.Chain.Common.Example (exampleStakeholderId)
import Test.Cardano.Crypto.Example
  (examplePublicKey, examplePublicKeys, staticSafeSigners)

staticHeavyDlgIndexes :: [HeavyDlgIndex]
staticHeavyDlgIndexes = map (HeavyDlgIndex . EpochIndex) [5, 1, 3, 27, 99, 247]

staticProtocolMagics :: [ProtocolMagic]
staticProtocolMagics = map ProtocolMagic [0 .. 5]

staticProxySKHeavys :: [ProxySKHeavy]
staticProxySKHeavys = zipWith4
  safeCreatePsk
  staticProtocolMagics
  staticSafeSigners
  (examplePublicKeys 1 6)
  staticHeavyDlgIndexes

exampleProxySKBlockInfo :: ProxySKBlockInfo
exampleProxySKBlockInfo = Just (staticProxySKHeavys !! 0, examplePublicKey)

exampleLightDlgIndices :: LightDlgIndices
exampleLightDlgIndices = LightDlgIndices (EpochIndex 7, EpochIndex 88)

exampleUndo :: Undo
exampleUndo = Undo
  { duPsks            = staticProxySKHeavys
  , duPrevEpochPosted = Set.singleton exampleStakeholderId
  }
