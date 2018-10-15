module Test.Cardano.Chain.Common.Example
       ( exampleAttributes
       , exampleChainDifficulty
       , exampleStakeholderId
       ) where

import           Cardano.Chain.Common (Attributes, BlockCount (..),
                     ChainDifficulty (..), StakeholderId, mkAttributes,
                     mkStakeholderId)

import           Test.Cardano.Crypto.Example (examplePublicKey)


exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleStakeholderId :: StakeholderId
exampleStakeholderId = mkStakeholderId examplePublicKey
