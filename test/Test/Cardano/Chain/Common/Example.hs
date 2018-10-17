module Test.Cardano.Chain.Common.Example
       ( exampleAttributes
       , exampleChainDifficulty
       , exampleSlotLeaders
       , exampleStakeholderId
       ) where

import           Cardano.Prelude

import           Data.List.NonEmpty (fromList)

import           Cardano.Chain.Common (Attributes, BlockCount (..),
                     ChainDifficulty (..), SlotLeaders, StakeholderId,
                     mkAttributes, mkStakeholderId)

import           Test.Cardano.Crypto.Example (examplePublicKey,
                     examplePublicKeys)

exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleSlotLeaders :: SlotLeaders
exampleSlotLeaders = mkStakeholderId <$> fromList (examplePublicKeys 16 3)

exampleStakeholderId :: StakeholderId
exampleStakeholderId = mkStakeholderId examplePublicKey
