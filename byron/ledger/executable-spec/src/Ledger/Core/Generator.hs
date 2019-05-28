-- | Generators for the 'Ledger.Core' values.
module Ledger.Core.Generator
  ( slotGen
  , vkGen
  , vkgenesisGen
  )
where

import           Data.Word (Word64)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Gen (integral)
import           Hedgehog.Range (linear)

import Ledger.Core
  ( Owner(Owner)
  , Slot(Slot)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  )

vkGen :: Gen VKey
vkGen = VKey . Owner <$> integral (linear 0 10000)

vkgenesisGen :: Gen VKeyGenesis
vkgenesisGen = VKeyGenesis <$> vkGen

-- | Generates a slot within the given bound
slotGen :: Word64 -> Word64 -> Gen Slot
slotGen lower upper = do
  Slot <$> Gen.word64 (linear lower upper)
