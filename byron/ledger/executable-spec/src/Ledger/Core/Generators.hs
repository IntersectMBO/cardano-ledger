-- | Generators for the 'Ledger.Core' values.
module Ledger.Core.Generators
  ( vkGen
  , vkgenesisGen
  , addrGen
  , slotGen
  )
where

import           Data.Word (Word64)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Ledger.Core
  ( Addr(Addr)
  , Owner(Owner)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  , Slot(Slot)
  )

vkGen :: Gen VKey
vkGen = VKey . Owner <$> Gen.integral (Range.linear 0 10000)

vkgenesisGen :: Gen VKeyGenesis
vkgenesisGen = VKeyGenesis <$> vkGen

addrGen :: Gen Addr
addrGen = Addr <$> vkGen

-- | Generates a slot within the given bound
slotGen :: Word64 -> Word64 -> Gen Slot
slotGen lower upper =
  Slot <$> Gen.word64 (Range.linear lower upper)
