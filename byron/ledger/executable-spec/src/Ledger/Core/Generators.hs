-- | Generators for the 'Ledger.Core' values.
module Ledger.Core.Generators
  ( vk
  , vkgenesis
  , addr
  , slot
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

vk :: Gen VKey
vk = VKey . Owner <$> Gen.integral (Range.linear 0 10000)

vkgenesis :: Gen VKeyGenesis
vkgenesis = VKeyGenesis <$> vk

addr :: Gen Addr
addr = Addr <$> vk

-- | Generates a slot within the given bound
slot :: Word64 -> Word64 -> Gen Slot
slot lower upper =
  Slot <$> Gen.word64 (Range.linear lower upper)
