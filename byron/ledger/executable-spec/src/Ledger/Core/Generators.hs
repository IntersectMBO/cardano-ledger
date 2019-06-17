-- | Generators for the 'Ledger.Core' values.
module Ledger.Core.Generators
  ( vkGen
  , vkgenesisGen
  , addrGen
  , slotGen
  , epochGen
  , blockCountGen
  )
where

import           Data.Word (Word64)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Ledger.Core
  ( Addr(Addr)
  , BlockCount(BlockCount)
  , Epoch(Epoch)
  , Owner(Owner)
  , Slot(Slot)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
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

-- | Generates an epoch within the given bound
epochGen :: Word64 -> Word64 -> Gen Epoch
epochGen lower upper =
  Epoch <$> Gen.word64 (Range.linear lower upper)

-- | Generates a block count within the given bound
blockCountGen :: Word64 -> Word64 -> Gen BlockCount
blockCountGen lower upper =
  BlockCount <$> Gen.word64 (Range.linear lower upper)
