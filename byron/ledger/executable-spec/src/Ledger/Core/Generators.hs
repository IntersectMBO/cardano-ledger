-- | Generators for the 'Ledger.Core' values.
module Ledger.Core.Generators
  ( vkGen
  , vkgenesisGen
  , addrGen
  )
where

import Hedgehog (Gen)
import Hedgehog.Gen (integral)
import Hedgehog.Range (linear)

import Ledger.Core
  ( Addr(Addr)
  , Owner(Owner)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  )

vkGen :: Gen VKey
vkGen = VKey . Owner <$> integral (linear 0 10000)

vkgenesisGen :: Gen VKeyGenesis
vkgenesisGen = VKeyGenesis <$> vkGen

addrGen :: Gen Addr
addrGen = Addr <$> vkGen
