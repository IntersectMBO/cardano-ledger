-- | Generators for the 'Ledger.Core' values.
module Ledger.Core.Generator
  ( vkGen
  , vkgenesisGen
  )
where

import Hedgehog (Gen)
import Hedgehog.Gen (integral)
import Hedgehog.Range (linear)

import Ledger.Core
  ( Owner(Owner)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  )

vkGen :: Gen VKey
vkGen = VKey . Owner <$> integral (linear 0 10000)

vkgenesisGen :: Gen VKeyGenesis
vkgenesisGen = VKeyGenesis <$> vkGen
