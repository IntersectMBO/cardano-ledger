-- | Ledger global parameters.

module Ledger.GlobalParams
  ( lovelaceCap
  , ngk
  , slotsPerEpoch
  )
where

import Data.Int (Int64)
import Data.Word (Word64)

import Ledger.Core (BlockCount(BlockCount), Lovelace(Lovelace))


-- | Constant amount of Lovelace in the system.
lovelaceCap :: Lovelace
lovelaceCap = Lovelace $ 45 * fromIntegral ((10 :: Int64) ^ (15 :: Int64))

-- | Number of genesis keys
ngk :: Word64
ngk = 7

-- | Given the chain stability parameter, often referred to as @k@, which is
-- expressed in an amount of blocks, return the number of slots contained in an
-- epoch.
slotsPerEpoch :: Integral n => BlockCount -> n
slotsPerEpoch (BlockCount c) = fromIntegral $ c * 10
