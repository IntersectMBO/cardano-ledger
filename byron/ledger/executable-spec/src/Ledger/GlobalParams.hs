-- | Ledger global parameters.

module Ledger.GlobalParams
  ( k
  , lovelaceCap
  , ngk
  )
where

import Data.Int (Int64)
import Data.Word (Word64)

import Ledger.Core (BlockCount (BlockCount), Lovelace (Lovelace))


-- | Chain stability parameter, measured in terms of number of blocks.
--
-- We're fixing this for now. In the future we might want to make this
-- configurable.
k :: BlockCount
k = BlockCount 2160

-- | Constant amount of Lovelace in the system.
lovelaceCap :: Lovelace
lovelaceCap = Lovelace $ 45 * fromIntegral ((10 :: Int64) ^ (15 :: Int64))

-- | Number of genesis keys
ngk :: Word64
ngk = 7
