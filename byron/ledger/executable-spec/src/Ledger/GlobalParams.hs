-- | Ledger global parameters.

module Ledger.GlobalParams
  (k)
where

import Ledger.Core (BlockCount (BlockCount))

-- | Chain stability parameter, measured in terms of number of blocks.
--
-- We're fixing this for now. In the future we might want to make this
-- configurable.
k :: BlockCount
k = BlockCount 2160
