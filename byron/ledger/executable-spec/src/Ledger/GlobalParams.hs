-- | Ledger global parameters.

module Ledger.GlobalParams
  ( lovelaceCap
  , ngk
  )
where

import Data.Int (Int64)
import Data.Word (Word64)

import Ledger.Core (Lovelace (Lovelace))


-- | Constant amount of Lovelace in the system.
lovelaceCap :: Lovelace
lovelaceCap = Lovelace $ 45 * fromIntegral ((10 :: Int64) ^ (15 :: Int64))

-- | Number of genesis keys
ngk :: Word64
ngk = 7
