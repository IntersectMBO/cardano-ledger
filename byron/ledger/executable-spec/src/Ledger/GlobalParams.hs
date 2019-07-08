-- | Ledger global parameters.

module Ledger.GlobalParams
  ( lovelaceCap
  , slotsPerEpoch
  , slotsPerEpochToK
  )
where

import           Data.Int (Int64)

import           Ledger.Core (BlockCount (BlockCount), Lovelace (Lovelace))


-- | Constant amount of Lovelace in the system.
lovelaceCap :: Lovelace
lovelaceCap = Lovelace $ 45 * fromIntegral ((10 :: Int64) ^ (15 :: Int64))

-- | Given the chain stability parameter, often referred to as @k@, which is
-- expressed in an amount of blocks, return the number of slots contained in an
-- epoch.
slotsPerEpoch :: Integral n => BlockCount -> n
slotsPerEpoch (BlockCount c) = fromIntegral $ c * 10

-- | The inverse of 'slotsPerEpoch': given a number of slots per-epoch, return the chain stability
-- parameter @k@.
slotsPerEpochToK :: (Integral n) => n -> BlockCount
slotsPerEpochToK n = BlockCount $ floor $ (fromIntegral n :: Double) / 10
