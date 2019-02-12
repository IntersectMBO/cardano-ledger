{-# LANGUAGE TemplateHaskell #-}

module Ledger.Update where

import Control.Lens (makeLenses)
import Numeric.Natural

import Ledger.Core (SlotCount)

-- | Protocol parameters.
--
data PParams = PParams -- TODO: this should be a module of @cs-ledger@.
  { _maxBkSz  :: !Natural
  -- ^ Maximum (abstract) block size in words.
  , _maxHdrSz :: !Natural
  -- ^ Maximum (abstract) block header size in words.
  , _dLiveness :: !SlotCount
  -- ^ Delegation liveness parameter: number of slots it takes a delegation
  -- certificate to take effect.
  , _bkSgnCntW :: !Int
  -- ^ Size of the moving window to count signatures.
  , _bkSgnCntT :: !Double
  -- ^ Fraction [0, 1] of the blocks that can be signed by any given key in a
  -- window of lenght '_bkSgnCntW'. This value will be typically between 1/5
  -- and 1/4.
  , _bkSlotsPerEpoch :: !SlotCount
  } deriving (Eq, Show)

makeLenses ''PParams
