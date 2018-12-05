{-# LANGUAGE TemplateHaskell #-}
module Ledger.Delegation where

import Control.Lens
import Ledger.Signatures
import Ledger.Core
import Numeric.Natural (Natural)
import Data.Set (Set)
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------
-- Abstract types
--------------------------------------------------------------------------------

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGen = VKeyGen VKey


data DCert = DCert
  { -- | Body of the delegation certificate
    _dbody :: (VKey, Epoch)
    -- | Witness for the delegation cerfiticate
  , _dwit :: Sig VKeyGen
    -- | Who delegates to whom
  , _dwho :: (VKeyGen, VKey)
    -- | Certificate epoch
  , _depoch :: Epoch
  }

makeLenses ''DCert

--------------------------------------------------------------------------------
-- Derived types
--------------------------------------------------------------------------------

-- | Delegation scheduling environment
data DSEnv = DSEnv
  { _dseAllowedDelegators :: Set VKeyGen
  , _dseEpoch :: Epoch
  , _dseSlot :: Slot
  , _dseLiveness :: SlotCount
  }

makeLenses ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dssScheduledDelegations :: [(Slot, VKeyGen, VKey)]
  , _dssKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeLenses ''DSState

-- | Delegation state
data DState = DState
  { _dsDelegationMap :: Map VKeyGen VKey
    -- | When was the last time each genesis key delegated.
  , _dsLastDelegation :: Map VKeyGen Slot
  }

makeLenses ''DState

--------------------------------------------------------------------------------
-- Transition systems
--------------------------------------------------------------------------------
