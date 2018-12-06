{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  deriving (Eq, Ord, Show)


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
  { _dSEnvAllowedDelegators :: Set VKeyGen
  , _dSEnvEpoch :: Epoch
  , _dSEnvSlot :: Slot
  , _dSEnvLiveness :: SlotCount
  }

makeFields ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dSStateScheduledDelegations :: [(Slot, (VKeyGen, VKey))]
  , _dSStateKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeFields ''DSState

-- | Delegation state
data DState = DState
  { _dSStateDelegationMap :: Map VKeyGen VKey
    -- | When was the last time each genesis key delegated.
  , _dSStateLastDelegation :: Map VKeyGen Slot
  }

makeFields ''DState

data DIEnv = DIEnv
  { _dIEnvAllowedDelegators :: Set VKeyGen
  , _dIEnvEpoch :: Epoch
  , _dIEnvSlot :: Slot
  , _dIEnvLiveness :: SlotCount

  }

makeFields ''DIEnv

data DIState = DIState
  { _dIStateDelegationMap :: Map VKeyGen VKey
  , _dIStateLastDelegation :: Map VKeyGen Slot
  , _dIStateScheduledDelegations :: [(Slot, (VKeyGen, VKey))]
  , _dIStateKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeFields ''DIState

--------------------------------------------------------------------------------
-- Transition systems
--------------------------------------------------------------------------------
