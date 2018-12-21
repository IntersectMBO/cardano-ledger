-- | Provides types and functions for the delegation interface
-- between the ledger layer and the blockchain layer
module Delegation.Interface
  ( initDIState
  )
where

import Control.Lens
import Chain.GenesisBlock (initVKeys)
import Control.State.Transition
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromSet)
import Data.Set (Set)
import qualified Data.Set as Set (empty)
import Ledger.Core (VKeyGen(VKeyGen), VKey)
import Ledger.Delegation
  ( DIState(DIState)
  , _dIStateDelegationMap
  , _dIStateLastDelegation
  , _dIStateScheduledDelegations
  , _dIStateKeyEpochDelegations
  )
import Types


-- | Computes an initial delegation interface state from a set of
-- verification keys
initDIStateFromKeys :: Set VKeyGen -> DIState
initDIStateFromKeys keys = DIState
  { _dIStateDelegationMap        = Map.fromSet (\(VKeyGen k) -> k) keys
  , _dIStateLastDelegation       = Map.empty
  , _dIStateScheduledDelegations = []
  , _dIStateKeyEpochDelegations  = Set.empty
  }

-- | The initial delegation interface state
initDIState :: DIState
initDIState = initDIStateFromKeys initVKeys
