-- | Provides types and functions for the delegation interface
-- between the ledger layer and the blockchain layer
module Delegation.Interface
  ( initDIState
  )
where

import Control.Lens
import Chain.GenesisBlock (initVKeys)
import Control.State.Transition
import Data.Set (Set)
import Ledger.Delegation (DIState, VKeyGen)
import Types


-- | Computes an initial delegation interface state from a set of
-- verification keys
initDIStateFromKeys :: Set VKeyGen -> DIState
initDIStateFromKeys certs = undefined

-- | The initial delegation interface state
initDIState :: DIState
initDIState = initDIStateFromKeys initVKeys
