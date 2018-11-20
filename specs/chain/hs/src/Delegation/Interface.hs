-- | Provides types and functions for the delegation interface
-- between the ledger layer and the blockchain layer
module Delegation.Interface
  ( DSIState
  , delegates
  , initDSIState
  , newCertsRule
  , updateCerts
  )
where

import Chain.GenesisBlock (initVKeys)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Types


-- TODO: to be implemented
-- | A delegation interface state, as described in the ledger layer
-- specification written by Damian Nadales.
data DSIState

-- | For a given delegation state, it returns a mapping from a delegatee key
-- to a delegator key. If a key is not present in the key set of the returned
-- map, it has no right to sign a block in the current slot
delegates :: DSIState -> Map.Map VKey VKey
delegates = undefined

-- | Computes an initial delegation interface state from a set of
-- verification keys
initDSIStateFromKeys :: Set VKey -> DSIState
initDSIStateFromKeys certs = undefined

-- | The initial delegation interface state
initDSIState :: DSIState
initDSIState = initDSIStateFromKeys initVKeys

-- | Defines when new certificates can be added to the ledger's state
newCertsRule :: Rule Interf
newCertsRule = undefined

-- | Updates the delegation interface state with a set of heavyweight
-- delegation certificates that arrived in a block issued in the given
-- slot
updateCerts :: Slot -> Set HCert -> DSIState -> DSIState
updateCerts = undefined
