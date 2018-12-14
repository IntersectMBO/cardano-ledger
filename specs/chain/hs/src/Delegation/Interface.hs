-- | Provides types and functions for the delegation interface
-- between the ledger layer and the blockchain layer
module Delegation.Interface
  ( delegates
  , maybeMapKeyForValue
  , mapKeyForValue
  , initDIState
  )
where

import Control.Lens
import Chain.GenesisBlock (initVKeys)
import Control.State.Transition
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Ledger.Core (VKey, Slot)
import Ledger.Delegation (DCert, DIState, VKeyGen, DELEG, DSEnv, delegationMap)
import Types


-- | For a given delegation state, it returns a mapping from a delegator key
-- to a delegatee key. If a key is not present in the value set of the returned
-- map, it has no right to sign a block in the current slot
delegates :: DIState -> Map.Map VKeyGen VKey
delegates s = s ^. delegationMap

-- | Returns a key from a map for a given value.
maybeMapKeyForValue :: (Eq a, Ord k) => a -> Map.Map k a -> Maybe k
maybeMapKeyForValue v = listToMaybe . map fst . Map.toList . Map.filter (== v)

-- | Unsafely returns a key from a map for a given value. It assumes there is
-- exactly one key mapping to the given value. If there is no such key, it will
-- result in a runtime exception.
mapKeyForValue :: (Eq a, Ord k) => a -> Map.Map k a -> k
mapKeyForValue v = fromJust . maybeMapKeyForValue v

-- | Computes an initial delegation interface state from a set of
-- verification keys
initDIStateFromKeys :: Set VKeyGen -> DIState
initDIStateFromKeys certs = undefined

-- | The initial delegation interface state
initDIState :: DIState
initDIState = initDIStateFromKeys initVKeys
