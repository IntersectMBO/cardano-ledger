{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Chain.Delegation.Validation.Activation
  (
  -- * Activation
    State(..)
  , activateDelegation
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M

import Cardano.Chain.Common (StakeholderId)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Delegation.Validation.Scheduling (ScheduledDelegation(..))
import Cardano.Chain.Slotting (FlatSlotId(..))


--------------------------------------------------------------------------------
-- Activation
--------------------------------------------------------------------------------

-- | Maps containing, for each delegator, the active delegation and the slot it
--   became active in.
data State = State
  { delegationMap   :: !Delegation.Map
  , delegationSlots :: !(Map StakeholderId FlatSlotId)
  } deriving (Eq, Show, Generic, NFData)

-- | Activate a 'ScheduledDelegation' if its activation slot is less than the
--   previous delegation slot for this delegate, otherwise discard it. This is
--   an implementation of the delegation activation rule in the ledger
--   specification.
activateDelegation :: State -> ScheduledDelegation -> State
activateDelegation as delegation
  | prevDelegationSlot < slot || unFlatSlotId slot == 0 = State
    { delegationMap   = Delegation.insert delegator delegate delegationMap
    , delegationSlots = M.insert delegator slot delegationSlots
    }
  | otherwise = as
 where
  State { delegationMap, delegationSlots }    = as
  ScheduledDelegation slot delegator delegate = delegation

  prevDelegationSlot =
    fromMaybe (FlatSlotId 0) $ M.lookup delegator delegationSlots
