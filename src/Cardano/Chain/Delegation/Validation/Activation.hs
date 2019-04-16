{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}

module Cardano.Chain.Delegation.Validation.Activation
  (
  -- * Activation
    State(..)
  , activateDelegation

  -- * Utility
  , delegatorOf
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M

import Cardano.Chain.Common (StakeholderId)
import Cardano.Chain.Delegation.Validation.Scheduling (ScheduledDelegation(..))
import Cardano.Chain.Slotting (FlatSlotId(..))


--------------------------------------------------------------------------------
-- Activation
--------------------------------------------------------------------------------

-- | Maps containing, for each delegator, the active delegation and the slot it
--   became active in.
data State = State
  { asDelegationMap   :: !(Map StakeholderId StakeholderId)
  , asDelegationSlots :: !(Map StakeholderId FlatSlotId)
  } deriving (Eq, Show, Generic, NFData)


-- TODO: Move this to `DelegationMap` module
-- | Find the delegator of the given stakeholder-id.
--
-- The function returns nothing if no delegator is found. This function does
-- not check injectivity of the delegation map.
delegatorOf
  :: StakeholderId -> Map StakeholderId StakeholderId -> Maybe StakeholderId
delegatorOf vk dms = case M.keys $ M.filter (== vk) dms of
  vkS : _ -> Just vkS
  _       -> Nothing

-- | Activate a 'ScheduledDelegation' if its activation slot is less than the
--   previous delegation slot for this delegate, otherwise discard it. This is
--   an implementation of the delegation activation rule in the ledger
--   specification.
activateDelegation :: State -> ScheduledDelegation -> State
activateDelegation as delegation
  | prevDelegationSlot < slot || unFlatSlotId slot == 0 = State
    { asDelegationMap   = M.insert delegator delegate delegationMap
    , asDelegationSlots = M.insert delegator slot delegationSlots
    }
  | otherwise = as
 where
  State delegationMap delegationSlots = as
  ScheduledDelegation slot delegator delegate   = delegation

  prevDelegationSlot =
    fromMaybe (FlatSlotId 0) $ M.lookup delegator (asDelegationSlots as)
