{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Delegation.Validation.Activation (
  -- * Activation
  State (..),
  activateDelegation,
)
where

import Cardano.Chain.Common (KeyHash)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Delegation.Validation.Scheduling (ScheduledDelegation (..))
import Cardano.Chain.Slotting (SlotNumber (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen, enforceSize)
import Cardano.Prelude hiding (State)
import qualified Data.Map.Strict as M
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Activation
--------------------------------------------------------------------------------

-- | Maps containing, for each delegator, the active delegation and the slot it
--   became active in.
data State = State
  { delegationMap :: !Delegation.Map
  , delegationSlots :: !(Map KeyHash SlotNumber)
  }
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance DecCBOR State where
  decCBOR = do
    enforceSize "State" 2
    State
      <$> decCBOR
      <*> decCBOR

instance EncCBOR State where
  encCBOR s =
    encodeListLen 2
      <> encCBOR (delegationMap s)
      <> encCBOR (delegationSlots s)

-- | Activate a 'ScheduledDelegation' if its activation slot is less than the
--   previous delegation slot for this delegate, otherwise discard it. This is
--   an implementation of the delegation activation rule in the ledger
--   specification.
activateDelegation :: State -> ScheduledDelegation -> State
activateDelegation as delegation
  | (delegate `Delegation.notMemberR` delegationMap)
      && (prevDelegationSlot < slot || unSlotNumber slot == 0) =
      State
        { delegationMap = Delegation.insert delegator delegate delegationMap
        , delegationSlots = M.insert delegator slot delegationSlots
        }
  | otherwise = as
  where
    State {delegationMap, delegationSlots} = as
    ScheduledDelegation slot delegator delegate = delegation

    prevDelegationSlot =
      fromMaybe (SlotNumber 0) $ M.lookup delegator delegationSlots
