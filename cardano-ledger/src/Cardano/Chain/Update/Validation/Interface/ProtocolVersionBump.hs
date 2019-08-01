{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump
  ( Environment (..)
  , State (..)
  , tryBumpVersion
  )
where

import Cardano.Prelude hiding (State)

import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.Slotting (EpochNumber, SlotNumber, subSlotCount)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.Validation.Endorsement
  ( CandidateProtocolUpdate(CandidateProtocolUpdate)
  , cpuProtocolParameters
  , cpuProtocolVersion
  , cpuSlot
  )

data Environment = Environment
  { k                         :: !BlockCount
  , currentSlot               :: !SlotNumber
  , candidateProtocolVersions :: ![CandidateProtocolUpdate]
  }

data State = State
  { currentEpoch              :: !EpochNumber
  , nextProtocolVersion       :: !ProtocolVersion
  , nextProtocolParameters    :: !ProtocolParameters
  }

-- | Change the protocol version when an epoch change is detected, and there is
-- a candidate protocol update that was confirmed at least @2 * k@ slots ago,
-- where @k@ is the chain security parameter.
--
-- This corresponds to the @PVBUMP@ rules in the Byron ledger specification.
tryBumpVersion
  :: Environment
  -> State
  -> EpochNumber
  -> State
tryBumpVersion env st lastSeenEpoch =
  case (currentEpoch < lastSeenEpoch, stableCandidates) of
    (True, newestStable:_) ->
      let CandidateProtocolUpdate
            { cpuProtocolVersion
            , cpuProtocolParameters
            } = newestStable
      in
        st { currentEpoch = lastSeenEpoch
           , nextProtocolVersion = cpuProtocolVersion
           , nextProtocolParameters = cpuProtocolParameters
           }
    _ -> st

  where
    Environment { k, currentSlot, candidateProtocolVersions } = env

    State { currentEpoch } = st

    stableCandidates =
      filter ((<= subSlotCount (kSlotSecurityParam k) currentSlot) . cpuSlot) candidateProtocolVersions
