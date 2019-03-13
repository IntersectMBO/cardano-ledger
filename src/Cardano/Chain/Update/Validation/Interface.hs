
-- | Blockchain interface validation rules.
--
module Cardano.Chain.Update.Validation.Interface
  (State (..))
where

import Cardano.Prelude (Map, Set)

import Cardano.Chain.Slotting (EpochIndex, FlatSlotId)
import Cardano.Chain.Common.StakeholderId (StakeholderId)

import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion
  , SoftwareVersion
  )
import Cardano.Chain.Update.Vote (UpId)

-- | Update interface state.
data TODO
data State
  = State
    { prevEpoch :: !EpochIndex
      -- ^ Previously seen epoch
    , adoptedProtocolParams :: !ProtocolParameters
      -- ^ Adopted protocol parameters
    , futureAdopts :: !TODO -- We should take this from Cardano.Chain.Update.Validation.Endorsement
      -- ^ Future protocol version adoptions
    , appVersions :: !(Map ApplicationName (NumSoftwareVersion, FlatSlotId))
      -- ^ Current application versions (by application name)
    , registeredProtocolUpdateProposals :: !(Map UpId (ProtocolVersion, ProtocolParameters))
      -- ^ Registered protocol update proposals
    , registeredSoftwareUpdateProposals :: !(Map UpId SoftwareVersion)
      -- ^ Registered software update proposals
    , confirmedProposals :: !(Map UpId FlatSlotId)
      -- ^ Confirmed update proposals
    , proposalVotes :: !(Map UpId (Set StakeholderId))
      -- ^ Update proposals votes
    , proposalsEndorsements :: TODO -- We should take this from Cardano.Chain.Update.Validation.Endorsement
                                    -- So this should be a @Set Endorsement@
      -- ^ Update proposals endorsements
    , proposalRegistrationSlot :: Map UpId FlatSlotId
      -- ^ Slot at which an update proposal was registered
    }
