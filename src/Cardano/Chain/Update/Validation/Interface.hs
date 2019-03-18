{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Blockchain interface validation rules.
--
module Cardano.Chain.Update.Validation.Interface
  ( -- * Environment
    Environment (..)
    -- * State
  , State (..)
    -- *Error
  , Error (..)
    -- * Interface functions
  , registerProposal
  , registerVote
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M

import Cardano.Chain.Slotting (EpochIndex, FlatSlotId)
import Cardano.Chain.Common.StakeholderId (StakeholderId)

import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion
  , SoftwareVersion
  , svAppName
  , svNumber
  )
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import qualified Cardano.Chain.Update.Validation.Voting as Voting
import Cardano.Chain.Update.Vote (UpId, AProposal, recoverUpId, AVote)
import Cardano.Crypto (ProtocolMagicId)

data TODO

data Environment = Environment
  { protocolMagic :: ProtocolMagicId
  , currentEpoch  :: !EpochIndex
  , currentSlot   :: !FlatSlotId
  , delegationMap :: !(Map StakeholderId StakeholderId)
  }

-- | Update interface state.
data State = State
  { prevEpoch                         :: !EpochIndex
    -- ^ Previously seen epoch
  , adoptedProtocolVersion            :: !ProtocolVersion
  , adoptedProtocolParams             :: !ProtocolParameters
    -- ^ Adopted protocol parameters
  , futureAdopts                      :: !TODO -- We should take this from Cardano.Chain.Update.Validation.Endorsement
    -- ^ Future protocol version adoptions
  , appVersions                       :: !(Map ApplicationName (NumSoftwareVersion, FlatSlotId))
    -- ^ Current application versions (by application name)
  , registeredProtocolUpdateProposals :: !(Map UpId (ProtocolVersion, ProtocolParameters))
    -- ^ Registered protocol update proposals
  , registeredSoftwareUpdateProposals :: !(Map UpId SoftwareVersion)
    -- ^ Registered software update proposals
  , confirmedProposals                :: !(Map UpId FlatSlotId)
    -- ^ Confirmed update proposals
  , proposalVotes                     :: !(Map UpId (Set StakeholderId))
    -- ^ Update proposals votes
  , proposalsEndorsements             :: TODO -- We should take this from Cardano.Chain.Update.Validation.Endorsement
                                              -- So this should be a @Set Endorsement@
    -- ^ Update proposals endorsements
  , proposalRegistrationSlot          :: Map UpId FlatSlotId
    -- ^ Slot at which an update proposal was registered
  }

data Error
  = Registration Registration.Error
  | Voting Voting.Error

-- | Register an update proposal.
--
-- This corresponds to the @UPIREG@ rules in the spec.
registerProposal
  :: MonadError Error m
  => Environment
  -> State
  -> AProposal ByteString
  -> m State
registerProposal env st proposal = do
  Registration.State rpus' raus' <-
    Registration.registerProposal pm pv pps avs dms regSubSt proposal
      `wrapError` Registration
  pure $!
    st { registeredProtocolUpdateProposals = rpus'
       , registeredSoftwareUpdateProposals = raus'
       , proposalRegistrationSlot = M.insert (recoverUpId proposal) currentSlot pws
       }
  where
    Environment
      { protocolMagic = pm
      , currentSlot
      , delegationMap = dms
      } = env

    State
      { adoptedProtocolVersion = pv
      , adoptedProtocolParams = pps
      , appVersions = avs
      , registeredProtocolUpdateProposals = rpus
      , registeredSoftwareUpdateProposals = raus
      , proposalRegistrationSlot = pws
      } = st

    regSubSt = Registration.State rpus raus

-- | Register a vote for the given proposal.
--
-- If the proposal gets enough confirmations after adding the given vote, then
-- it will get added to the set of confirmed proposals.
-- This corresponds to the @UPIVOTE@ rules in the spec.
--
registerVote
  :: MonadError Error m
  => Environment
  -> State
  -> AVote ByteString
  -> m State
registerVote env st vote = do
  Voting.State vts' cps' <-
    Voting.registerVoteWithConfirmation pm subEnv subSt vote
      `wrapError` Voting
  let
    avsNew =
      M.fromList $! [ (svAppName sv, (svNumber sv, sn))
                    | (pid, sv) <- M.toList raus
                    , pid `elem` M.keys cps'
                    ]
  pure $!
    st { confirmedProposals = cps'
       , proposalVotes = vts'
       , appVersions = M.union avsNew avs
       , registeredSoftwareUpdateProposals = M.withoutKeys raus (M.keysSet cps)
       }
       -- TODO: consider using the `Relation` instances from `fm-ledger-rules` (see `Ledger.Core`)

  where

    Environment
      { protocolMagic = pm
      , currentSlot = sn
      , delegationMap = dms
      } = env

    State
      { adoptedProtocolParams = pps
      , proposalRegistrationSlot
      , proposalVotes = vts
      , confirmedProposals = cps
      , appVersions =  avs
      , registeredSoftwareUpdateProposals = raus
      } = st

    rups = M.keysSet proposalRegistrationSlot

    subEnv = Voting.Environment sn pps (Voting.RegistrationEnvironment rups dms)

    subSt = Voting.State vts cps
