{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}

-- | Blockchain interface validation rules.
--
module Cardano.Chain.Update.Validation.Interface
  (
  -- * Environment
    Environment(..)

  -- * State
  , State(..)
  , initialState

  -- * Signal
  , Signal(..)

  -- *Error
  , Error(..)

  -- * Interface functions
  , registerUpdate
  , registerProposal
  , registerVote
  , registerEndorsement
  , registerEpoch
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import Data.Set (union)
import qualified Data.Set as S

import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.Common.StakeholderId (StakeholderId)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting (EpochIndex, FlatSlotId)

import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters
  , ppUpdateImplicit
  )
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion(..))
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion
  , SoftwareVersion
  , svAppName
  , svNumber
  )
import Cardano.Chain.Update.Validation.Endorsement
  ( CandidateProtocolUpdate
  , Endorsement
  , endorsementProtocolVersion
  )
import qualified Cardano.Chain.Update.Validation.Endorsement as Endorsement
import Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump
  ( tryBumpVersion
  )
import qualified Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump as PVBump
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import qualified Cardano.Chain.Update.Validation.Voting as Voting
import Cardano.Chain.Update.Vote (AProposal, AVote, UpId, recoverUpId)
import Cardano.Crypto (ProtocolMagicId)


data Environment = Environment
  { protocolMagic :: !ProtocolMagicId
  , k             :: !BlockCount
  -- ^ TODO: this is the chain security parameter, a.k.a. @stableAfter@, it is not part
  -- of our protocol parameters, so it seems that we need to pass it in the
  -- environment. However we need to double-check this with others.
  , currentSlot   :: !FlatSlotId
  , numGenKeys    :: !Word8
  -- ^ Number of genesis keys. This is used to calculate the proportion of
  -- genesis keys that need to endorse a new protocol version for it to be
  -- considered for adoption. See
  -- @Cardano.Chain.Update.Validation.Endorsement.Environment@.
  , delegationMap :: !(Map StakeholderId StakeholderId)
  }

-- | Update interface state.
data State = State
  { currentEpoch                      :: !EpochIndex
    -- ^ Current epoch
  , adoptedProtocolVersion            :: !ProtocolVersion
  , adoptedProtocolParameters         :: !ProtocolParameters
    -- ^ Adopted protocol parameters
  , candidateProtocolUpdates          :: ![CandidateProtocolUpdate]
    -- ^ Candidate protocol versions
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
  , registeredEndorsements            :: !(Set Endorsement)
    -- ^ Update proposals endorsements
  , proposalRegistrationSlot          :: !(Map UpId FlatSlotId)
    -- ^ Slot at which an update proposal was registered
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

data Error
  = Registration Registration.Error
  | Voting Voting.Error
  | Endorsement Endorsement.Error
  | NumberOfGenesisKeysTooLarge (Registration.TooLarge Int)
  deriving (Eq, Show)


-- | Signal combining signals from various rules
data Signal = Signal
  { proposal    :: !(Maybe (AProposal ByteString))
  , votes       :: ![AVote ByteString]
  , endorsement :: !Endorsement
  }


-- | Initial update interface state
initialState :: Genesis.Config -> State
initialState config = State
  { currentEpoch                      = 0
  , adoptedProtocolVersion            = ProtocolVersion 0 0 0
  , adoptedProtocolParameters         = Genesis.configProtocolParameters config
  , candidateProtocolUpdates          = []
  , appVersions                       = mempty
  , registeredProtocolUpdateProposals = mempty
  , registeredSoftwareUpdateProposals = mempty
  , confirmedProposals                = mempty
  , proposalVotes                     = mempty
  , registeredEndorsements            = mempty
  , proposalRegistrationSlot          = mempty
  }


-- | Group together the other registration rules in a single rule
--
--   This corresponds to the @BUPI@ rule in the Byron chain specification.
registerUpdate
  :: MonadError Error m => Environment -> State -> Signal -> m State
registerUpdate env st Signal { proposal, votes, endorsement } = do
  -- Register proposal if it exists
  st' <- case proposal of
    Nothing -> pure st
    Just p  -> registerProposal env st p

  -- Register the votes
  st'' <- foldM (registerVote env) st' votes

  -- Register endorsement
  registerEndorsement env st'' endorsement


-- | Register an update proposal.
--
-- This corresponds to the @UPIREG@ rule in the Byron ledger specification.
registerProposal
  :: MonadError Error m
  => Environment
  -> State
  -> AProposal ByteString
  -> m State
registerProposal env st proposal = do
  Registration.State registeredProtocolUpdateProposals' registeredSoftwareUpdateProposals'
    <- Registration.registerProposal subEnv subSt proposal
       `wrapError` Registration
  pure $!
    st { registeredProtocolUpdateProposals = registeredProtocolUpdateProposals'
       , registeredSoftwareUpdateProposals = registeredSoftwareUpdateProposals'
       , proposalRegistrationSlot =
           M.insert (recoverUpId proposal) currentSlot proposalRegistrationSlot
       }

  where
    Environment
      { protocolMagic
      , currentSlot
      , delegationMap
      } = env

    State
      { adoptedProtocolVersion
      , adoptedProtocolParameters
      , appVersions
      , registeredProtocolUpdateProposals
      , registeredSoftwareUpdateProposals
      , proposalRegistrationSlot
      } = st

    subEnv =
      Registration.Environment
        protocolMagic
        adoptedProtocolVersion
        adoptedProtocolParameters
        appVersions
        delegationMap

    subSt =
      Registration.State
        registeredProtocolUpdateProposals
        registeredSoftwareUpdateProposals

-- | Register a vote for the given proposal.
--
-- If the proposal gets enough confirmations after adding the given vote, then
-- it will get added to the set of confirmed proposals.
--
-- This corresponds to the @UPIVOTE@ rule in the Byron ledger
-- specification.
--
registerVote
  :: MonadError Error m
  => Environment
  -> State
  -> AVote ByteString
  -> m State
registerVote env st vote = do
  Voting.State proposalVotes' confirmedProposals'
    <- Voting.registerVoteWithConfirmation protocolMagic subEnv subSt vote
      `wrapError` Voting
  let
    appVersions' =
      M.fromList $! [ (svAppName sv, (svNumber sv, currentSlot))
                    | (pid, sv) <- M.toList registeredSoftwareUpdateProposals
                    , pid `elem` M.keys confirmedProposals'
                    ]
  pure $!
    st { confirmedProposals = confirmedProposals'
       , proposalVotes = proposalVotes'
       -- Note that it's important that the new application versions are passed
       -- as the first argument of @M.union@, since the values in this first
       -- argument overwrite the values in the second.
       , appVersions = M.union appVersions' appVersions
       , registeredSoftwareUpdateProposals =
           M.withoutKeys
             registeredSoftwareUpdateProposals
             (M.keysSet confirmedProposals)
       }
       -- TODO: consider using the `Relation` instances from `fm-ledger-rules` (see `Ledger.Core`)

  where
    Environment
      { protocolMagic
      , currentSlot
      , delegationMap
      } = env

    State
      { adoptedProtocolParameters
      , proposalRegistrationSlot
      , proposalVotes
      , confirmedProposals
      , appVersions
      , registeredSoftwareUpdateProposals
      } = st

    rups = M.keysSet proposalRegistrationSlot

    subEnv =
      Voting.Environment
        currentSlot
        adoptedProtocolParameters
        (Voting.RegistrationEnvironment rups delegationMap)

    subSt = Voting.State proposalVotes confirmedProposals

-- | Register an endorsement.
--
-- An endorsement represents the fact that a genesis stakeholder is ready to
-- start using the protocol version being endorsed. In the decentralized era
-- only genesis key holders can endorse protocol versions.
--
-- This corresponds to the @UPIEND@ rule in the Byron ledger
-- specification.
registerEndorsement
  :: MonadError Error m
  => Environment
  -> State
  -> Endorsement
  -> m State
registerEndorsement env st endorsement = do
  Endorsement.State candidateProtocolUpdates' registeredEndorsements'
    <- Endorsement.register subEnv subSt endorsement
       `wrapError` Endorsement
  let
    pidsKeep = nonExpiredPids `union` confirmedPids

    nonExpiredPids =
      M.keysSet $ M.filter (currentSlot - u <=) proposalRegistrationSlot

    confirmedPids = M.keysSet confirmedProposals

    registeredProtocolUpdateProposals' =
      M.restrictKeys registeredProtocolUpdateProposals pidsKeep

    vsKeep = S.fromList $ fst <$> M.elems registeredProtocolUpdateProposals'

  pure $!
    st { candidateProtocolUpdates = candidateProtocolUpdates'
       , registeredProtocolUpdateProposals = registeredProtocolUpdateProposals'
       , registeredSoftwareUpdateProposals =
           M.restrictKeys registeredSoftwareUpdateProposals pidsKeep
       , proposalVotes =
           M.restrictKeys proposalVotes pidsKeep
       , registeredEndorsements =
           S.filter ((`S.member` vsKeep) . endorsementProtocolVersion) registeredEndorsements'
       , proposalRegistrationSlot =
           M.restrictKeys proposalRegistrationSlot pidsKeep
       }

  where
    subEnv =
      Endorsement.Environment
        k
        currentSlot
        delegationMap
        adoptedProtocolParameters
        confirmedProposals
        registeredProtocolUpdateProposals
        numGenKeys

    Environment
      { k
      , currentSlot
      , numGenKeys
      , delegationMap
      } = env

    State
      { adoptedProtocolParameters
      , confirmedProposals
      , registeredProtocolUpdateProposals
      , registeredSoftwareUpdateProposals
      , candidateProtocolUpdates
      , proposalVotes
      , registeredEndorsements
      , proposalRegistrationSlot
      } = st

    subSt =
      Endorsement.State
        candidateProtocolUpdates
        registeredEndorsements

    u = ppUpdateImplicit adoptedProtocolParameters

-- | Register an epoch. Whenever an epoch number is seen on a block this epoch
-- number should be passed to this function so that on epoch change the
-- protocol parameters can be updated, provided that there is an update
-- candidate that was accepted and endorsed by a majority of the genesis keys.
--
-- This corresponds to the @UPIEC@ rules in the Byron ledger specification.
registerEpoch
  :: MonadError Error m
  => Environment
  -> State
  -> EpochIndex
  -- ^ Epoch seen on the block.
  -> m State
registerEpoch env st lastSeenEpoch = do
  let PVBump.State
        currentEpoch'
        adoptedProtocolVersion'
        nextProtocolParameters'
        = tryBumpVersion subEnv subSt lastSeenEpoch
  if adoptedProtocolVersion' == adoptedProtocolVersion
    then
      -- Nothing changes in the state, since we are not changing protocol
      -- versions. This happens when either the epoch does not change (and
      -- therefore the protocol parameters cannot change) or there are no
      -- update proposals that can be adopted (either because there are no
      -- candidates or they do not fulfill the requirements for adoption).
      pure $! st
    else
      -- We have a new protocol version, so we update the current protocol
      -- version and parameters, and we perform a cleanup of the state
      -- variables.
      pure $!
        st { currentEpoch = currentEpoch'
           , adoptedProtocolVersion = adoptedProtocolVersion'
           , adoptedProtocolParameters = nextProtocolParameters'
           , candidateProtocolUpdates = []
           , registeredProtocolUpdateProposals = M.empty
           , confirmedProposals = M.empty
           , proposalVotes = M.empty
           , registeredEndorsements = S.empty
           , proposalRegistrationSlot = M.empty
           }
  where
    subEnv = PVBump.Environment k currentSlot candidateProtocolUpdates

    subSt =
      PVBump.State
        currentEpoch
        adoptedProtocolVersion
        adoptedProtocolParameters


    Environment
      { k
      , currentSlot
      } = env

    State
      { currentEpoch
      , adoptedProtocolVersion
      , adoptedProtocolParameters
      , candidateProtocolUpdates
      } = st
