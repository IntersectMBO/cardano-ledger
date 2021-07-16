{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Blockchain interface validation rules.
module Cardano.Chain.Update.Validation.Interface
  ( -- * Environment
    Environment (..),

    -- * State
    State (..),
    initialState,

    -- * Signal
    Signal (..),

    -- * Error
    Error (..),

    -- * Interface functions
    registerUpdate,
    registerProposal,
    registerVote,
    registerEndorsement,
    registerEpoch,
  )
where

import Cardano.Binary
  ( Annotated,
    Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    enforceSize,
    matchSize,
  )
import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.Common.KeyHash (KeyHash)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.ProtocolConstants (kEpochSlots)
import Cardano.Chain.Slotting
  ( EpochNumber,
    SlotCount (SlotCount),
    SlotNumber,
    epochFirstSlot,
    subSlotCount,
    unSlotNumber,
  )
import Cardano.Chain.Update.Proposal (AProposal, UpId, recoverUpId)
import Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters,
    ppUpdateProposalTTL,
    upAdptThd,
  )
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion (..))
import Cardano.Chain.Update.SoftwareVersion
  ( svAppName,
    svNumber,
  )
import Cardano.Chain.Update.Validation.Endorsement
  ( CandidateProtocolUpdate,
    Endorsement,
    endorsementProtocolVersion,
  )
import qualified Cardano.Chain.Update.Validation.Endorsement as Endorsement
import Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump
  ( tryBumpVersion,
  )
import qualified Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump as PVBump
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import qualified Cardano.Chain.Update.Validation.Voting as Voting
import Cardano.Chain.Update.Vote (AVote)
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Prelude hiding (State)
import qualified Data.Map.Strict as M
import Data.Set (union)
import qualified Data.Set as S
import NoThunks.Class (NoThunks (..))

data Environment = Environment
  { protocolMagic :: !(Annotated ProtocolMagicId ByteString),
    -- | TODO: this is the chain security parameter, a.k.a. @stableAfter@, it is not part
    -- of our protocol parameters, so it seems that we need to pass it in the
    -- environment. However we need to double-check this with others.
    k :: !BlockCount,
    currentSlot :: !SlotNumber,
    -- | Number of genesis keys. This is used to calculate the proportion of
    -- genesis keys that need to endorse a new protocol version for it to be
    -- considered for adoption. See
    -- @Cardano.Chain.Update.Validation.Endorsement.Environment@.
    numGenKeys :: !Word8,
    delegationMap :: !Delegation.Map
  }

-- | Update interface state.
data State = State
  { -- | Current epoch
    currentEpoch :: !EpochNumber,
    adoptedProtocolVersion :: !ProtocolVersion,
    -- | Adopted protocol parameters
    adoptedProtocolParameters :: !ProtocolParameters,
    -- | Candidate protocol versions
    candidateProtocolUpdates :: ![CandidateProtocolUpdate],
    -- | Current application versions
    appVersions :: !Registration.ApplicationVersions,
    -- | Registered protocol update proposals
    registeredProtocolUpdateProposals :: !Registration.ProtocolUpdateProposals,
    -- | Registered software update proposals
    registeredSoftwareUpdateProposals :: !Registration.SoftwareUpdateProposals,
    -- | Confirmed update proposals
    confirmedProposals :: !(Map UpId SlotNumber),
    -- | Update proposals votes
    proposalVotes :: !(Map UpId (Set KeyHash)),
    -- | Update proposals endorsements
    registeredEndorsements :: !(Set Endorsement),
    -- | Slot at which an update proposal was registered
    proposalRegistrationSlot :: !(Map UpId SlotNumber)
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance FromCBOR State where
  fromCBOR = do
    enforceSize "State" 11
    State
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance ToCBOR State where
  toCBOR s =
    encodeListLen 11
      <> toCBOR (currentEpoch s)
      <> toCBOR (adoptedProtocolVersion s)
      <> toCBOR (adoptedProtocolParameters s)
      <> toCBOR (candidateProtocolUpdates s)
      <> toCBOR (appVersions s)
      <> toCBOR (registeredProtocolUpdateProposals s)
      <> toCBOR (registeredSoftwareUpdateProposals s)
      <> toCBOR (confirmedProposals s)
      <> toCBOR (proposalVotes s)
      <> toCBOR (registeredEndorsements s)
      <> toCBOR (proposalRegistrationSlot s)

data Error
  = Registration Registration.Error
  | Voting Voting.Error
  | Endorsement Endorsement.Error
  | NumberOfGenesisKeysTooLarge (Registration.TooLarge Int)
  deriving (Eq, Show)

instance ToCBOR Error where
  toCBOR err = case err of
    Registration registrationErr ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR registrationErr
    Voting votingErr ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR votingErr
    Endorsement endorsementErr ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR endorsementErr
    NumberOfGenesisKeysTooLarge tooLarge ->
      encodeListLen 2
        <> toCBOR (3 :: Word8)
        <> toCBOR tooLarge

instance FromCBOR Error where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "Interface.Error" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> Registration <$> fromCBOR
      1 -> checkSize 2 >> Voting <$> fromCBOR
      2 -> checkSize 2 >> Endorsement <$> fromCBOR
      3 -> checkSize 2 >> NumberOfGenesisKeysTooLarge <$> fromCBOR
      _ -> cborError $ DecoderErrorUnknownTag "Interface.Error" tag

-- | Signal combining signals from various rules
data Signal = Signal
  { proposal :: !(Maybe (AProposal ByteString)),
    votes :: ![AVote ByteString],
    endorsement :: !Endorsement
  }

-- | Initial update interface state
initialState :: Genesis.Config -> State
initialState config =
  State
    { currentEpoch = 0,
      adoptedProtocolVersion = ProtocolVersion 0 0 0,
      adoptedProtocolParameters = Genesis.configProtocolParameters config,
      candidateProtocolUpdates = [],
      appVersions = mempty,
      registeredProtocolUpdateProposals = mempty,
      registeredSoftwareUpdateProposals = mempty,
      confirmedProposals = mempty,
      proposalVotes = mempty,
      registeredEndorsements = mempty,
      proposalRegistrationSlot = mempty
    }

-- | Group together the other registration rules in a single rule
--
--   This corresponds to the @BUPI@ rule in the Byron chain specification.
registerUpdate ::
  MonadError Error m => Environment -> State -> Signal -> m State
registerUpdate env st Signal {proposal, votes, endorsement} = do
  -- Register proposal if it exists
  st' <- case proposal of
    Nothing -> pure st
    Just p -> registerProposal env st p

  -- Register the votes
  st'' <- registerVotes env st' votes

  -- Register endorsement
  registerEndorsement env st'' endorsement

-- | Register an update proposal.
--
-- This corresponds to the @UPIREG@ rule in the Byron ledger specification.
registerProposal ::
  MonadError Error m =>
  Environment ->
  State ->
  AProposal ByteString ->
  m State
registerProposal env st proposal = do
  Registration.State registeredProtocolUpdateProposals' registeredSoftwareUpdateProposals' <-
    Registration.registerProposal subEnv subSt proposal
      `wrapError` Registration
  pure
    $! st
      { registeredProtocolUpdateProposals = registeredProtocolUpdateProposals',
        registeredSoftwareUpdateProposals = registeredSoftwareUpdateProposals',
        proposalRegistrationSlot =
          M.insert (recoverUpId proposal) currentSlot proposalRegistrationSlot
      }
  where
    Environment
      { protocolMagic,
        currentSlot,
        delegationMap
      } = env

    State
      { adoptedProtocolVersion,
        adoptedProtocolParameters,
        appVersions,
        registeredProtocolUpdateProposals,
        registeredSoftwareUpdateProposals,
        proposalRegistrationSlot
      } = st

    subEnv =
      Registration.Environment
        protocolMagic
        currentSlot
        adoptedProtocolVersion
        adoptedProtocolParameters
        appVersions
        delegationMap

    subSt =
      Registration.State
        registeredProtocolUpdateProposals
        registeredSoftwareUpdateProposals

-- | Register a sequence of votes.
--
-- After applying the votes, we check for confirmed proposals, and update the
-- application versions according to the proposals that, in the new state, are
-- confirmed and stable.
--
-- This corresponds to the @UPIVOTES@ rule in the Byron ledger
-- specification.
registerVotes ::
  MonadError Error m =>
  Environment ->
  State ->
  [AVote ByteString] ->
  m State
registerVotes env st votes = do
  st' <- foldM (registerVote env) st votes
  let Environment
        { currentSlot
        } = env

      State
        { confirmedProposals,
          appVersions,
          registeredSoftwareUpdateProposals
        } = st'

      confirmedApplicationUpdates =
        M.restrictKeys
          registeredSoftwareUpdateProposals
          (M.keysSet confirmedProposals)
      appVersions' =
        M.fromList $
          [ (svAppName sv, av)
            | (pid, sup) <- M.toList registeredSoftwareUpdateProposals,
              pid `elem` M.keys confirmedApplicationUpdates,
              let Registration.SoftwareUpdateProposal sv metadata = sup
                  av = Registration.ApplicationVersion (svNumber sv) currentSlot metadata
          ]
  pure $
    st' -- Note that it's important that the new application versions are passed
    -- as the first argument of @M.union@, since the values in this first
    -- argument overwrite the values in the second.
      { appVersions = M.union appVersions' appVersions,
        -- TODO: consider using the `Relation` instances from `cardano-ledger-specs` (see `Ledger.Core`)
        registeredSoftwareUpdateProposals =
          M.withoutKeys
            registeredSoftwareUpdateProposals
            (M.keysSet confirmedProposals)
      }

-- | Register a vote for the given proposal.
--
-- This corresponds to the @UPIVOTE@ rule in the Byron ledger
registerVote ::
  MonadError Error m =>
  Environment ->
  State ->
  AVote ByteString ->
  m State
registerVote env st vote = do
  Voting.State proposalVotes' confirmedProposals' <-
    Voting.registerVoteWithConfirmation protocolMagic subEnv subSt vote
      `wrapError` Voting
  pure
    $! st
      { confirmedProposals = confirmedProposals',
        proposalVotes = proposalVotes'
      }
  where
    Environment
      { protocolMagic,
        currentSlot,
        numGenKeys,
        delegationMap
      } = env

    State
      { adoptedProtocolParameters,
        proposalRegistrationSlot,
        proposalVotes,
        confirmedProposals
      } = st

    rups = M.keysSet proposalRegistrationSlot

    subEnv =
      Voting.Environment
        currentSlot
        (upAdptThd numGenKeys adoptedProtocolParameters)
        (Voting.RegistrationEnvironment rups delegationMap)

    subSt = Voting.State proposalVotes confirmedProposals

-- | Register an endorsement.
--
-- An endorsement represents the fact that a genesis key is ready to start using
-- the protocol version being endorsed. In the decentralized era only genesis
-- key holders can endorse protocol versions.
--
-- This corresponds to the @UPIEND@ rule in the Byron ledger
-- specification.
registerEndorsement ::
  MonadError Error m =>
  Environment ->
  State ->
  Endorsement ->
  m State
registerEndorsement env st endorsement = do
  Endorsement.State candidateProtocolUpdates' registeredEndorsements' <-
    Endorsement.register subEnv subSt endorsement
      `wrapError` Endorsement
  let pidsKeep = nonExpiredPids `union` confirmedPids

      nonExpiredPids =
        M.keysSet $ M.filter (subSlotCount u currentSlot <=) proposalRegistrationSlot

      confirmedPids = M.keysSet confirmedProposals

      registeredProtocolUpdateProposals' =
        M.restrictKeys registeredProtocolUpdateProposals pidsKeep

      vsKeep =
        S.fromList $
          Registration.pupProtocolVersion
            <$> M.elems registeredProtocolUpdateProposals'

  pure
    $! st
      { candidateProtocolUpdates = forceElemsToWHNF candidateProtocolUpdates',
        registeredProtocolUpdateProposals = registeredProtocolUpdateProposals',
        registeredSoftwareUpdateProposals =
          M.restrictKeys registeredSoftwareUpdateProposals pidsKeep,
        proposalVotes =
          M.restrictKeys proposalVotes pidsKeep,
        registeredEndorsements =
          S.filter ((`S.member` vsKeep) . endorsementProtocolVersion) registeredEndorsements',
        proposalRegistrationSlot =
          M.restrictKeys proposalRegistrationSlot pidsKeep
      }
  where
    subEnv =
      Endorsement.Environment
        k
        currentSlot
        (upAdptThd numGenKeys adoptedProtocolParameters)
        delegationMap
        confirmedProposals
        registeredProtocolUpdateProposals

    Environment
      { k,
        currentSlot,
        numGenKeys,
        delegationMap
      } = env

    State
      { adoptedProtocolParameters,
        confirmedProposals,
        registeredProtocolUpdateProposals,
        registeredSoftwareUpdateProposals,
        candidateProtocolUpdates,
        proposalVotes,
        registeredEndorsements,
        proposalRegistrationSlot
      } = st

    subSt =
      Endorsement.State
        candidateProtocolUpdates
        registeredEndorsements

    u = SlotCount . unSlotNumber . ppUpdateProposalTTL $ adoptedProtocolParameters

-- | Register an epoch. Whenever an epoch number is seen on a block this epoch
-- number should be passed to this function so that on epoch change the
-- protocol parameters can be updated, provided that there is an update
-- candidate that was accepted and endorsed by a majority of the genesis keys.
--
-- This corresponds to the @UPIEC@ rules in the Byron ledger specification.
registerEpoch ::
  Environment ->
  State ->
  -- | Epoch seen on the block.
  EpochNumber ->
  State
registerEpoch env st lastSeenEpoch = do
  let PVBump.State
        adoptedProtocolVersion'
        nextProtocolParameters' =
          tryBumpVersion subEnv subSt
  if adoptedProtocolVersion' == adoptedProtocolVersion
    then -- Nothing changes in the state, since we are not changing protocol
    -- versions. This happens when either the epoch does not change (and
    -- therefore the protocol parameters cannot change) or there are no
    -- update proposals that can be adopted (either because there are no
    -- candidates or they do not fulfill the requirements for adoption).
      st
    else -- We have a new protocol version, so we update the current protocol
    -- version and parameters, and we perform a cleanup of the state
    -- variables.

      st
        { adoptedProtocolVersion = adoptedProtocolVersion',
          adoptedProtocolParameters = nextProtocolParameters',
          candidateProtocolUpdates = [],
          registeredProtocolUpdateProposals = M.empty,
          registeredSoftwareUpdateProposals = M.empty,
          confirmedProposals = M.empty,
          proposalVotes = M.empty,
          registeredEndorsements = S.empty,
          proposalRegistrationSlot = M.empty
        }
  where
    subEnv = PVBump.Environment k firstSlotOfLastSeenEpoch candidateProtocolUpdates

    subSt =
      PVBump.State
        adoptedProtocolVersion
        adoptedProtocolParameters

    firstSlotOfLastSeenEpoch = epochFirstSlot (kEpochSlots k) lastSeenEpoch

    Environment
      { k
      } = env

    State
      { adoptedProtocolVersion,
        adoptedProtocolParameters,
        candidateProtocolUpdates
      } = st
