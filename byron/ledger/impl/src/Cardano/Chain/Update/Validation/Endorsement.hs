{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Update.Validation.Endorsement
  ( Environment (..)
  , State (..)
  , Endorsement (..)
  , CandidateProtocolUpdate (..)
  , register
  , Error (..)
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Binary
  ( DecoderError(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeWord8
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Common (BlockCount, KeyHash)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting (SlotNumber, subSlotCount)
import Cardano.Chain.Update.Proposal (UpId)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import qualified Cardano.Chain.Update.Validation.Registration as Registration


data Environment = Environment
  { k                                 :: !BlockCount
  -- ^ Chain stability parameter.
  , currentSlot                       :: !SlotNumber
  , adoptionThreshold                 :: !Int
  , delegationMap                     :: !Delegation.Map
  , confirmedProposals                :: !(Map UpId SlotNumber)
  , registeredProtocolUpdateProposals :: !Registration.ProtocolUpdateProposals
  }

data State = State
  { candidateProtocolVersions :: ![CandidateProtocolUpdate]
  , registeredEndorsements    :: !(Set Endorsement)
  }

data CandidateProtocolUpdate = CandidateProtocolUpdate
  { cpuSlot               :: !SlotNumber
    -- ^ Slot at which this protocol version and parameters gathered enough
    -- endorsements and became a candidate. This is used to check which
    -- versions became candidates 2k slots before the end of an epoch (and only
    -- those can be adopted at that epoch). Versions that became candidates
    -- later than 2k slots before the end of an epoch can be adopted in
    -- following epochs.
  , cpuProtocolVersion    :: !ProtocolVersion
  , cpuProtocolParameters :: !ProtocolParameters
  } deriving (Eq, Show, Generic)
    deriving anyclass (NFData, NoUnexpectedThunks)

instance FromCBOR CandidateProtocolUpdate where
  fromCBOR = do
    enforceSize "CandidateProtocolUpdate" 3
    CandidateProtocolUpdate
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance ToCBOR CandidateProtocolUpdate where
  toCBOR cpu =
    encodeListLen 3
      <> toCBOR (cpuSlot cpu)
      <> toCBOR (cpuProtocolVersion cpu)
      <> toCBOR (cpuProtocolParameters cpu)

data Endorsement = Endorsement
  { endorsementProtocolVersion :: !ProtocolVersion
  , endorsementKeyHash         :: !KeyHash
  } deriving (Eq, Show, Ord, Generic)
    deriving anyclass (NFData, NoUnexpectedThunks)

instance FromCBOR Endorsement where
  fromCBOR = do
    enforceSize "Endorsement" 2
    Endorsement
      <$> fromCBOR
      <*> fromCBOR

instance ToCBOR Endorsement where
  toCBOR sh =
    encodeListLen 2
      <> toCBOR (endorsementProtocolVersion sh)
      <> toCBOR (endorsementKeyHash sh)

data Error
  = MultipleProposalsForProtocolVersion ProtocolVersion
  -- ^ Multiple proposals were found, which propose an update to the same
  -- protocol version.
  deriving (Eq, Show)

instance ToCBOR Error where
  toCBOR (MultipleProposalsForProtocolVersion protocolVersion) =
    encodeListLen 2
      <> toCBOR (0 :: Word8)
      <> toCBOR protocolVersion

instance FromCBOR Error where
  fromCBOR = do
    enforceSize "Endorsement.Error" 2
    tag <- decodeWord8
    case tag of
      0 -> MultipleProposalsForProtocolVersion <$> fromCBOR
      _ -> cborError   $  DecoderErrorUnknownTag "Endorsement.Error" tag

-- | Register an endorsement.
--
-- This corresponds to the @UPEND@ rule.
register
  :: MonadError Error m => Environment -> State -> Endorsement -> m State
register env st endorsement =
  case M.toList (M.filter ((== pv) . Registration.pupProtocolVersion)
                          registeredProtocolUpdateProposals) of
    -- We ignore endorsement of proposals that aren't registered
    [] -> pure st

    -- Try to register the endorsement and check if we can adopt the proposal
    [(upId, Registration.ProtocolUpdateProposal _ pps')] -> if isConfirmedAndStable upId
      then if numberOfEndorsements >= adoptionThreshold
        -- Register the endorsement and adopt the proposal in the next epoch
        then do
          let
            cpu = CandidateProtocolUpdate
              { cpuSlot = currentSlot
              , cpuProtocolVersion = pv
              , cpuProtocolParameters = pps'
              }
            cpus' =
              updateCandidateProtocolUpdates candidateProtocolVersions cpu
          pure $ State
            { candidateProtocolVersions = cpus'
            , registeredEndorsements    = registeredEndorsements'
            }

        -- Just register the endorsement if we cannot adopt
        else pure $ st { registeredEndorsements = registeredEndorsements' }

      -- Ignore the endorsement if the registration isn't stable
      else pure st

    -- Throw an error if there are multiple proposals for this protocol version
    _ -> throwError $ MultipleProposalsForProtocolVersion pv
 where
  Environment
    { k
    , currentSlot
    , adoptionThreshold
    , delegationMap
    , confirmedProposals
    , registeredProtocolUpdateProposals
    } = env

  isConfirmedAndStable upId = upId `M.member` scps
   where
    -- Stable and confirmed proposals.
    scps     = M.filter (<= stableAt) confirmedProposals
    stableAt = subSlotCount (kSlotSecurityParam k) currentSlot

  numberOfEndorsements :: Int
  numberOfEndorsements = length $ Set.filter
    ((== pv) . endorsementProtocolVersion)
    registeredEndorsements'

  pv = endorsementProtocolVersion endorsement

  State { candidateProtocolVersions, registeredEndorsements } = st

  registeredEndorsements' = case Delegation.lookupR vk delegationMap of
    Just vkS -> Set.insert (Endorsement epv vkS) registeredEndorsements
    Nothing  -> registeredEndorsements
      -- Note that we do not throw an error if there is no corresponding
      -- delegate for the given endorsement keyHash. This is consistent
      -- with the @UPEND@ rules. The check that there is a delegator should be
      -- done in the rule that checks that the block issuer is a delegate of a
      -- genesis key.
   where
    vk  = endorsementKeyHash endorsement
    epv = endorsementProtocolVersion endorsement


-- | Add a newly endorsed protocol version to the 'CandidateProtocolUpdate's
--
--   We only add it to the list if the 'ProtocolVersion' is strictly greater
--   than all other `CandidateProtocolUpdate`s
--
-- This corresponds to the @FADS@ rule.
updateCandidateProtocolUpdates
  :: [CandidateProtocolUpdate]
  -> CandidateProtocolUpdate
  -> [CandidateProtocolUpdate]
updateCandidateProtocolUpdates [] cpu = [cpu]
updateCandidateProtocolUpdates cpus@(cpu : _) cpu'
  | cpuProtocolVersion cpu < cpuProtocolVersion cpu' = cpu' : cpus
  | otherwise = cpus
