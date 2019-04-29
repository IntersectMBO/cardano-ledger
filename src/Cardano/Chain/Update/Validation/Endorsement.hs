{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Cardano.Chain.Update.Validation.Endorsement
  ( Environment (..)
  , State (..)
  , Endorsement (..)
  , CandidateProtocolUpdate (..)
  , register
  , Error
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Cardano.Chain.Common
import Cardano.Chain.Delegation.Validation.Activation (delegatorOf)
import Cardano.Chain.Slotting
import Cardano.Chain.Update.Proposal (UpId)
import Cardano.Chain.Update.ProtocolParameters
import Cardano.Chain.Update.ProtocolVersion
import Cardano.Chain.Update.SoftforkRule
import qualified Cardano.Chain.Update.Validation.Registration as Registration


data Environment = Environment
  { k                                 :: !BlockCount
  -- ^ Chain stability parameter.
  , currentSlot                       :: !FlatSlotId
  , delegationMap                     :: !(Map StakeholderId StakeholderId)
  , adoptedProtocolParameters         :: !ProtocolParameters
  , confirmedProposals                :: !(Map UpId FlatSlotId)
  , registeredProtocolUpdateProposals :: !Registration.ProtocolUpdateProposals
  , numGenesisKeys                    :: !Word8
  -- ^ Number of genesis keys. This is used in combination with the
  -- 'ppUpdateProposalThd' protocol parameter to calculate the proportion of
  -- genesis keys that need to endorse a new protocol version for it to be
  -- considered for adoption.
  }

data State = State
  { candidateProtocolVersions :: ![CandidateProtocolUpdate]
  , registeredEndorsements    :: !(Set Endorsement)
  }

data CandidateProtocolUpdate = CandidateProtocolUpdate
  { cpuSlot               :: !FlatSlotId
    -- ^ Slot at which this protocol version and parameters gathered enough
    -- endorsements and became a candidate. This is used to check which
    -- versions became candidates 2k slots before the end of an epoch (and only
    -- those can be adopted at that epoch). Versions that became candidates
    -- later than 2k slots before the end of an epoch can be adopted in
    -- following epochs.
  , cpuProtocolVersion    :: !ProtocolVersion
  , cpuProtocolParameters :: !ProtocolParameters
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

data Endorsement = Endorsement
  { endorsementProtocolVersion :: !ProtocolVersion
  , endorsementStakeholder     :: !StakeholderId
  } deriving (Eq, Show, Ord, Generic)
    deriving anyclass NFData

data Error
  = MultipleProposalsForProtocolVersion ProtocolVersion
  -- ^ Multiple proposals were found, which propose an update to the same
  -- protocol version.
  deriving (Eq, Show)

-- | Register an endorsement.
--
-- This corresponds to the @UPEND@ rule.
register
  :: MonadError Error m => Environment -> State -> Endorsement -> m State
register env st endorsement =
  case M.toList (M.filter ((== pv) . fst) registeredProtocolUpdateProposals) of
    -- We ignore endorsement of proposals that aren't registered
    [] -> pure st

    -- Try to register the endorsement and check if we can adopt the proposal
    [(upId, (_, pps'))] -> if isConfirmedAndStable upId
      then if canAdopt numGenesisKeys pps registeredEndorsements' pv
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
    , delegationMap
    , confirmedProposals
    , registeredProtocolUpdateProposals
    , numGenesisKeys
    } = env

  isConfirmedAndStable upId = upId `M.member` scps
   where
    -- Stable and confirmed proposals.
    scps     = M.filter (<= stableAt) confirmedProposals
    stableAt = currentSlot - twice k

  pps = adoptedProtocolParameters env
  pv  = endorsementProtocolVersion endorsement

  State { candidateProtocolVersions, registeredEndorsements } = st

  registeredEndorsements' = case delegatorOf vk delegationMap of
    Just vkS -> Set.insert (Endorsement epv vkS) registeredEndorsements
    Nothing  -> registeredEndorsements
      -- Note that we do not throw an error if there is no corresponding
      -- delegate for the given endorsement stakeholder. This is consistent
      -- with the @UPEND@ rules. The check that there is a delegator should be
      -- done in the rule that checks that the block issuer is a delegate of a
      -- genesis key.
   where
    vk  = endorsementStakeholder endorsement
    epv = endorsementProtocolVersion endorsement

canAdopt
  :: Word8
  -- ^ Number of genesis keys.
  -> ProtocolParameters
  -> Set Endorsement
  -> ProtocolVersion
  -> Bool
canAdopt ngk pps endorsements protocolVersion =
  upAdptThd <= numberOfEndorsements
 where
  -- In Byron we do not have a @upAdptThd@ protocol parameter, so we have to
  -- use the existing ones.
  --
  -- @lovelacePortionToDouble . srMinThd . ppSoftforkRule@ will give us the
  -- ratio (in the interval @[0, 1]@) of the total stake that has to endorse a
  -- protocol version to become adopted. In genesis configuration, this ratio
  -- will evaluate to @0.6@, so if we have 7 genesis keys, @upAdptThd = 4@.
  upAdptThd :: Int
  upAdptThd = floor $ stakeRatio * fromIntegral ngk
   where
    stakeRatio = lovelacePortionToDouble . srMinThd . ppSoftforkRule $ pps

  numberOfEndorsements :: Int
  numberOfEndorsements = length $ Set.filter
    ((== protocolVersion) . endorsementProtocolVersion)
    endorsements

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
