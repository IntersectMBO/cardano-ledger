{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Cardano.Chain.Update.Validation.Endorsement
  ( registerEndorsement
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map as M
import qualified Data.Set as Set

import Cardano.Chain.Common
import Cardano.Chain.Delegation.Validation
import Cardano.Chain.Slotting
import Cardano.Chain.Update.ProtocolParameters
import Cardano.Chain.Update.ProtocolVersion
import Cardano.Chain.Update.Vote
import qualified Cardano.Chain.Update.Validation.Registration as Registration

data Environment = Environment
  { k                         :: !BlockCount
  -- ^ Chain stability parameter.
  , currentSlot               :: !FlatSlotId
  , delegationMap             :: !(Map StakeholderId StakeholderId)
  , adoptedProtocolParameters :: !ProtocolParameters
  , confirmedProposals        :: !(Map UpId FlatSlotId)
  , registeredUpdateProposals :: !Registration.ProtocolUpdateProposals
  , numGenesisKeys            :: !Word8
  -- ^ Number of genesis keys. This is used in combination with the
  -- 'ppUpdateProposalThd' protocol parameter to calculate the proportion of
  -- genesis keys that need to endorse a new protocol version for it to be considered for
  -- adoption.
  }

data State = State
  { futureProtocolVersions :: [CandidateProtocolVersion]
  , registeredEndorsements :: Set Endorsement
  }


data CandidateProtocolVersion = CandidateProtocolVersion
  { fpvSlot               :: FlatSlotId
  , fpvProtocolVersion    :: ProtocolVersion
  , fpvProtocolParameters :: ProtocolParameters
  }

data Endorsement = Endorsement
  { endorsementProtocolVersion :: ProtocolVersion
  , endorsementStakeholder     :: StakeholderId
  } deriving (Eq, Ord)

data Error
  = MultipleProposalsForProtocolVersion ProtocolVersion
  -- ^ Multiple proposals were found, which propose an update to the same
  -- protocol version.

-- | Register an endorsement.
--
-- This corresponds to the @UPEND@ rule.
registerEndorsement
  :: MonadError Error m => Environment -> State -> Endorsement -> m State
registerEndorsement env st endorsement =
  case M.toList (M.filter ((== pv) . fst) registeredUpdateProposals) of
    -- We ignore endorsement of proposals that aren't registered
    [] -> pure st

    -- Try to register the endorsement and check if we can adopt the proposal
    [(upId, (_, pps'))] -> if isConfirmedAndStable upId
      then if canAdopt numGenesisKeys pps registeredEndorsements' pv
-- Register the endorsement and adopt the proposal in the next epoch
        then do
          let
            fpv = CandidateProtocolVersion
              { fpvSlot = currentSlot
              , fpvProtocolVersion = pv
              , fpvProtocolParameters = pps'
              }
            fpvs' = updateCandidateProtocolVersions futureProtocolVersions fpv
          pure $ State
            { futureProtocolVersions = fpvs'
            , registeredEndorsements = registeredEndorsements'
            }

-- Just register the endorsement if we cannot adopt
        else pure $ st { registeredEndorsements = registeredEndorsements' }

-- Ignore the endorsement if the registration isn't stable
      else pure st

    -- Throw an error if there are multiple proposals for this protocol version
    _ -> throwError $ MultipleProposalsForProtocolVersion pv
 where
  Environment { k, currentSlot, delegationMap, confirmedProposals, registeredUpdateProposals, numGenesisKeys }
    = env

  isConfirmedAndStable upId = upId `M.member` scps
   where
      -- Stable and confirmed proposals.
    scps = M.filter (currentSlot - _2 k <=) confirmedProposals
    _2 x = 2 * unBlockCount x

  pps = adoptedProtocolParameters env
  pv  = endorsementProtocolVersion endorsement

  State { futureProtocolVersions, registeredEndorsements } = st

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
canAdopt n pps endorsements protocolVersion = t <= numberOfEndorsements
 where
  ProtocolParameters { ppUpdateProposalThd } = pps

  t :: Double
  t = lovelacePortionToDouble ppUpdateProposalThd * fromIntegral n

  numberOfEndorsements :: Double
  numberOfEndorsements = fromIntegral $ length $ Set.filter
    ((== protocolVersion) . endorsementProtocolVersion)
    endorsements

-- | Add a newly endorsed protocol version to the 'CandidateProtocolVersion's
--
--   We only add it to the list if the 'ProtocolVersion' is strictly greater
--   than all other `CandidateProtocolVersion`s
--
-- This corresponds to the @FADS@ rule.
updateCandidateProtocolVersions
  :: [CandidateProtocolVersion]
  -> CandidateProtocolVersion
  -> [CandidateProtocolVersion]
updateCandidateProtocolVersions [] fpv = [fpv]
updateCandidateProtocolVersions fpvs@(fpv : _) fpv'
  | fpvProtocolVersion fpv < fpvProtocolVersion fpv' = fpv' : fpvs
  | otherwise = fpvs
