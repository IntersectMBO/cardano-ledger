{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Cardano.Chain.Update.Validation.Endorsement
  ()
where

import Cardano.Prelude hiding (State)

import qualified Data.Map as M
import qualified Data.Set as Set

import Cardano.Chain.Common
import Cardano.Chain.Slotting
import Cardano.Chain.Update.ProtocolParameters
import Cardano.Chain.Update.ProtocolVersion
import Cardano.Chain.Update.Vote
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import Cardano.Crypto


data Environment = Environment
  { k                         :: BlockCount
  , currentSlot               :: FlatSlotId
  , adoptedProtocolParameters :: ProtocolParameters
  , confirmedProposals        :: Map UpId FlatSlotId
  , registeredUpdateProposals :: Registration.ProtocolUpdateProposals
  , numGenesisKeys            :: Word8
  }

data State = State
  { futureProtocolVersions :: [FutureProtocolVersion]
  , registeredEndorsements :: Set Endorsement
  }

data FutureProtocolVersion = FutureProtocolVersion
  { fpvSlot               :: FlatSlotId
  , fpvProtocolVersion    :: ProtocolVersion
  , fpvProtocolParameters :: ProtocolParameters
  }

data Endorsement = Endorsement
  { endorsementProtocolVersion :: ProtocolVersion
  , endorsementPublicKey       :: PublicKey
  } deriving (Eq, Ord)

data Error = MultipleProposalsForProtocolVersion ProtocolVersion


registerEndorsement
  :: MonadError Error m => Environment -> State -> Endorsement -> m State
registerEndorsement env state endorsement =
  case M.toList (M.filter ((== pv) . fst) registeredUpdateProposals) of
    -- We ignore endorsement of proposals that aren't registered
    [] -> pure state

    -- Try to register the endorsement and check if we can adopt the proposal
    [(upId, (_, pps'))] -> if isConfirmedAndStable upId
      then if canAdopt numGenesisKeys pps registeredEndorsements' pv

        -- Register the endorsement and adopt the proposal in the next epoch
        then do
          let
            fpv = FutureProtocolVersion
              { fpvSlot = currentSlot
              , fpvProtocolVersion = pv
              , fpvProtocolParameters = pps'
              }
            fpvs' = updateFutureProtocolVersions futureProtocolVersions fpv
          pure $ State
            { futureProtocolVersions = fpvs'
            , registeredEndorsements = registeredEndorsements'
            }

        -- Just register the endorsement if we cannot adopt
        else pure $ state { registeredEndorsements = registeredEndorsements' }

      -- Ignore the endorsement if the registration isn't stable
      else pure state

    -- Throw an error if there are multiple proposals for this protocol version
    _ -> throwError $ MultipleProposalsForProtocolVersion pv
 where
  Environment
    { k
    , currentSlot
    , confirmedProposals
    , registeredUpdateProposals
    , numGenesisKeys } = env

  isConfirmedAndStable upId =
    upId `M.member` scps
    where
      -- Stable and confirmed proposals.
      scps = M.filter (currentSlot - _2 k <=) confirmedProposals
      _2 x = 2 * getBlockCount x

  pps = adoptedProtocolParameters env
  pv  = endorsementProtocolVersion endorsement

  State { futureProtocolVersions, registeredEndorsements } = state
  registeredEndorsements' = Set.insert endorsement registeredEndorsements

-- TODO: Change this to take into account the number of genesis keys
-- TODO: what do we exactly need to change according to the comment above? :)
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

  t = lovelacePortionToDouble ppUpdateProposalThd * fromIntegral n

  numberOfEndorsements = fromIntegral $ length $ Set.filter
    ((== protocolVersion) . endorsementProtocolVersion)
    endorsements

-- | Add a newly endorsed protocol version to the 'FutureProtocolVersion's
--
--   We only add it to the list if the 'ProtocolVersion' is strictly greater
--   than all other `FutureProtocolVersion`s
updateFutureProtocolVersions
  :: [FutureProtocolVersion] -> FutureProtocolVersion -> [FutureProtocolVersion]
updateFutureProtocolVersions [] fpv = [fpv]
updateFutureProtocolVersions fpvs@(fpv : _) fpv'
  | fpvProtocolVersion fpv < fpvProtocolVersion fpv' = fpv' : fpvs
  | otherwise = fpvs
