{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Api.State.Query.Reward (
  -- * Stable query result types
  QueryResultRewardInfoPools (..),

  -- * Queries
  queryNonMyopicMemberRewards,
  queryRewardInfoPools,
  queryRewardProvenance,
) where

import Cardano.Ledger.Api.State.Query.Snapshot (queryCurrentSnapshot)
import Cardano.Ledger.BaseTypes (Globals (..), epochInfoPure, unNonZero, (%?))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (ppA0L, ppNOptL)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.API.Wallet (
  RewardInfoPool (..),
  RewardParams (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  NewEpochState (..),
  RewardUpdate,
  circulation,
  createRUpd,
  curPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.PoolRank (
  NonMyopic (..),
  PerformanceEstimate (..),
  calcNonMyopicMemberReward,
  getTopRankedPools,
  percentile',
 )
import Cardano.Ledger.Shelley.RewardProvenance (RewardProvenance)
import Cardano.Ledger.Shelley.Rewards (StakeShare (..))
import Cardano.Ledger.Slot (epochInfoSize)
import Cardano.Ledger.State (
  EraCertState,
  EraGov,
  EraStake,
  spssCost,
  spssMargin,
  spssPledge,
  spssSelfDelegatedOwnersStake,
  spssStake,
  ssStakePoolsSnapShot,
 )
import qualified Cardano.Ledger.State as LS
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (runReader)
import Data.Aeson (ToJSON)
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

-- | Stable query result for per-pool reward information.
data QueryResultRewardInfoPools = QueryResultRewardInfoPools
  { qrripParams :: !RewardParams
  , qrripPools :: !(Map (KeyHash StakePool) RewardInfoPool)
  }
  deriving (Show, Eq, Ord, Generic)

deriving instance ToJSON QueryResultRewardInfoPools

instance NFData QueryResultRewardInfoPools

instance NoThunks QueryResultRewardInfoPools

instance EncCBOR QueryResultRewardInfoPools where
  encCBOR (QueryResultRewardInfoPools params pools) =
    encode $
      Rec QueryResultRewardInfoPools
        !> To params
        !> To pools

instance DecCBOR QueryResultRewardInfoPools where
  decCBOR =
    decode $
      RecD QueryResultRewardInfoPools
        <! From
        <! From

-- | Query non-myopic pool member rewards for a set of credentials.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:415
--   answerPureBlockQuery case for GetNonMyopicMemberRewards
--
-- For each given credential (or hypothetical stake amount), returns a map from
-- each stake pool to the estimated non-myopic member reward for that pool.
-- Wallets use this to answer "how much would I earn if I delegated to pool X?".
--
-- "Non-myopic" means rewards are estimated under the assumption that stake
-- will eventually converge to a Nash equilibrium across pools, rather than
-- using today's distribution naively. This avoids overestimating rewards for
-- currently under-saturated pools.
--
-- The input set contains @'Either' 'Coin' ('Credential' 'Staking')@ values:
--
-- * @'Left' coin@ — a hypothetical stake amount (for "what if I delegated
--   this much?" queries).
-- * @'Right' cred@ — an existing credential whose current active stake is
--   looked up from the ledger state.
--
-- This uses a fresh snapshot derived from the current ledger state, not the
-- stored mark\/set\/go snapshots.
--
-- @
--   O(c + k * p)
-- @
-- where,
--   (c) is the staking credentials, queryCurrentSnapshot
--   (k) is the input credential\/coin set, iterate over
--   (p) is the registered stake pools, VMap.mapWithKey per credential
queryNonMyopicMemberRewards ::
  (EraGov era, EraStake era, EraCertState era) =>
  -- | maxLovelaceSupply from genesis config
  Word64 ->
  NewEpochState era ->
  Set (Either Coin (Credential Staking)) ->
  Map
    (Either Coin (Credential Staking))
    (Map (KeyHash StakePool) Coin)
queryNonMyopicMemberRewards maxLovelaceSupply ss = Map.fromSet nmmRewards
  where
    maxSupply = Coin . fromIntegral $ maxLovelaceSupply
    totalStakeCoin@(Coin totalStake) = circulation es maxSupply
    toShare (Coin x) = StakeShare $ x %? totalStake
    memShare (Right cred) =
      toShare $
        maybe mempty (fromCompact . unNonZero . LS.swdStake) $
          VMap.lookup cred (LS.unActiveStake activeStake)
    memShare (Left coin) = toShare coin
    es = nesEs ss
    pp = es ^. curPParamsEpochStateL
    NonMyopic {likelihoodsNM = ls, rewardPotNM = rPot} = esNonMyopic es
    LS.SnapShot {LS.ssActiveStake = activeStake, LS.ssStakePoolsSnapShot = stakePoolsSnapShot} = queryCurrentSnapshot ss
    calcNMMRewards t poolId spss
      | spssPledge <= spssSelfDelegatedOwnersStake =
          calcNonMyopicMemberReward pp rPot poolId spssCost spssMargin s sigma t topPools hitRateEst
      | otherwise = mempty
      where
        LS.StakePoolSnapShot {spssSelfDelegatedOwnersStake, spssPledge, spssCost, spssMargin} = spss
        s = toShare spssPledge
        hitRateEst = percentile' (histLookup poolId)
        sigma = toShare (fromCompact (LS.spssStake spss))

    nmmRewards cred = VMap.toMap $ VMap.mapWithKey (calcNMMRewards $ memShare cred) stakePoolsSnapShot
    histLookup k = VMap.findWithDefault mempty k ls
    topPools =
      getTopRankedPools rPot totalStakeCoin pp $
        Map.intersectionWith (,) (VMap.toMap (VMap.map percentile' ls)) $
          VMap.toMap stakePoolsSnapShot

-- | Query per-pool reward information from the current stake distribution.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:447
--   answerPureBlockQuery case for GetRewardInfoPools
--
-- This is the primary query for wallets and tools that display pool ranking,
-- saturation levels, and estimated returns. It returns per-pool data (stake,
-- pledge, costs, margins, performance estimates) together with global reward
-- parameters (a0, nOpt, totalStake, rPot).
--
-- Uses a fresh snapshot from 'queryCurrentSnapshot' rather than the stored
-- epoch-boundary snapshots, so the data reflects the most recent ledger state.
--
-- @
--   O(c + p)
-- @
-- where,
--   (c) is the staking credentials, queryCurrentSnapshot
--   (p) is the registered stake pools, VMap.mapWithKey
queryRewardInfoPools ::
  (EraGov era, EraStake era, EraCertState era) =>
  -- | maxLovelaceSupply from genesis config
  Word64 ->
  NewEpochState era ->
  QueryResultRewardInfoPools
queryRewardInfoPools maxLovelaceSupply nes =
  QueryResultRewardInfoPools rewardParams poolInfos
  where
    es = nesEs nes
    pp = es ^. curPParamsEpochStateL
    NonMyopic {likelihoodsNM = ls, rewardPotNM = rPot} = esNonMyopic es
    histLookup poolId = VMap.findWithDefault mempty poolId ls

    LS.SnapShot {ssStakePoolsSnapShot} = queryCurrentSnapshot nes

    rewardParams =
      RewardParams
        { a0 = pp ^. ppA0L
        , nOpt = pp ^. ppNOptL
        , totalStake =
            let supply = Coin (fromIntegral maxLovelaceSupply)
             in circulation es supply
        , rPot = rPot
        }
    poolInfos = VMap.toMap $ VMap.mapWithKey mkRewardInfoPool ssStakePoolsSnapShot
    mkRewardInfoPool poolId LS.StakePoolSnapShot {spssStake, spssSelfDelegatedOwnersStake, spssPledge, spssMargin, spssCost} =
      RewardInfoPool
        { stake = fromCompact spssStake
        , ownerStake = spssSelfDelegatedOwnersStake
        , ownerPledge = spssPledge
        , margin = spssMargin
        , cost = spssCost
        , performanceEstimate =
            unPerformanceEstimate $ percentile' $ histLookup poolId
        }

-- | Query reward provenance by computing a reward update (internally based on the go snapshot).
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:441
--   answerPureBlockQuery case for GetRewardProvenance
--
-- __Note:__ The 'RewardProvenance' component of the result is always 'def'
-- (empty\/default). Only the 'RewardUpdate' is meaningful. This matches the
-- consensus implementation, which also discards provenance.
--
-- This function requires the full 'Globals' because it internally runs
-- 'createRUpd' in a 'Reader' monad that needs access to 'epochInfo',
-- 'activeSlotCoeff', 'securityParameter', and 'maxLovelaceSupply'.
--
-- @
--   O(c * log(p))
-- @
-- where,
--   (c) is the staking credentials, createRUpd reward computation
--   (p) is the registered stake pools, reward calculation per credential
queryRewardProvenance ::
  (EraGov era, EraCertState era) =>
  Globals ->
  NewEpochState era ->
  (RewardUpdate, RewardProvenance)
queryRewardProvenance globals newEpochState =
  ( runReader
      (createRUpd slotsPerEpoch blocksMade epochState maxSupply asc secparam)
      globals
  , def
  )
  where
    epochState = nesEs newEpochState
    maxSupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksMade = nesBprev newEpochState
    epochNo = nesEL newEpochState
    slotsPerEpoch = epochInfoSize (epochInfoPure globals) epochNo
    asc = activeSlotCoeff globals
    secparam = securityParameter globals
{-# DEPRECATED
  queryRewardProvenance
  "Wallets should prefer 'queryRewardInfoPools' for up-to-date reward information based on the current stake distribution."
  #-}
