{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Shelley.Rewards (
  StakeShare (..),
  PoolRewardInfo (..),
  mkApparentPerformance,
  RewardType (..),
  Reward (..),
  calcStakePoolOperatorReward,
  calcStakePoolMemberReward,
  LeaderOnlyReward (..),
  leaderRewardToGeneral,
  leaderRew,
  memberRew,
  aggregateRewards,
  filterRewards,
  sumRewards,
  aggregateCompactRewards,
  sumCompactRewards,
  rewardOnePoolMember,
  mkPoolRewardInfo,
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  BoundedRational (..),
  NonZero,
  ProtVer,
  UnitInterval,
  nonZeroOr,
  (%.),
  (%?),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decNoShareCBOR,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm,
  coinToRational,
  compactCoinOrError,
  rationalToCoinViaFloor,
  unCoinNonZero,
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Rewards (Reward (..), RewardType (..))
import Cardano.Ledger.Shelley.Era (
  hardforkAllegraAggregatedRewards,
  hardforkBabbageForgoRewardPrefilter,
 )
import Cardano.Ledger.State (Stake (..), StakePoolParams (..), StakePoolSnapShot (..), maxPool')
import Cardano.Ledger.Val ((<->))
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad (guard)
import Data.Foldable (fold, foldMap')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet

-- | StakeShare type
newtype StakeShare = StakeShare {unStakeShare :: Rational}
  deriving (Generic, Ord, Eq, NoThunks)
  deriving (Show) via Quiet StakeShare

instance NFData StakeShare

-- | Calculate pool reward
mkApparentPerformance ::
  UnitInterval ->
  Rational ->
  Natural ->
  Natural ->
  Rational
mkApparentPerformance d_ sigma blocksN blocksTotal
  | sigma == 0 = 0
  | unboundRational d_ < 0.8 = beta / sigma
  | otherwise = 1
  where
    beta = toInteger blocksN % toInteger (max 1 blocksTotal)

-- | Calculate pool leader reward
leaderRew ::
  Coin ->
  StakePoolParams ->
  StakeShare ->
  StakeShare ->
  Coin
leaderRew f pool = calcStakePoolOperatorReward f (sppCost pool) (sppMargin pool)
{-# DEPRECATED leaderRew "In favor of `calcStakePoolOperatorReward`" #-}

-- | See section "5.5.4.1 Pool Operator Reward" in [Design Specification for Delegation and Incentives in Cardano](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf)
calcStakePoolOperatorReward ::
  -- | Total Pool Rewards
  Coin ->
  -- | Stake Pool Cost
  Coin ->
  -- | Stake Pool Margin
  UnitInterval ->
  -- | Stake delegated to the pool by its owner(s)
  StakeShare ->
  -- | The relative stake of the pool.
  StakeShare ->
  Coin
calcStakePoolOperatorReward f cost margin (StakeShare s) (StakeShare sigma)
  | f <= cost = f
  | otherwise =
      cost <> rationalToCoinViaFloor (coinToRational (f <-> cost) * (m + (1 - m) * s / sigma))
  where
    m = unboundRational margin

-- | Calculate pool member reward
memberRew ::
  Coin ->
  StakePoolParams ->
  StakeShare ->
  StakeShare ->
  Coin
memberRew f pool = calcStakePoolMemberReward f (sppCost pool) (sppMargin pool)
{-# DEPRECATED memberRew "In favor of `calcStakePoolMemberReward`" #-}

-- | See section "5.5.4.2 Pool Member Reward" in [Design Specification for Delegation and Incentives in Cardano](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf)
calcStakePoolMemberReward ::
  -- | Total Pool Rewards
  Coin ->
  -- | Stake Pool Cost
  Coin ->
  -- | Stake Pool Margin
  UnitInterval ->
  -- | Member stake share
  StakeShare ->
  -- | The relative stake of the pool.
  StakeShare ->
  Coin
calcStakePoolMemberReward (Coin f) (Coin cost) margin (StakeShare t) (StakeShare sigma)
  | f <= cost = mempty
  | otherwise =
      rationalToCoinViaFloor $ fromIntegral (f - cost) * (1 - m) * t / sigma
  where
    m = unboundRational margin

sumRewards ::
  ProtVer ->
  Map (Credential Staking) (Set Reward) ->
  Coin
sumRewards protocolVersion rs = fold $ aggregateRewards protocolVersion rs

-- | Filter the reward payments to those that will actually be delivered. This
-- function exists since in Shelley, a stake credential earning rewards from
-- multiple sources would only receive one reward. So some of the coins are ignored,
-- because of this backward compatibility issue in early protocolVersions. Note that
-- both of the domains of the returned maps are a subset of the the domain of the input map 'rewards'
filterRewards ::
  ProtVer ->
  Map (Credential Staking) (Set Reward) ->
  ( Map (Credential Staking) (Set Reward) -- delivered
  , Map (Credential Staking) (Set Reward) -- ignored in Shelley Era
  )
filterRewards pv rewards =
  if hardforkAllegraAggregatedRewards pv
    then (rewards, Map.empty)
    else
      let mp = Map.map Set.deleteFindMin rewards
       in (Map.map (Set.singleton . fst) mp, Map.filter (not . Set.null) $ Map.map snd mp)

-- | for each (Set (Reward c)) entry in the map, sum up the coin. In the ShelleyEra
--   some of the coins are ignored (because of backward compatibility) see 'filterRewards'
--   Note that domain of the returned map is a subset of the input map 'rewards'
aggregateRewards ::
  ProtVer ->
  Map (Credential Staking) (Set Reward) ->
  Map (Credential Staking) Coin
aggregateRewards pv rewards =
  Map.map (foldMap' rewardAmount) $ fst $ filterRewards pv rewards

-- ================================================
-- Compact Coin versions of sumRewards and aggregateCompactRewards

sumCompactRewards ::
  ProtVer ->
  Map (Credential Staking) (Set Reward) ->
  CompactForm Coin
sumCompactRewards protocolVersion rs = fold $ aggregateCompactRewards protocolVersion rs

-- | for each (Set (Reward c)) entry in the map, sum up the coin. In the ShelleyEra
--   some of the coins are ignored (because of backward compatibility) see 'filterRewards'
--   Note that the domain of the output map is a subset of the domain of the input rewards.
aggregateCompactRewards ::
  ProtVer ->
  Map (Credential Staking) (Set Reward) ->
  Map (Credential Staking) (CompactForm Coin)
aggregateCompactRewards pv rewards =
  Map.map (foldMap' (compactCoinOrError . rewardAmount)) $ fst $ filterRewards pv rewards

-- We argue that the call to 'compactCoinOrError' will never return error.
-- The Reward is stored in the LedgerState, and we know the sum of all Ada in the LedgerState cannot
-- exceed (maxBound :: Word64), So if the sum cannot exceed it, neither can any component of the sum.
-- We need a (CompactForm Coin) because the reward map is stored in the UMap, which stores rewards
-- as (CompactForm Coin). And aggregateRewards is used to update that part of the UMap.
-- See  Cardano.Ledger.Shelley.LedgerState.IncrementalStake(applyRUpdFiltered)

-- =====================================================

data LeaderOnlyReward = LeaderOnlyReward
  { lRewardPool :: !(KeyHash StakePool)
  , lRewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks LeaderOnlyReward

instance NFData LeaderOnlyReward

instance EncCBOR LeaderOnlyReward where
  encCBOR (LeaderOnlyReward pool c) = encode $ Rec LeaderOnlyReward !> To pool !> To c

instance DecCBOR LeaderOnlyReward where
  decCBOR = decode $ RecD LeaderOnlyReward <! From <! From

leaderRewardToGeneral :: LeaderOnlyReward -> Reward
leaderRewardToGeneral (LeaderOnlyReward poolId r) = Reward LeaderReward poolId r

-- | Stake Pool specific information needed to compute the rewards
-- for its members.
data PoolRewardInfo = PoolRewardInfo
  { poolRelativeStake :: !StakeShare
  -- ^ The stake pool's stake divided by the total stake
  , poolPot :: !Coin
  -- ^ The maximum rewards available for the entire pool
  , poolPs :: !StakePoolSnapShot
  -- ^ The snapshot from stake pool state
  , poolBlocks :: !Natural
  -- ^ The number of blocks the stake pool produced
  , poolLeaderReward :: !LeaderOnlyReward
  -- ^ The leader reward
  }
  deriving (Show, Eq, Generic)

instance NoThunks PoolRewardInfo

instance NFData PoolRewardInfo

instance EncCBOR PoolRewardInfo where
  encCBOR
    (PoolRewardInfo a b c d e) =
      encode $
        Rec PoolRewardInfo
          !> E (encCBOR . unStakeShare) a
          !> To b
          !> To c
          !> To d
          !> To e

instance DecCBOR PoolRewardInfo where
  decCBOR =
    decode
      ( RecD PoolRewardInfo
          <! D (StakeShare <$> decCBOR)
          <! From
          <! D decNoShareCBOR
          <! From
          <! From
      )

notPoolOwner ::
  Set (KeyHash Staking) ->
  Credential Staking ->
  Bool
notPoolOwner owners = \case
  KeyHashObj hk -> hk `Set.notMember` owners
  ScriptHashObj _ -> True

-- | The stake pool member reward calculation
rewardOnePoolMember ::
  -- | The protocol version
  ProtVer ->
  -- | The total amount of stake in the system
  Coin ->
  -- | The set of registered stake credentials
  Set (Credential Staking) ->
  -- | Stake pool specific intermediate values needed
  -- to compute member rewards.
  PoolRewardInfo ->
  -- | The stake credential whose reward is being calculated.
  Credential Staking ->
  -- | The stake controlled by the stake credential
  -- in the previous parameter above.
  Coin ->
  -- | The reward for the given stake credential.
  -- This could be Nothing if the credential is no longer registered,
  -- if it is an owner, or if the reward is zero.
  Maybe Coin
rewardOnePoolMember pv totalStake addrsRew rewardInfo hk (Coin c) =
  if prefilter && notPoolOwner (spssSelfDelegatedOwners (poolPs rewardInfo)) hk && r /= Coin 0
    then Just r
    else Nothing
  where
    prefilter = hardforkBabbageForgoRewardPrefilter pv || hk `Set.member` addrsRew
    StakePoolSnapShot {spssCost, spssMargin} = poolPs rewardInfo
    sigma = poolRelativeStake rewardInfo
    poolR = poolPot rewardInfo
    -- warning: totalStake could be zero!
    stakeShare = StakeShare $ c % unCoin totalStake
    r = calcStakePoolMemberReward poolR spssCost spssMargin stakeShare sigma

-- | Calculate single stake pool specific values for the reward computation.
--
-- Note that if a stake pool has made no blocks in the given epoch, it will
-- get no rewards, and so we do not need to return 'PoolRewardInfo'. We do,
-- however, need to return the relative stake of the pool in order to
-- compute data for the stake pool ranking. Eventually we will remove
-- the ranking information out of the ledger code and into a separate service,
-- and at that point we can simplify this function to not care about ranking.
mkPoolRewardInfo ::
  EraPParams era =>
  PParams era ->
  Coin ->
  BlocksMade ->
  Natural ->
  Stake ->
  VMap.VMap VMap.VB VMap.VB (Credential Staking) (KeyHash StakePool) ->
  Coin ->
  NonZero Coin ->
  KeyHash StakePool ->
  StakePoolSnapShot ->
  Either StakeShare PoolRewardInfo
mkPoolRewardInfo
  pp
  r
  blocks
  blocksTotal
  stake
  delegs
  (Coin totalStake)
  totalActiveStake
  stakePoolId
  stakePoolSnapShot =
    case Map.lookup stakePoolId (unBlocksMade blocks) of
      -- This pool made no blocks this epoch. For the purposes of stake pool
      -- ranking only, we return the relative stake of this pool so that we
      -- can judge how likely it was that this pool made no blocks.
      Nothing -> Left $! StakeShare sigma
      -- This pool made some blocks, so we can proceed to calculate the
      -- intermediate values needed for the individual reward calculations.
      Just numBlocksMade ->
        let Coin maxP =
              if pledge <= selfDelegatedOwnersStake
                then maxPool' pp_a0 pp_nOpt r sigma poolRelativePledge
                else mempty
            appPerf = mkApparentPerformance pp_d sigmaA numBlocksMade blocksTotal
            poolR = rationalToCoinViaFloor (appPerf * fromIntegral maxP)
            stakePoolOperatorReward =
              calcStakePoolOperatorReward
                poolR
                (spssCost stakePoolSnapShot)
                (spssMargin stakePoolSnapShot)
                (StakeShare poolOwnerRelativeStake)
                (StakeShare sigma)
            rewardInfo =
              PoolRewardInfo
                { poolRelativeStake = StakeShare sigma
                , poolPot = poolR
                , poolPs = stakePoolSnapShot
                , poolBlocks = numBlocksMade
                , poolLeaderReward = LeaderOnlyReward stakePoolId stakePoolOperatorReward
                }
         in Right $! rewardInfo
    where
      pp_d = pp ^. ppDG
      pp_a0 = pp ^. ppA0L
      pp_nOpt = (pp ^. ppNOptL) `nonZeroOr` error "nOpt is zero"
      Coin poolTotalStake = fromCompact (spssStake stakePoolSnapShot)
      accOwnerStake c o = maybe c (c <>) $ do
        hk <- VMap.lookup (KeyHashObj o) delegs
        guard (hk == stakePoolId)
        VMap.lookup (KeyHashObj o) (unStake stake)
      Coin selfDelegatedOwnersStake = spssSelfDelegatedOwnersStake stakePoolSnapShot
      Coin pledge = spssPledge stakePoolSnapShot
      -- warning: In theory `totalStake` and `totalActiveStake` could be zero, but that would imply no
      -- active stake pools and no delegators, which would mean PoS would be dead!
      poolRelativePledge = pledge % totalStake
      poolOwnerRelativeStake = selfDelegatedOwnersStake %? totalStake
      sigma = poolTotalStake %? totalStake
      sigmaA = poolTotalStake %. unCoinNonZero totalActiveStake
