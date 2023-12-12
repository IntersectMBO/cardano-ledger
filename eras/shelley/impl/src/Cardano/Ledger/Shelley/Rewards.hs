{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Shelley.Rewards (
  StakeShare (..),
  PoolRewardInfo (..),
  mkApparentPerformance,
  RewardType (..),
  Reward (..),
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
)
where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  BoundedRational (..),
  ProtVer,
  UnitInterval,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm,
  coinToRational,
  rationalToCoinViaFloor,
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.EpochBoundary (Stake (..), maxPool')
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.UMap (compactCoinOrError)
import Cardano.Ledger.Val ((<->))
import Control.DeepSeq (NFData)
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
    beta = fromIntegral blocksN / fromIntegral (max 1 blocksTotal)

-- | Calculate pool leader reward
leaderRew ::
  Coin ->
  PoolParams c ->
  StakeShare ->
  StakeShare ->
  Coin
leaderRew f pool (StakeShare s) (StakeShare sigma)
  | f <= c = f
  | otherwise =
      c
        <> rationalToCoinViaFloor
          (coinToRational (f <-> c) * (m' + (1 - m') * s / sigma))
  where
    c = ppCost pool
    m = ppMargin pool
    m' = unboundRational m

-- | Calculate pool member reward
memberRew ::
  Coin ->
  PoolParams c ->
  StakeShare ->
  StakeShare ->
  Coin
memberRew (Coin f') pool (StakeShare t) (StakeShare sigma)
  | f' <= c = mempty
  | otherwise =
      rationalToCoinViaFloor $
        fromIntegral (f' - c) * (1 - m') * t / sigma
  where
    Coin c = ppCost pool
    m = ppMargin pool
    m' = unboundRational m

sumRewards ::
  forall c.
  ProtVer ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  Coin
sumRewards protocolVersion rs = fold $ aggregateRewards protocolVersion rs

-- | Filter the reward payments to those that will actually be delivered. This
-- function exists since in Shelley, a stake credential earning rewards from
-- multiple sources would only receive one reward. So some of the coins are ignored,
-- because of this backward compatibility issue in early protocolVersions. Note that
-- both of the domains of the returned maps are a subset of the the domain of the input map 'rewards'
filterRewards ::
  forall c.
  ProtVer ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  ( Map (Credential 'Staking c) (Set (Reward c)) -- delivered
  , Map (Credential 'Staking c) (Set (Reward c)) -- ignored in Shelley Era
  )
filterRewards pv rewards =
  if HardForks.aggregatedRewards pv
    then (rewards, Map.empty)
    else
      let mp = Map.map Set.deleteFindMin rewards
       in (Map.map (Set.singleton . fst) mp, Map.filter (not . Set.null) $ Map.map snd mp)

-- | for each (Set (Reward c)) entry in the map, sum up the coin. In the ShelleyEra
--   some of the coins are ignored (because of backward compatibility) see 'filterRewards'
--   Note that domain of the returned map is a subset of the input map 'rewards'
aggregateRewards ::
  forall c.
  ProtVer ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  Map (Credential 'Staking c) Coin
aggregateRewards pv rewards =
  Map.map (foldMap' rewardAmount) $ fst $ filterRewards pv rewards

-- ================================================
-- Compact Coin versions of sumRewards and aggregateCompactRewards

sumCompactRewards ::
  ProtVer ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  CompactForm Coin
sumCompactRewards protocolVersion rs = fold $ aggregateCompactRewards protocolVersion rs

-- | for each (Set (Reward c)) entry in the map, sum up the coin. In the ShelleyEra
--   some of the coins are ignored (because of backward compatibility) see 'filterRewards'
--   Note that the domain of the output map is a subset of the domain of the input rewards.
aggregateCompactRewards ::
  ProtVer ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  Map (Credential 'Staking c) (CompactForm Coin)
aggregateCompactRewards pv rewards =
  Map.map (foldMap' (compactCoinOrError . rewardAmount)) $ fst $ filterRewards pv rewards

-- We argue that the call to 'compactCoinOrError' will never return error.
-- The Reward is stored in the LedgerState, and we know the sum of all Ada in the LedgerState cannot
-- exceed (maxBound :: Word64), So if the sum cannot exceed it, neither can any component of the sum.
-- We need a (CompactForm Coin) because the reward map is stored in the UMap, which stores rewards
-- as (CompactForm Coin). And aggregateRewards is used to update that part of the UMap.
-- See  Cardano.Ledger.Shelley.LedgerState.IncrementalStake(applyRUpdFiltered)

-- =====================================================

data LeaderOnlyReward c = LeaderOnlyReward
  { lRewardPool :: !(KeyHash 'StakePool c)
  , lRewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks (LeaderOnlyReward c)

instance NFData (LeaderOnlyReward c)

instance CC.Crypto c => EncCBOR (LeaderOnlyReward c) where
  encCBOR (LeaderOnlyReward pool c) = encode $ Rec LeaderOnlyReward !> To pool !> To c

instance CC.Crypto c => DecCBOR (LeaderOnlyReward c) where
  decCBOR = decode $ RecD LeaderOnlyReward <! From <! From

leaderRewardToGeneral :: LeaderOnlyReward c -> Reward c
leaderRewardToGeneral (LeaderOnlyReward poolId r) = Reward LeaderReward poolId r

-- | Stake Pool specific information needed to compute the rewards
-- for its members.
data PoolRewardInfo c = PoolRewardInfo
  { poolRelativeStake :: !StakeShare
  -- ^ The stake pool's stake divided by the total stake
  , poolPot :: !Coin
  -- ^ The maximum rewards available for the entire pool
  , poolPs :: !(PoolParams c)
  -- ^ The stake pool parameters
  , poolBlocks :: !Natural
  -- ^ The number of blocks the stake pool produced
  , poolLeaderReward :: !(LeaderOnlyReward c)
  -- ^ The leader reward
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (PoolRewardInfo c)

instance NFData (PoolRewardInfo c)

instance CC.Crypto c => EncCBOR (PoolRewardInfo c) where
  encCBOR
    (PoolRewardInfo a b c d e) =
      encode $
        Rec PoolRewardInfo
          !> E (encCBOR . unStakeShare) a
          !> To b
          !> To c
          !> To d
          !> To e

instance CC.Crypto c => DecCBOR (PoolRewardInfo c) where
  decCBOR =
    decode
      ( RecD PoolRewardInfo
          <! D (StakeShare <$> decCBOR)
          <! From
          <! From
          <! From
          <! From
      )

notPoolOwner ::
  PoolParams c ->
  Credential 'Staking c ->
  Bool
notPoolOwner pps = \case
  KeyHashObj hk -> hk `Set.notMember` ppOwners pps
  ScriptHashObj _ -> True

-- | The stake pool member reward calculation
rewardOnePoolMember ::
  -- | The protocol version
  ProtVer ->
  -- | The total amount of stake in the system
  Coin ->
  -- | The set of registered stake credentials
  Set (Credential 'Staking c) ->
  -- | Stake pool specific intermediate values needed
  -- to compute member rewards.
  PoolRewardInfo c ->
  -- | The stake credential whose reward is being calculated.
  Credential 'Staking c ->
  -- | The stake controlled by the stake credential
  -- in the previous parameter above.
  Coin ->
  -- | The reward for the given stake credential.
  -- This could be Nothing if the credential is no longer registered,
  -- if it is an owner, or if the reward is zero.
  Maybe Coin
rewardOnePoolMember
  pp
  (Coin totalStake)
  addrsRew
  rewardInfo
  hk
  (Coin c) =
    if prefilter && notPoolOwner (poolPs rewardInfo) hk && r /= Coin 0
      then Just r
      else Nothing
    where
      prefilter = HardForks.forgoRewardPrefilter pp || hk `Set.member` addrsRew
      pool = poolPs rewardInfo
      sigma = poolRelativeStake rewardInfo
      poolR = poolPot rewardInfo
      r = memberRew poolR pool (StakeShare (c % totalStake)) sigma

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
  BlocksMade (EraCrypto era) ->
  Natural ->
  Stake (EraCrypto era) ->
  VMap.VMap VMap.VB VMap.VB (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
  Map (KeyHash 'StakePool (EraCrypto era)) Coin ->
  Coin ->
  Coin ->
  PoolParams (EraCrypto era) ->
  Either StakeShare (PoolRewardInfo (EraCrypto era))
mkPoolRewardInfo
  pp
  r
  blocks
  blocksTotal
  stake
  delegs
  stakePerPool
  (Coin totalStake)
  (Coin activeStake)
  pool = case Map.lookup (ppId pool) (unBlocksMade blocks) of
    -- This pool made no blocks this epoch. For the purposes of stake pool
    -- ranking only, we return the relative stake of this pool so that we
    -- can judge how likely it was that this pool made no blocks.
    Nothing -> Left $! StakeShare sigma
    -- This pool made no blocks, so we can proceed to calculate the
    -- intermediate values needed for the individual reward calculations.
    Just blocksN ->
      let Coin pledge = ppPledge pool
          pledgeRelative = pledge % totalStake
          sigmaA = if activeStake == 0 then 0 else pstakeTot % activeStake
          Coin maxP =
            if pledge <= ostake
              then maxPool' pp_a0 pp_nOpt r sigma pledgeRelative
              else mempty
          appPerf = mkApparentPerformance pp_d sigmaA blocksN blocksTotal
          poolR = rationalToCoinViaFloor (appPerf * fromIntegral maxP)
          lreward =
            leaderRew
              poolR
              pool
              (StakeShare $ if totalStake == 0 then 0 else ostake % totalStake)
              (StakeShare sigma)
          rewardInfo =
            PoolRewardInfo
              { poolRelativeStake = StakeShare sigma
              , poolPot = poolR
              , poolPs = pool
              , poolBlocks = blocksN
              , poolLeaderReward = LeaderOnlyReward (ppId pool) lreward
              }
       in Right $! rewardInfo
    where
      pp_d = pp ^. ppDG
      pp_a0 = pp ^. ppA0L
      pp_nOpt = pp ^. ppNOptL
      Coin pstakeTot = Map.findWithDefault mempty (ppId pool) stakePerPool
      accOwnerStake c o = maybe c (c <>) $ do
        hk <- VMap.lookup (KeyHashObj o) delegs
        guard (hk == ppId pool)
        fromCompact <$> VMap.lookup (KeyHashObj o) (unStake stake)
      Coin ostake = Set.foldl' accOwnerStake mempty (ppOwners pool)
      sigma = if totalStake == 0 then 0 else fromIntegral pstakeTot % fromIntegral totalStake
