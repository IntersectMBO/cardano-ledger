{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.Rewards
  ( StakeShare (..),
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
    rewardOnePoolMember,
    mkPoolRewardInfo,
  )
where

import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    BoundedRational (..),
    NonNegativeInterval,
    ProtVer,
    UnitInterval,
  )
import Cardano.Ledger.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin
  ( Coin (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (EraCrypto, PParams, Reward (..), RewardType (..))
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.EpochBoundary (Stake (..), maxPool')
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Delegation.PoolParams (poolSpec)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
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
import GHC.Records (HasField (getField))
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
    (c, m, _) = poolSpec pool
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
    (Coin c, m, _) = poolSpec pool
    m' = unboundRational m

sumRewards ::
  forall c pp.
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  Coin
sumRewards protocolVersion rs = fold $ aggregateRewards protocolVersion rs

-- | Filter the reward payments to those that will actually be delivered. This
-- function exists since in Shelley, a stake credential earning rewards from
-- multiple sources would only receive one reward.
filterRewards ::
  forall c pp.
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  ( Map (Credential 'Staking c) (Set (Reward c)), -- delivered
    Map (Credential 'Staking c) (Set (Reward c)) -- ignored in Shelley Era
  )
filterRewards pp rewards =
  if HardForks.aggregatedRewards pp
    then (rewards, Map.empty)
    else
      let mp = Map.map Set.deleteFindMin rewards
       in (Map.map (Set.singleton . fst) mp, Map.filter (not . Set.null) $ Map.map snd mp)

-- | for each (Set (Reward c)) entry in the map, sum up the coin. In the ShelleyEra
--   some of the coins are ignored (because of backward compatibility)
aggregateRewards ::
  forall c pp.
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Map (Credential 'Staking c) (Set (Reward c)) ->
  Map (Credential 'Staking c) Coin
aggregateRewards pp rewards =
  Map.map (foldMap' rewardAmount) $ fst $ filterRewards pp rewards

data LeaderOnlyReward c = LeaderOnlyReward
  { lRewardPool :: !(KeyHash 'StakePool c),
    lRewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks (LeaderOnlyReward c)

instance NFData (LeaderOnlyReward c)

instance CC.Crypto c => ToCBOR (LeaderOnlyReward c) where
  toCBOR (LeaderOnlyReward pool c) = encode $ Rec LeaderOnlyReward !> To pool !> To c

instance CC.Crypto c => FromCBOR (LeaderOnlyReward c) where
  fromCBOR = decode $ RecD LeaderOnlyReward <! From <! From

leaderRewardToGeneral :: LeaderOnlyReward c -> Reward c
leaderRewardToGeneral (LeaderOnlyReward poolId r) = Reward LeaderReward poolId r

-- | Stake Pool specific information needed to compute the rewards
-- for its members.
data PoolRewardInfo c = PoolRewardInfo
  { -- | The stake pool's stake divided by the total stake
    poolRelativeStake :: !StakeShare,
    -- | The maximum rewards available for the entire pool
    poolPot :: !Coin,
    -- | The stake pool parameters
    poolPs :: !(PoolParams c),
    -- | The number of blocks the stake pool produced
    poolBlocks :: !Natural,
    -- | The leader reward
    poolLeaderReward :: !(LeaderOnlyReward c)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (PoolRewardInfo c)

instance NFData (PoolRewardInfo c)

instance CC.Crypto c => ToCBOR (PoolRewardInfo c) where
  toCBOR
    (PoolRewardInfo a b c d e) =
      encode $
        Rec PoolRewardInfo
          !> E (toCBOR . unStakeShare) a
          !> To b
          !> To c
          !> To d
          !> To e

instance CC.Crypto c => FromCBOR (PoolRewardInfo c) where
  fromCBOR =
    decode
      ( RecD PoolRewardInfo
          <! D (StakeShare <$> fromCBOR)
          <! From
          <! From
          <! From
          <! From
      )

notPoolOwner ::
  HasField "_protocolVersion" pp ProtVer =>
  pp ->
  PoolParams c ->
  Credential 'Staking c ->
  Bool
notPoolOwner pp pps = \case
  KeyHashObj hk -> hk `Set.notMember` ppOwners pps
  ScriptHashObj _ -> HardForks.allowScriptStakeCredsToEarnRewards pp

-- | The stake pool member reward calculation
rewardOnePoolMember ::
  HasField "_protocolVersion" pp ProtVer =>
  -- | The protocol parameters
  pp ->
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
    if prefilter && notPoolOwner pp (poolPs rewardInfo) hk && r /= Coin 0
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
  ( HasField "_d" (PParams era) UnitInterval,
    HasField "_a0" (PParams era) NonNegativeInterval,
    HasField "_nOpt" (PParams era) Natural
  ) =>
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
              { poolRelativeStake = StakeShare sigma,
                poolPot = poolR,
                poolPs = pool,
                poolBlocks = blocksN,
                poolLeaderReward = LeaderOnlyReward (ppId pool) lreward
              }
       in Right $! rewardInfo
    where
      pp_d = getField @"_d" pp
      pp_a0 = getField @"_a0" pp
      pp_nOpt = getField @"_nOpt" pp
      Coin pstakeTot = Map.findWithDefault mempty (ppId pool) stakePerPool
      accOwnerStake c o = maybe c (c <>) $ do
        hk <- VMap.lookup (KeyHashObj o) delegs
        guard (hk == ppId pool)
        fromCompact <$> VMap.lookup (KeyHashObj o) (unStake stake)
      Coin ostake = Set.foldl' accOwnerStake mempty (ppOwners pool)
      sigma = if totalStake == 0 then 0 else fromIntegral pstakeTot % fromIntegral totalStake
