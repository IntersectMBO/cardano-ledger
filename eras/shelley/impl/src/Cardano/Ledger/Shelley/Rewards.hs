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
    LeaderOnlyReward (..),
    leaderRewardToGeneral,
    Reward (..),
    leaderRew,
    memberRew,
    aggregateRewards,
    filterRewards,
    sumRewards,
    rewardOnePoolMember,
    mkPoolRewardInfo,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeWord,
    encodeWord,
  )
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    BoundedRational (..),
    NonNegativeInterval,
    ProtVer,
    UnitInterval,
    invalidKey,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Cardano.Ledger.Compactible (fromCompact)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Delegation.PoolParams (poolSpec)
import Cardano.Ledger.Shelley.EpochBoundary (Stake (..), maxPool')
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.Val ((<->))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import qualified Data.Compact.VMap as VMap
import Data.Foldable (fold, foldMap')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
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
  PoolParams crypto ->
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
  PoolParams crypto ->
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

data RewardType = MemberReward | LeaderReward
  deriving (Eq, Show, Ord, Generic)

instance NoThunks RewardType

instance NFData RewardType

instance ToCBOR RewardType where
  toCBOR MemberReward = encodeWord 0
  toCBOR LeaderReward = encodeWord 1

instance FromCBOR RewardType where
  fromCBOR =
    decodeWord >>= \case
      0 -> pure MemberReward
      1 -> pure LeaderReward
      n -> invalidKey n

data Reward crypto = Reward
  { rewardType :: RewardType,
    rewardPool :: KeyHash 'StakePool crypto,
    rewardAmount :: Coin
  }
  deriving (Eq, Show, Generic)

-- | Note that this Ord instance is chosen to align precisely
--  with the Allegra reward aggregation, as given by the
--  function 'aggregateRewards' so that 'Set.findMax' returns
--  the expected value.
instance Ord (Reward crypto) where
  compare (Reward MemberReward _ _) (Reward LeaderReward _ _) = GT
  compare (Reward LeaderReward _ _) (Reward MemberReward _ _) = LT
  compare (Reward _ pool1 _) (Reward _ pool2 _) = compare pool1 pool2

instance NoThunks (Reward crypto)

instance NFData (Reward crypto)

instance CC.Crypto crypto => ToCBOR (Reward crypto) where
  toCBOR (Reward rt pool c) =
    encode $ Rec Reward !> To rt !> To pool !> To c

instance CC.Crypto crypto => FromCBOR (Reward crypto) where
  fromCBOR =
    decode $ RecD Reward <! From <! From <! From

sumRewards ::
  forall crypto pp.
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Map (Credential 'Staking crypto) (Set (Reward crypto)) ->
  Coin
sumRewards protocolVersion rs = fold $ aggregateRewards protocolVersion rs

-- | Filter the reward payments to those that will actually be delivered. This
-- function exists since in Shelley, a stake credential earning rewards from
-- multiple sources would only receive one reward.
filterRewards ::
  forall crypto pp.
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Map (Credential 'Staking crypto) (Set (Reward crypto)) ->
  Map (Credential 'Staking crypto) (Set (Reward crypto))
filterRewards pp rewards =
  if HardForks.aggregatedRewards pp
    then rewards
    else Map.map (Set.singleton . Set.findMin) rewards

aggregateRewards ::
  forall crypto pp.
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Map (Credential 'Staking crypto) (Set (Reward crypto)) ->
  Map (Credential 'Staking crypto) Coin
aggregateRewards pp rewards =
  Map.map (foldMap' rewardAmount) $ filterRewards pp rewards

data LeaderOnlyReward crypto = LeaderOnlyReward
  { lRewardPool :: !(KeyHash 'StakePool crypto),
    lRewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks (LeaderOnlyReward crypto)

instance NFData (LeaderOnlyReward crypto)

instance CC.Crypto crypto => ToCBOR (LeaderOnlyReward crypto) where
  toCBOR (LeaderOnlyReward pool c) = encode $ Rec LeaderOnlyReward !> To pool !> To c

instance CC.Crypto crypto => FromCBOR (LeaderOnlyReward crypto) where
  fromCBOR = decode $ RecD LeaderOnlyReward <! From <! From

leaderRewardToGeneral :: LeaderOnlyReward c -> Reward c
leaderRewardToGeneral (LeaderOnlyReward poolId r) = Reward LeaderReward poolId r

-- | Stake Pool specific information needed to compute the rewards
-- for its members.
data PoolRewardInfo crypto = PoolRewardInfo
  { -- | The stake pool's stake divided by the total stake
    poolRelativeStake :: !StakeShare,
    -- | The maximum rewards available for the entire pool
    poolPot :: !Coin,
    -- | The stake pool parameters
    poolPs :: !(PoolParams crypto),
    -- | The number of blocks the stake pool produced
    poolBlocks :: !Natural,
    -- | The leader reward
    poolLeaderReward :: !(LeaderOnlyReward crypto)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (PoolRewardInfo crypto)

instance NFData (PoolRewardInfo crypto)

instance CC.Crypto crypto => ToCBOR (PoolRewardInfo crypto) where
  toCBOR
    (PoolRewardInfo a b c d e) =
      encode $
        Rec PoolRewardInfo
          !> E (toCBOR . unStakeShare) a
          !> To b
          !> To c
          !> To d
          !> To e

instance CC.Crypto crypto => FromCBOR (PoolRewardInfo crypto) where
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
  PoolParams crypto ->
  Credential 'Staking crypto ->
  Bool
notPoolOwner pp pps = \case
  KeyHashObj hk -> hk `Set.notMember` _poolOwners pps
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
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_nOpt" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Coin ->
  BlocksMade (Crypto era) ->
  Natural ->
  Stake (Crypto era) ->
  VMap.VMap VMap.VB VMap.VB (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era)) ->
  Map (KeyHash 'StakePool (Crypto era)) Coin ->
  Coin ->
  Coin ->
  PoolParams (Crypto era) ->
  Either StakeShare (PoolRewardInfo (Crypto era))
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
  pool = case Map.lookup (_poolId pool) (unBlocksMade blocks) of
    -- This pool made no blocks this epoch. For the purposes of stake pool
    -- ranking only, we return the relative stake of this pool so that we
    -- can judge how likely it was that this pool made no blocks.
    Nothing -> Left $! StakeShare sigma
    -- This pool made no blocks, so we can proceed to calculate the
    -- intermediate values needed for the individual reward calculations.
    Just blocksN ->
      let Coin pledge = _poolPledge pool
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
                poolLeaderReward = LeaderOnlyReward (_poolId pool) lreward
              }
       in Right $! rewardInfo
    where
      pp_d = getField @"_d" pp
      pp_a0 = getField @"_a0" pp
      pp_nOpt = getField @"_nOpt" pp
      Coin pstakeTot = Map.findWithDefault mempty (_poolId pool) stakePerPool
      accOwnerStake c o = maybe c (c <>) $ do
        hk <- VMap.lookup (KeyHashObj o) delegs
        guard (hk == _poolId pool)
        fromCompact <$> VMap.lookup (KeyHashObj o) (unStake stake)
      Coin ostake = Set.foldl' accOwnerStake mempty (_poolOwners pool)
      sigma = if totalStake == 0 then 0 else fromIntegral pstakeTot % fromIntegral totalStake
