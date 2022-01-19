{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rewards
  ( desirability,
    PerformanceEstimate (..),
    NonMyopic (..),
    getTopRankedPools,
    getTopRankedPoolsVMap,
    StakeShare (..),
    PoolRewardInfo (..),
    mkApparentPerformance,
    RewardType (..),
    LeaderOnlyReward (..),
    leaderRewardToGeneral,
    Reward (..),
    nonMyopicStake,
    nonMyopicMemberRew,
    percentile',
    Histogram (..),
    LogWeight (..),
    likelihood,
    applyDecay,
    Likelihood (..),
    leaderProbability,
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
    decodeDouble,
    decodeWord,
    encodeDouble,
    encodeListLen,
    encodeWord,
  )
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    BlocksMade (..),
    BoundedRational (..),
    NonNegativeInterval,
    ProtVer,
    UnitInterval,
    activeSlotVal,
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
import Cardano.Ledger.Serialization
  ( decodeRecordNamedT,
    decodeSeq,
    encodeFoldable,
  )
import Cardano.Ledger.Shelley.Delegation.PoolParams (poolSpec)
import Cardano.Ledger.Shelley.EpochBoundary
  ( Stake (..),
    maxPool,
    maxPool',
  )
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.Val ((<->))
import Cardano.Slotting.Slot (EpochSize (..))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans
import Data.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import qualified Data.Compact.VMap as VMap
import Data.Default.Class (Default, def)
import Data.Foldable (find, fold, foldMap')
import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Lens.Micro (_1)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet

newtype LogWeight = LogWeight {unLogWeight :: Float}
  deriving (Eq, Generic, Ord, Num, NFData, NoThunks, ToCBOR, FromCBOR)
  deriving (Show) via Quiet LogWeight

toLogWeight :: Double -> LogWeight
toLogWeight d = LogWeight (realToFrac $ log d)

fromLogWeight :: LogWeight -> Double
fromLogWeight (LogWeight l) = exp (realToFrac l)

newtype Histogram = Histogram {unHistogram :: StrictSeq LogWeight}
  deriving (Eq, Show, Generic)

newtype Likelihood = Likelihood {unLikelihood :: StrictSeq LogWeight}
  -- TODO: replace with small data structure
  deriving (Show, Ord, Generic, NFData)

instance NoThunks Likelihood

instance Eq Likelihood where
  (==) = (==) `on` unLikelihood . normalizeLikelihood

instance Semigroup Likelihood where
  (Likelihood x) <> (Likelihood y) =
    normalizeLikelihood $ Likelihood (StrictSeq.zipWith (+) x y)

instance Monoid Likelihood where
  mempty = Likelihood $ StrictSeq.forceToStrict $ Seq.replicate (length samplePositions) (LogWeight 0)

normalizeLikelihood :: Likelihood -> Likelihood
normalizeLikelihood (Likelihood xs) = Likelihood $ (\x -> x - m) <$> xs
  where
    m = minimum xs

instance ToCBOR Likelihood where
  toCBOR (Likelihood logweights) = encodeFoldable logweights

instance FromCBOR Likelihood where
  fromCBOR = Likelihood . StrictSeq.forceToStrict <$> decodeSeq fromCBOR

leaderProbability :: ActiveSlotCoeff -> Rational -> UnitInterval -> Double
leaderProbability activeSlotCoeff relativeStake decentralizationParameter =
  (1 - (1 - asc) ** s) * (1 - d')
  where
    d' = realToFrac . unboundRational $ decentralizationParameter
    asc = realToFrac . unboundRational . activeSlotVal $ activeSlotCoeff
    s = realToFrac relativeStake

samplePositions :: StrictSeq Double
samplePositions = (\x -> (x + 0.5) / 100.0) <$> StrictSeq.fromList [0.0 .. 99.0]

likelihood ::
  Natural -> -- number of blocks produced this epoch
  Double -> -- chance we're allowed to produce a block in this slot
  EpochSize ->
  Likelihood
likelihood blocks t slotsPerEpoch =
  Likelihood $
    sample <$> samplePositions
  where
    -- The likelihood function L(x) is the probability of observing the data we got
    -- under the assumption that the underlying pool performance is equal to x.
    -- L(x) = C(n,m) * (tx)^n * (1-tx)^m
    -- where
    -- t is the chance we're allowed to produce a block
    -- n is the number of slots in which a block was produced
    -- m is the number of slots in which a block was not produced
    --      (slots per epoch minus n)
    -- C(n,m) is a coefficient that will be irrelevant
    -- Since the likelihood function only matters up to a scalar multiple, we will
    -- will divide out C(n,m) t^n and use the following instead:
    -- L(x) = x^n * (1-tx)^m
    -- We represent this function using 100 sample points, but to avoid very
    -- large exponents, we store the log of the value instead of the value itself.
    -- log(L(x)) = log [ x^n * (1-tx)^m ]
    --           = n * log(x) + m * log(1 - tx)
    -- TODO: worry more about loss of floating point precision
    --
    -- example:
    -- a pool has relative stake of 1 / 1,000,000 (~ 30k ada of 35b ada)
    -- f = active slot coefficient = 1/20
    -- t = 1 - (1-f)^(1/1,000,000)
    n = fromIntegral blocks
    m = fromIntegral $ slotsPerEpoch - fromIntegral blocks
    l :: Double -> Double
    l x = n * log x + m * log (1 - t * x)
    sample position = LogWeight (realToFrac $ l position)

-- | Decay previous likelihood
applyDecay :: Float -> Likelihood -> Likelihood
applyDecay decay (Likelihood logWeights) = Likelihood $ mul decay <$> logWeights
  where
    mul x (LogWeight f) = LogWeight (x * f)

posteriorDistribution :: Histogram -> Likelihood -> Histogram
posteriorDistribution (Histogram points) (Likelihood likelihoods) =
  normalize $
    Histogram $ StrictSeq.zipWith (+) points likelihoods

-- | Normalize the histogram so that the total area is 1
normalize :: Histogram -> Histogram
normalize (Histogram values) = Histogram $ (\x -> x - logArea) <$> values'
  where
    m = maximum values
    values' = (\x -> x - m) <$> values
    logArea = toLogWeight area
    area = reimannSum 0.01 (fromLogWeight <$> values')

-- | Calculate the k percentile for this distribution.
-- k is a value between 0 and 1. The 0 percentile is 0 and the 1 percentile is 1
percentile :: Double -> Histogram -> Likelihood -> PerformanceEstimate
percentile p prior likelihoods =
  PerformanceEstimate . fst $
    fromMaybe (1, 1) $
      find (\(_x, fx) -> fx > p) cdf
  where
    (Histogram values) = posteriorDistribution prior likelihoods
    cdf =
      Seq.zip
        (StrictSeq.fromStrict samplePositions)
        (StrictSeq.fromStrict (StrictSeq.scanl (+) 0 (fromLogWeight <$> values)))

percentile' :: Likelihood -> PerformanceEstimate
percentile' = percentile 0.5 h
  where
    h = normalize . Histogram $ logBeta 40 1 <$> samplePositions
    -- Beta(n,m)(x) = C * x^(n-1)*(1-x)^(m-1)
    -- log( Beta(n,m)(x) ) = (n-1) * log x + (m-1) * log (1-x)
    logBeta n m x = LogWeight . realToFrac $ (n - 1) * log x + (m - 1) * log (1 - x)

reimannSum :: (Functor f, Foldable f) => Double -> f Double -> Double
reimannSum width heights = sum $ fmap (width *) heights

-- | This is a estimate of the proportion of allowed blocks a pool will
-- make in the future. It is used for ranking pools in delegation.
newtype PerformanceEstimate = PerformanceEstimate {unPerformanceEstimate :: Double}
  deriving (Show, Eq, Generic, NoThunks)

instance ToCBOR PerformanceEstimate where
  toCBOR = encodeDouble . unPerformanceEstimate

instance FromCBOR PerformanceEstimate where
  fromCBOR = PerformanceEstimate <$> decodeDouble

data NonMyopic crypto = NonMyopic
  { likelihoodsNM :: !(Map (KeyHash 'StakePool crypto) Likelihood),
    rewardPotNM :: !Coin
  }
  deriving (Show, Eq, Generic)

instance Default (NonMyopic crypto) where
  def = NonMyopic Map.empty (Coin 0)

instance NoThunks (NonMyopic crypto)

instance NFData (NonMyopic crypto)

instance CC.Crypto crypto => ToCBOR (NonMyopic crypto) where
  toCBOR
    NonMyopic
      { likelihoodsNM = aps,
        rewardPotNM = rp
      } =
      encodeListLen 3
        <> toCBOR aps
        <> toCBOR rp

instance CC.Crypto crypto => FromSharedCBOR (NonMyopic crypto) where
  type Share (NonMyopic crypto) = Interns (KeyHash 'StakePool crypto)
  fromSharedPlusCBOR = do
    decodeRecordNamedT "NonMyopic" (const 3) $ do
      likelihoodsNM <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
      rewardPotNM <- lift fromCBOR
      pure $ NonMyopic {likelihoodsNM, rewardPotNM}

-- | Desirability calculation for non-myopic utility,
-- corresponding to f^~ in section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
desirability ::
  (NonNegativeInterval, Natural) ->
  Coin ->
  PoolParams c ->
  PerformanceEstimate ->
  Coin ->
  Double
desirability (a0, nOpt) r pool (PerformanceEstimate p) (Coin totalStake) =
  if fTilde <= cost
    then 0
    else (fTilde - cost) * (1 - margin)
  where
    fTilde = fTildeNumer / fTildeDenom
    fTildeNumer = p * fromRational (coinToRational r * (z0 + min s z0 * unboundRational a0))
    fTildeDenom = fromRational $ 1 + unboundRational a0
    cost = (fromRational . coinToRational . _poolCost) pool
    margin = (fromRational . unboundRational . _poolMargin) pool
    tot = max 1 (fromIntegral totalStake)
    Coin pledge = _poolPledge pool
    s = fromIntegral pledge % tot
    z0 = 1 % max 1 (fromIntegral nOpt)

-- | Computes the top ranked stake pools
-- corresponding to section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
getTopRankedPools ::
  (HasField "_a0" pp NonNegativeInterval, HasField "_nOpt" pp Natural) =>
  Coin ->
  Coin ->
  pp ->
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  Map (KeyHash 'StakePool crypto) PerformanceEstimate ->
  Set (KeyHash 'StakePool crypto)
getTopRankedPools rPot totalStake pp poolParams aps =
  let pdata = Map.toAscList $ Map.intersectionWith (,) poolParams aps
   in getTopRankedPoolsInternal rPot totalStake pp pdata

getTopRankedPoolsVMap ::
  (HasField "_a0" pp NonNegativeInterval, HasField "_nOpt" pp Natural) =>
  Coin ->
  Coin ->
  pp ->
  VMap.VMap VMap.VB VMap.VB (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  Map (KeyHash 'StakePool crypto) PerformanceEstimate ->
  Set (KeyHash 'StakePool crypto)
getTopRankedPoolsVMap rPot totalStake pp poolParams aps =
  let pdata = [(kh, (pps, a)) | (kh, a) <- Map.toAscList aps, Just pps <- [VMap.lookup kh poolParams]]
   in getTopRankedPoolsInternal rPot totalStake pp pdata

getTopRankedPoolsInternal ::
  (HasField "_a0" pp NonNegativeInterval, HasField "_nOpt" pp Natural) =>
  Coin ->
  Coin ->
  pp ->
  [(KeyHash 'StakePool crypto, (PoolParams crypto, PerformanceEstimate))] ->
  Set (KeyHash 'StakePool crypto)
getTopRankedPoolsInternal rPot totalStake pp pdata =
  Set.fromList $
    fst
      <$> take (fromIntegral $ getField @"_nOpt" pp) (sortBy (flip compare `on` snd) rankings)
  where
    rankings =
      [ ( hk,
          desirability (getField @"_a0" pp, getField @"_nOpt" pp) rPot pool ap totalStake
        )
        | (hk, (pool, ap)) <- pdata
      ]

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
  { lRewardPool :: KeyHash 'StakePool crypto,
    lRewardAmount :: Coin
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

-- | Compute the Non-Myopic Pool Stake
--
--   This function implements non-myopic stake calculation in section 5.6.2
--   of "Design Specification for Delegation and Incentives in Cardano".
--   Note that the protocol parameters are implicit in the design document.
--   Additionally, instead of passing a rank r to compare with k,
--   we pass the top k desirable pools and check for membership.
nonMyopicStake ::
  HasField "_nOpt" pp Natural =>
  pp ->
  StakeShare ->
  StakeShare ->
  StakeShare ->
  KeyHash 'StakePool c ->
  Set (KeyHash 'StakePool c) ->
  StakeShare
nonMyopicStake pp (StakeShare s) (StakeShare sigma) (StakeShare t) kh topPools =
  let z0 = 1 % max 1 (fromIntegral (getField @"_nOpt" pp))
   in if kh `Set.member` topPools
        then StakeShare (max (sigma + t) z0)
        else StakeShare (s + t)

-- | Compute the Non-Myopic Pool Member Reward
--
--   This function implements equation (3) in section 5.6.4
--   of "Design Specification for Delegation and Incentives in Cardano".
--   Note that the protocol parameters and the reward pot are implicit
--   in the design document. Additionally, instead of passing a rank
--   r to compare with k, we pass the top k desirable pools and
--   check for membership.
nonMyopicMemberRew ::
  ( HasField "_a0" pp NonNegativeInterval,
    HasField "_nOpt" pp Natural
  ) =>
  pp ->
  Coin ->
  PoolParams c ->
  StakeShare ->
  StakeShare ->
  StakeShare ->
  Set (KeyHash 'StakePool c) ->
  PerformanceEstimate ->
  Coin
nonMyopicMemberRew
  pp
  rPot
  pool
  s
  sigma
  t
  topPools
  (PerformanceEstimate p) =
    let nm = nonMyopicStake pp s sigma t (_poolId pool) topPools
        f = maxPool pp rPot (unStakeShare nm) (unStakeShare s)
        fHat = floor (p * (fromRational . coinToRational) f)
     in memberRew (Coin fHat) pool t nm
