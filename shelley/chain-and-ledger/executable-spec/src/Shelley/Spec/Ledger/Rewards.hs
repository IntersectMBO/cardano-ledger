{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Shelley.Spec.Ledger.Rewards
  ( desirability,
    PerformanceEstimate (..),
    NonMyopic (..),
    emptyNonMyopic,
    getTopRankedPools,
    StakeShare (..),
    mkApparentPerformance,
    reward,
    nonMyopicStake,
    nonMyopicMemberRew,
    percentile',
    Histogram (..),
    LogWeight (..),
    likelihood,
    Likelihood (..),
    leaderProbability,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeDouble,
    encodeDouble,
    encodeListLen,
  )
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Cardano.Slotting.Slot (EpochSize)
import Control.Iterate.SetAlgebra (eval, (◁))
import Data.Foldable (find, fold)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ratio ((%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Quiet
import Shelley.Spec.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    UnitInterval,
    activeSlotVal,
    unitIntervalToRational,
  )
import Shelley.Spec.Ledger.Coin
  ( Coin (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Delegation.PoolParams (poolSpec)
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    SnapShot (..),
    Stake (..),
    emptySnapShot,
    maxPool,
    poolStake,
  )
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.PParams (PParams, _a0, _d, _nOpt)
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed, decodeSeq, encodeFoldable)
import Shelley.Spec.Ledger.TxData (PoolParams (..), getRwdCred)

newtype LogWeight = LogWeight {unLogWeight :: Float}
  deriving (Eq, Generic, Ord, Num, NFData, NoUnexpectedThunks, ToCBOR, FromCBOR)
  deriving (Show) via Quiet LogWeight

toLogWeight :: Double -> LogWeight
toLogWeight d = LogWeight (realToFrac $ log d)

fromLogWeight :: LogWeight -> Double
fromLogWeight (LogWeight l) = exp (realToFrac l)

data Histogram = Histogram {unHistogram :: Seq LogWeight}
  deriving (Eq, Show, Generic)

newtype Likelihood = Likelihood {unLikelihood :: Seq LogWeight}
  -- TODO: replace with small data structure
  deriving (Show, Generic, NFData)

instance NoUnexpectedThunks Likelihood

instance Eq Likelihood where
  (==) = (==) `on` unLikelihood . normalizeLikelihood

instance Semigroup Likelihood where
  (Likelihood x) <> (Likelihood y) = normalizeLikelihood $ Likelihood (Seq.zipWith (+) x y)

instance Monoid Likelihood where
  mempty = Likelihood $ Seq.replicate (length samplePositions) (LogWeight 0)

normalizeLikelihood :: Likelihood -> Likelihood
normalizeLikelihood (Likelihood xs) = Likelihood $ (\x -> x - m) <$> xs
  where
    m = minimum xs

instance ToCBOR Likelihood where
  toCBOR (Likelihood logweights) = encodeFoldable logweights

instance FromCBOR Likelihood where
  fromCBOR = Likelihood <$> decodeSeq fromCBOR

leaderProbability :: ActiveSlotCoeff -> Rational -> UnitInterval -> Double
leaderProbability activeSlotCoeff relativeStake decentralizationParameter =
  (1 - (1 - asc) ** s) * (1 - d')
  where
    d' = realToFrac . unitIntervalToRational $ decentralizationParameter
    asc = realToFrac . unitIntervalToRational . activeSlotVal $ activeSlotCoeff
    s = realToFrac relativeStake

samplePositions :: [Double]
samplePositions = (\x -> (x + 0.5) / 100.0) <$> [0.0 .. 99.0]

likelihood ::
  Natural -> -- number of blocks produced this epoch
  Double -> -- chance we're allowed to produce a block in this slot
  EpochSize ->
  Likelihood
likelihood blocks t slotsPerEpoch = Likelihood . Seq.fromList $ sample <$> samplePositions
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

posteriorDistribution :: Histogram -> Likelihood -> Histogram
posteriorDistribution (Histogram points) (Likelihood likelihoods) = normalize $ Histogram $ Seq.zipWith (+) points likelihoods

-- TODO decay the histogram

-- | Normalize the histogram so that the total area is 1
normalize :: Histogram -> Histogram
normalize (Histogram values) = Histogram $ (\x -> x - logArea) <$> values
  where
    logArea = toLogWeight area
    area = reimannSum 0.01 (fromLogWeight <$> values)

-- | Calculate the k percentile for this distribution.
-- k is a value between 0 and 1. The 0 percentile is 0 and the 1 percentile is 1
percentile :: Double -> Histogram -> Likelihood -> PerformanceEstimate
percentile p prior likelihoods =
  PerformanceEstimate . fst $
    fromMaybe (1, 1) $
      find (\(_x, fx) -> fx > p) cdf
  where
    (Histogram values) = posteriorDistribution prior likelihoods
    cdf = Seq.zip (Seq.fromList samplePositions) $ Seq.scanl (+) 0 (fromLogWeight <$> values)

percentile' :: Likelihood -> PerformanceEstimate
percentile' = percentile 0.1 h
  where
    h = normalize . Histogram . Seq.fromList $ logBeta 40 3 <$> samplePositions
    -- Beta(n,m)(x) = C * x^(n-1)*(1-x)^(m-1)
    -- log( Beta(n,m)(x) ) = (n-1) * log x + (m-1) * log (1-x)
    logBeta n m x = LogWeight . realToFrac $ (n -1) * log x + (m -1) * log (1 - x)

reimannSum :: (Functor f, Foldable f) => Double -> f Double -> Double
reimannSum width heights = sum $ fmap (width *) heights

-- | This is a estimate of the proportion of allowed blocks a pool will
-- make in the future. It is used for ranking pools in delegation.
newtype PerformanceEstimate = PerformanceEstimate {unPerformanceEstimate :: Double}
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

instance ToCBOR PerformanceEstimate where
  toCBOR = encodeDouble . unPerformanceEstimate

instance FromCBOR PerformanceEstimate where
  fromCBOR = PerformanceEstimate <$> decodeDouble

data NonMyopic crypto = NonMyopic
  { likelihoodsNM :: !(Map (KeyHash 'StakePool crypto) Likelihood),
    rewardPotNM :: !Coin,
    snapNM :: !(SnapShot crypto) -- TODO we can remove this map
  }
  deriving (Show, Eq, Generic)

emptyNonMyopic :: NonMyopic crypto
emptyNonMyopic = NonMyopic Map.empty (Coin 0) emptySnapShot

instance NoUnexpectedThunks (NonMyopic crypto)

instance NFData (NonMyopic crypto)

instance Crypto crypto => ToCBOR (NonMyopic crypto) where
  toCBOR
    NonMyopic
      { likelihoodsNM = aps,
        rewardPotNM = rp,
        snapNM = s
      } =
      encodeListLen 3
        <> toCBOR aps
        <> toCBOR rp
        <> toCBOR s

instance Crypto crypto => FromCBOR (NonMyopic crypto) where
  fromCBOR = do
    decodeRecordNamed "NonMyopic" (const 3) $ do
      aps <- fromCBOR
      rp <- fromCBOR
      s <- fromCBOR
      pure $
        NonMyopic
          { likelihoodsNM = aps,
            rewardPotNM = rp,
            snapNM = s
          }

-- | Desirability calculation for non-myopic utily,
-- corresponding to f^~ in section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
desirability ::
  PParams ->
  Coin ->
  PoolParams crypto ->
  PerformanceEstimate ->
  Coin ->
  Double
desirability pp r pool (PerformanceEstimate p) (Coin total) =
  if fTilde <= cost
    then 0
    else (fTilde - cost) * (1 - margin)
  where
    fTilde = fTildeNumer / fTildeDenom
    fTildeNumer = p * fromRational (coinToRational r * (z0 + min s z0 * a0))
    fTildeDenom = fromRational $ 1 + a0
    cost = (fromRational . coinToRational . _poolCost) pool
    margin = (fromRational . unitIntervalToRational . _poolMargin) pool
    tot = max 1 (fromIntegral total)
    Coin pledge = _poolPledge pool
    s = fromIntegral pledge % tot
    a0 = _a0 pp
    z0 = 1 % max 1 (fromIntegral (_nOpt pp))

-- | Computes the top ranked stake pools
-- corresponding to section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
getTopRankedPools ::
  Coin ->
  Coin ->
  PParams ->
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  Map (KeyHash 'StakePool crypto) PerformanceEstimate ->
  Set (KeyHash 'StakePool crypto)
getTopRankedPools rPot total pp poolParams aps =
  Set.fromList $
    fmap fst $
      take (fromIntegral $ _nOpt pp) (sortBy (flip compare `on` snd) rankings)
  where
    pdata = Map.toList $ Map.intersectionWith (,) poolParams aps
    rankings =
      [ ( hk,
          desirability pp rPot pool ap total
        )
        | (hk, (pool, ap)) <- pdata
      ]

-- | StakeShare type
newtype StakeShare = StakeShare {unStakeShare :: Rational}
  deriving (Generic, Ord, Eq, NoUnexpectedThunks)
  deriving (Show) via Quiet StakeShare

-- | Calculate pool reward
mkApparentPerformance ::
  UnitInterval ->
  Rational ->
  Natural ->
  Natural ->
  Rational
mkApparentPerformance d_ sigma blocksN blocksTotal
  | sigma == 0 = 0
  | unitIntervalToRational d_ < 0.8 = beta / sigma
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
      + rationalToCoinViaFloor
        (coinToRational (f - c) * (m' + (1 - m') * s / sigma))
  where
    (c, m, _) = poolSpec pool
    m' = unitIntervalToRational m

-- | Calculate pool member reward
memberRew ::
  Coin ->
  PoolParams crypto ->
  StakeShare ->
  StakeShare ->
  Coin
memberRew (Coin f') pool (StakeShare t) (StakeShare sigma)
  | f' <= c = 0
  | otherwise = rationalToCoinViaFloor $ fromIntegral (f' - c) * (1 - m') * t / sigma
  where
    (Coin c, m, _) = poolSpec pool
    m' = unitIntervalToRational m

-- | Reward one pool
rewardOnePool ::
  PParams ->
  Coin ->
  Natural ->
  Natural ->
  PoolParams crypto ->
  Stake crypto ->
  Rational ->
  Rational ->
  Coin ->
  Set (Credential 'Staking crypto) ->
  Map (Credential 'Staking crypto) Coin
rewardOnePool pp r blocksN blocksTotal pool (Stake stake) sigma sigmaA (Coin total) addrsRew =
  rewards'
  where
    Coin ostake =
      Set.foldl'
        (\c o -> c + (fromMaybe (Coin 0) $ Map.lookup (KeyHashObj o) stake))
        (Coin 0)
        (_poolOwners pool)
    Coin pledge = _poolPledge pool
    pr = fromIntegral pledge % fromIntegral total
    (Coin maxP) =
      if pledge <= ostake
        then maxPool pp r sigma pr
        else 0
    appPerf = mkApparentPerformance (_d pp) sigmaA blocksN blocksTotal
    poolR = rationalToCoinViaFloor (appPerf * fromIntegral maxP)
    tot = fromIntegral total
    poolHashes = (KeyHashObj `Set.map` _poolOwners pool)
    mRewards =
      Map.fromList
        [ ( hk,
            memberRew poolR pool (StakeShare (fromIntegral c % tot)) (StakeShare sigma)
          )
          | (hk, Coin c) <- Map.toList stake,
            hk `Set.notMember` poolHashes
        ]
    iReward = leaderRew poolR pool (StakeShare $ fromIntegral ostake % tot) (StakeShare sigma)
    potentialRewards = Map.insert (getRwdCred $ _poolRAcnt pool) iReward mRewards
    rewards' = Map.filter (/= Coin 0) $ eval (addrsRew ◁ potentialRewards)

reward ::
  PParams ->
  BlocksMade crypto ->
  Coin ->
  Set (Credential 'Staking crypto) ->
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  Stake crypto ->
  Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto) ->
  Coin ->
  ActiveSlotCoeff ->
  EpochSize ->
  (Map (Credential 'Staking crypto) Coin, Map (KeyHash 'StakePool crypto) Likelihood)
reward
  pp
  (BlocksMade b)
  r
  addrsRew
  poolParams
  stake
  delegs
  (Coin total)
  asc
  slotsPerEpoch = (rewards', hs)
    where
      totalBlocks = sum b
      Coin totalActive = sum . unStake $ stake
      results = do
        (hk, pparams) <- Map.toList poolParams
        let sigma = fromIntegral pstake % fromIntegral total
            sigmaA = fromIntegral pstake % fromIntegral totalActive
            blocksProduced = Map.lookup hk b
            actgr@(Stake s) = poolStake hk delegs stake
            Coin pstake = sum s
            rewardMap = case blocksProduced of
              Nothing -> Nothing -- This is equivalent to calling rewarOnePool with n = 0
              Just n ->
                Just $
                  rewardOnePool
                    pp
                    r
                    n
                    totalBlocks
                    pparams
                    actgr
                    sigma
                    sigmaA
                    (Coin total)
                    addrsRew
            ls =
              likelihood
                (fromMaybe 0 blocksProduced)
                (leaderProbability asc sigma (_d pp))
                slotsPerEpoch
        pure (hk, rewardMap, ls)
      rewards' = fold $ catMaybes $ fmap (\(_, x, _) -> x) results
      hs = Map.fromList $ fmap (\(hk, _, l) -> (hk, l)) results

nonMyopicStake ::
  KeyHash 'StakePool crypto ->
  StakeShare ->
  StakeShare ->
  PParams ->
  Set (KeyHash 'StakePool crypto) ->
  StakeShare
nonMyopicStake kh (StakeShare sigma) (StakeShare s) pp topPools =
  let z0 = 1 % max 1 (fromIntegral (_nOpt pp))
   in if kh `Set.member` topPools
        then StakeShare (max sigma z0)
        else StakeShare s

nonMyopicMemberRew ::
  PParams ->
  PoolParams crypto ->
  Coin ->
  StakeShare ->
  StakeShare ->
  StakeShare ->
  PerformanceEstimate ->
  Coin
nonMyopicMemberRew
  pp
  pool
  rPot
  (StakeShare s)
  (StakeShare t)
  (StakeShare nm)
  (PerformanceEstimate p) =
    let nm' = max t nm -- TODO check with researchers that this is how to handle t > nm
        f = maxPool pp rPot nm' s
        fHat = floor (p * (fromRational . coinToRational) f)
     in memberRew (Coin fHat) pool (StakeShare t) (StakeShare nm')
