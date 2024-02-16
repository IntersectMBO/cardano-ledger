{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.PoolRank (
  desirability,
  PerformanceEstimate (..),
  NonMyopic (..),
  getTopRankedPools,
  getTopRankedPoolsVMap,
  nonMyopicStake,
  nonMyopicMemberRew,
  percentile',
  Histogram (..),
  LogWeight (..),
  likelihood,
  applyDecay,
  Likelihood (..),
  leaderProbability,
)
where

import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BoundedRational (..),
  NonNegativeInterval,
  UnitInterval,
  activeSlotVal,
 )
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecShareCBOR (Share, decSharePlusCBOR),
  EncCBOR (encCBOR),
  Interns,
  decSharePlusLensCBOR,
  decodeDouble,
  decodeRecordNamedT,
  encodeDouble,
  encodeListLen,
  toMemptyLens,
 )
import Cardano.Ledger.Coin (Coin (..), coinToRational)
import Cardano.Ledger.Core (Era (..), EraPParams, PParams, ppA0L, ppNOptL)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.EpochBoundary (maxPool)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Rewards (StakeShare (..), memberRew)
import Cardano.Slotting.Slot (EpochSize (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans
import Data.Aeson (FromJSON, KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default, def)
import Data.Foldable (find)
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
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import Lens.Micro ((^.), _1)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet

newtype LogWeight = LogWeight {unLogWeight :: Float}
  deriving (Eq, Generic, Ord, Num, NFData, NoThunks, EncCBOR, DecCBOR, ToJSON, FromJSON)
  deriving (Show) via Quiet LogWeight

toLogWeight :: Double -> LogWeight
toLogWeight d = LogWeight (realToFrac $ log d)

fromLogWeight :: LogWeight -> Double
fromLogWeight (LogWeight l) = exp (realToFrac l)

newtype Histogram = Histogram {unHistogram :: StrictSeq LogWeight}
  deriving (Eq, Show, Generic)

newtype Likelihood = Likelihood {unLikelihood :: StrictSeq LogWeight}
  -- TODO: replace with small data structure
  deriving (Show, Ord, Generic, NFData, EncCBOR, DecCBOR)

instance NoThunks Likelihood

instance Eq Likelihood where
  (==) = (==) `on` unLikelihood . normalizeLikelihood

instance Semigroup Likelihood where
  (Likelihood x) <> (Likelihood y) =
    normalizeLikelihood $ Likelihood (StrictSeq.zipWith (+) x y)

instance Monoid Likelihood where
  mempty = Likelihood $ StrictSeq.forceToStrict $ Seq.replicate (length samplePositions) (LogWeight 0)

instance ToJSON Likelihood where
  toJSON = toJSON . fmap fromLogWeight . unLikelihood
  toEncoding = toEncoding . fmap fromLogWeight . unLikelihood

normalizeLikelihood :: Likelihood -> Likelihood
normalizeLikelihood (Likelihood xs) = Likelihood $ (\x -> x - m) <$> xs
  where
    m = minimum xs

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
    m = fromIntegral $ unEpochSize slotsPerEpoch - fromIntegral blocks
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
    Histogram $
      StrictSeq.zipWith (+) points likelihoods

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

instance EncCBOR PerformanceEstimate where
  encCBOR = encodeDouble . unPerformanceEstimate

instance DecCBOR PerformanceEstimate where
  decCBOR = PerformanceEstimate <$> decodeDouble

data NonMyopic c = NonMyopic
  { likelihoodsNM :: !(Map (KeyHash 'StakePool c) Likelihood)
  , rewardPotNM :: !Coin
  }
  deriving (Show, Eq, Generic)

instance Default (NonMyopic c) where
  def = NonMyopic Map.empty (Coin 0)

instance NoThunks (NonMyopic c)

instance NFData (NonMyopic c)

instance Crypto c => EncCBOR (NonMyopic c) where
  encCBOR
    NonMyopic
      { likelihoodsNM = aps
      , rewardPotNM = rp
      } =
      encodeListLen 2
        <> encCBOR aps
        <> encCBOR rp

instance Crypto c => DecShareCBOR (NonMyopic c) where
  type Share (NonMyopic c) = Interns (KeyHash 'StakePool c)
  decSharePlusCBOR = do
    decodeRecordNamedT "NonMyopic" (const 2) $ do
      likelihoodsNM <- decSharePlusLensCBOR (toMemptyLens _1 id)
      rewardPotNM <- lift decCBOR
      pure $ NonMyopic {likelihoodsNM, rewardPotNM}

instance Crypto crypto => ToJSON (NonMyopic crypto) where
  toJSON = object . toNonMyopicPair
  toEncoding = pairs . mconcat . toNonMyopicPair

toNonMyopicPair :: (KeyValue e a, Crypto crypto) => NonMyopic crypto -> [a]
toNonMyopicPair nm@(NonMyopic _ _) =
  let NonMyopic {likelihoodsNM, rewardPotNM} = nm
   in [ "likelihoodsNM" .= likelihoodsNM
      , "rewardPotNM" .= rewardPotNM
      ]

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
    cost = (fromRational . coinToRational . ppCost) pool
    margin = (fromRational . unboundRational . ppMargin) pool
    tot = max 1 (fromIntegral totalStake)
    Coin pledge = ppPledge pool
    s = fromIntegral pledge % tot
    z0 = 1 % max 1 (fromIntegral nOpt)

-- | Computes the top ranked stake pools
-- corresponding to section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
getTopRankedPools ::
  EraPParams era =>
  Coin ->
  Coin ->
  PParams era ->
  Map (KeyHash 'StakePool c) (PoolParams c) ->
  Map (KeyHash 'StakePool c) PerformanceEstimate ->
  Set (KeyHash 'StakePool c)
getTopRankedPools rPot totalStake pp poolParams aps =
  let pdata = Map.toAscList $ Map.intersectionWith (,) poolParams aps
   in getTopRankedPoolsInternal rPot totalStake pp pdata

getTopRankedPoolsVMap ::
  EraPParams era =>
  Coin ->
  Coin ->
  PParams era ->
  VMap.VMap VMap.VB VMap.VB (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)) ->
  Map (KeyHash 'StakePool (EraCrypto era)) PerformanceEstimate ->
  Set (KeyHash 'StakePool (EraCrypto era))
getTopRankedPoolsVMap rPot totalStake pp poolParams aps =
  let pdata = [(kh, (pps, a)) | (kh, a) <- Map.toAscList aps, Just pps <- [VMap.lookup kh poolParams]]
   in getTopRankedPoolsInternal rPot totalStake pp pdata

getTopRankedPoolsInternal ::
  EraPParams era =>
  Coin ->
  Coin ->
  PParams era ->
  [(KeyHash 'StakePool c, (PoolParams c, PerformanceEstimate))] ->
  Set (KeyHash 'StakePool c)
getTopRankedPoolsInternal rPot totalStake pp pdata =
  Set.fromList $
    fst
      <$> take (fromIntegral $ pp ^. ppNOptL) (sortBy (flip compare `on` snd) rankings)
  where
    rankings =
      [ ( hk
        , desirability (pp ^. ppA0L, pp ^. ppNOptL) rPot pool ap totalStake
        )
      | (hk, (pool, ap)) <- pdata
      ]

-- | Compute the Non-Myopic Pool Stake
--
--   This function implements non-myopic stake calculation in section 5.6.2
--   of "Design Specification for Delegation and Incentives in Cardano".
--   Note that the protocol parameters are implicit in the design document.
--   Additionally, instead of passing a rank r to compare with k,
--   we pass the top k desirable pools and check for membership.
nonMyopicStake ::
  EraPParams era =>
  PParams era ->
  StakeShare ->
  StakeShare ->
  StakeShare ->
  KeyHash 'StakePool c ->
  Set (KeyHash 'StakePool c) ->
  StakeShare
nonMyopicStake pp (StakeShare s) (StakeShare sigma) (StakeShare t) kh topPools =
  let z0 = 1 % max 1 (fromIntegral (pp ^. ppNOptL))
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
  EraPParams era =>
  PParams era ->
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
    let nm = nonMyopicStake pp s sigma t (ppId pool) topPools
        f = maxPool pp rPot (unStakeShare nm) (unStakeShare s)
        fHat = floor (p * (fromRational . coinToRational) f)
     in memberRew (Coin fHat) pool t nm
