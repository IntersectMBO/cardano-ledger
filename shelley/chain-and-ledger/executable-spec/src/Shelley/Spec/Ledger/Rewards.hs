{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Shelley.Spec.Ledger.Rewards
  ( desirability
  , ApparentPerformance (..)
  , NonMyopic (..)
  , emptyNonMyopic
  , getTopRankedPools
  , StakeShare(..)
  , mkApparentPerformance
  , reward
  , nonMyopicStake
  , nonMyopicMemberRew
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeDouble, encodeDouble,
                     encodeListLen, enforceSize)
import           Cardano.Ledger.Shelley.Crypto (Crypto)
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ledger.Core ((◁))
import           Shelley.Spec.Ledger.BaseTypes (UnitInterval (..), intervalValue, mkUnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.PoolParams (poolSpec)
import           Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), SnapShot (..), Stake (..),
                     emptySnapShot, maxPool, poolStake)
import           Shelley.Spec.Ledger.Keys (KeyHash)
import           Shelley.Spec.Ledger.PParams (PParams, _a0, _d, _nOpt)
import           Shelley.Spec.Ledger.TxData (Credential (..), PoolParams (..), RewardAcnt (..))


newtype ApparentPerformance = ApparentPerformance { unApparentPerformance :: Double }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

instance ToCBOR ApparentPerformance
 where toCBOR = encodeDouble . unApparentPerformance

instance FromCBOR ApparentPerformance
 where fromCBOR = ApparentPerformance <$> decodeDouble

data NonMyopic crypto= NonMyopic
  { apparentPerformances :: Map (KeyHash crypto) ApparentPerformance
  , rewardPot :: Coin
  , snap :: SnapShot crypto
  } deriving (Show, Eq, Generic)

emptyNonMyopic :: NonMyopic crypto
emptyNonMyopic = NonMyopic Map.empty (Coin 0) emptySnapShot

instance NoUnexpectedThunks (NonMyopic crypto)

instance Crypto crypto => ToCBOR (NonMyopic crypto)
 where
  toCBOR NonMyopic
           { apparentPerformances = aps
           , rewardPot = rp
           , snap = s
           } =
    encodeListLen 3
      <> toCBOR aps
      <> toCBOR rp
      <> toCBOR s

instance Crypto crypto => FromCBOR (NonMyopic crypto)
 where
  fromCBOR = do
    enforceSize "NonMyopic" 3
    aps <- fromCBOR
    rp <- fromCBOR
    s <- fromCBOR
    pure $ NonMyopic { apparentPerformances = aps
                     , rewardPot = rp
                     , snap = s
                     }

-- | Desirability calculation for non-myopic utily,
-- corresponding to f^~ in section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
desirability
  :: PParams
  -> Coin
  -> PoolParams crypto
  -> ApparentPerformance
  -> Coin
  -> Double
desirability pp r pool (ApparentPerformance p) (Coin total) =
  if fTilde <= cost
    then 0
    else (fTilde - cost)*(1 - margin)
  where
    fTilde      = fTildeNumer / fTildeDenom
    fTildeNumer = p * fromRational (fromIntegral r * (z0 + min s z0 * a0))
    fTildeDenom = fromRational $ 1 + a0

    cost   = (fromIntegral . _poolCost) pool
    margin = (fromRational . intervalValue . _poolMargin) pool
    tot = max 1 (fromIntegral total)

    Coin pledge = _poolPledge pool
    s = fromIntegral pledge % tot
    a0 = _a0 pp
    z0 = 1 % max 1 (fromIntegral (_nOpt pp))

-- | Computes the top ranked stake pools
-- corresponding to section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
getTopRankedPools
  :: Coin
  -> Coin
  -> PParams
  -> Map (KeyHash crypto) (PoolParams crypto)
  -> Map (KeyHash crypto) ApparentPerformance
  -> Set (KeyHash crypto)
getTopRankedPools rPot total pp poolParams aps =
  Set.fromList $ fmap fst $
    take (fromIntegral $ _nOpt pp) (sortBy (flip compare `on` snd) rankings)
  where
    pdata =
      [ ( hk
        , ( poolParams Map.! hk
          , aps Map.! hk))
      | hk <-
          Set.toList $ Map.keysSet poolParams `Set.intersection` Map.keysSet aps
      ]
    rankings =
      [ ( hk
        , desirability pp rPot pool ap total)
      | (hk, (pool, ap)) <- pdata
      ]

-- | StakeShare type
newtype StakeShare =
  StakeShare Rational
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

-- | Calculate pool reward
mkApparentPerformance
  :: UnitInterval
  -> UnitInterval
  -> Natural
  -> Natural
  -> Rational
mkApparentPerformance d_ sigma blocksN blocksTotal
  | sigma' == 0 = 0
  | intervalValue d_ < 0.8 = beta / sigma'
  | otherwise = 1
  where
    beta = fromIntegral blocksN / fromIntegral (max 1 blocksTotal)
    sigma' = intervalValue sigma

-- | Calculate pool leader reward
leaderRew
  :: Coin
  -> PoolParams crypto
  -> StakeShare
  -> StakeShare
  -> Coin
leaderRew f@(Coin f') pool (StakeShare s) (StakeShare sigma)
  | f' <= c = f
  | otherwise =
    Coin $ c + floor (fromIntegral (f' - c) * (m' + (1 - m') * s / sigma))
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Calculate pool member reward
memberRew
  :: Coin
  -> PoolParams crypto
  -> StakeShare
  -> StakeShare
  -> Coin
memberRew (Coin f') pool (StakeShare t) (StakeShare sigma)
  | f' <= c = 0
  | otherwise = floor $ fromIntegral (f' - c) * (1 - m') * t / sigma
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Reward one pool
rewardOnePool
  :: PParams
  -> Coin
  -> Natural
  -> Natural
  -> Credential crypto
  -> PoolParams crypto
  -> Stake crypto
  -> Coin
  -> Set (RewardAcnt crypto)
  -> (Map (RewardAcnt crypto) Coin, Rational)
rewardOnePool pp r blocksN blocksTotal poolHK pool (Stake stake) (Coin total) addrsRew =
  (rewards', appPerf)
  where
    Coin pstake = sum stake
    Coin ostake = Set.foldl
                    (\c o -> c + (stake Map.! KeyHashObj o))
                    (Coin 0)
                    (_poolOwners pool)
    sigma = fromIntegral pstake % fromIntegral total
    Coin pledge = _poolPledge pool
    pr = fromIntegral pledge % fromIntegral total
    (Coin maxP) =
      if pledge <= ostake
        then maxPool pp r sigma pr
        else 0
    s' = fromMaybe (error "LedgerState.rewardOnePool: Unexpected Nothing") $ mkUnitInterval sigma
    appPerf = mkApparentPerformance (_d pp) s' blocksN blocksTotal
    poolR = floor (appPerf * fromIntegral maxP)
    tot = fromIntegral total
    mRewards = Map.fromList
     [(RewardAcnt hk,
       memberRew poolR pool (StakeShare (fromIntegral c % tot)) (StakeShare sigma))
     | (hk, Coin c) <- Map.toList stake, hk /= poolHK]
    iReward  = leaderRew poolR pool (StakeShare $ fromIntegral ostake % tot) (StakeShare sigma)
    potentialRewards = Map.insert (_poolRAcnt pool) iReward mRewards
    rewards' = Map.filter (/= Coin 0) $ addrsRew ◁ potentialRewards

reward
  :: PParams
  -> BlocksMade crypto
  -> Coin
  -> Set (RewardAcnt crypto)
  -> Map (KeyHash crypto) (PoolParams crypto)
  -> Stake crypto
  -> Map (Credential crypto) (KeyHash crypto)
  -> Coin
  -> (Map (RewardAcnt crypto) Coin, Map (KeyHash crypto) Rational)
reward pp (BlocksMade b) r addrsRew poolParams stake delegs total =
  (rewards', appPerformances)
  where
    pdata =
      [ ( hk
        , ( poolParams Map.! hk
          , b Map.! hk
          , poolStake hk delegs stake))
      | hk <-
          Set.toList $ Map.keysSet poolParams `Set.intersection` Map.keysSet b
      ]
    results =
      [ ( hk
        , rewardOnePool pp r n totalBlocks (KeyHashObj hk) pool actgr total addrsRew)
      | (hk, (pool, n, actgr)) <- pdata
      ]
    rewards' = foldl (\m (_, r') -> Map.union m (fst r')) Map.empty results
    appPerformances = Map.fromList $ fmap (\(hk, r') -> (hk, snd r')) results
    totalBlocks = sum b

nonMyopicStake
  :: KeyHash crypto
  -> StakeShare
  -> StakeShare
  -> PParams
  -> Set (KeyHash crypto)
  -> StakeShare
nonMyopicStake kh (StakeShare sigma) (StakeShare s) pp topPools =
  let
    z0 = 1 % max 1 (fromIntegral (_nOpt pp))
  in
    if kh `Set.member` topPools
      then StakeShare (max sigma z0)
      else StakeShare s

nonMyopicMemberRew
  :: PParams
  -> PoolParams crypto
  -> Coin
  -> StakeShare
  -> StakeShare
  -> StakeShare
  -> ApparentPerformance
  -> Coin
nonMyopicMemberRew
  pp
  pool
  rPot
  (StakeShare s)
  (StakeShare t)
  (StakeShare nm)
  (ApparentPerformance p) =

  let
    nm' = max t nm -- TODO check with researchers that this is how to handle t > nm
    (Coin f) = maxPool pp rPot nm' s
    fHat = floor (p * fromIntegral f)
  in
    memberRew (Coin fHat) pool (StakeShare s) (StakeShare nm')
