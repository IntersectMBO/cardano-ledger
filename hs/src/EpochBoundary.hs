{-|
Module      : EpochBoundary
Description : Functions and definitions for rules at epoch boundary.

This modules implements the necessary functions for the changes that can happen at epoch boundaries.
-}
module EpochBoundary
  ( Stake
  , Distr
  , Production
  , StakeShare
  , mkStakeShare
  , baseStake
  , ptrStake
  , stake
  , isActive
  , activeStake
  , obligation
  , poolRefunds
  , maxPool
  , movingAvg
  , poolRew
  , leaderRew
  , memberRew
  ) where

import           Coin
import           Delegation.Certificates (Allocs, decayKey, decayPool, refund)
import           Delegation.StakePool
import           Keys
import           PParams
import           Slot
import           UTxO

import           Data.List               (groupBy, sort)
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust, isJust)
import           Data.Ratio
import qualified Data.Set                as Set

import           Numeric.Natural         (Natural)

import           Lens.Micro              ((^.))

-- | StakeShare type
newtype StakeShare =
  StakeShare Rational
  deriving (Show, Ord, Eq)

-- | Construct an optional probability value
mkStakeShare :: Rational -> Maybe StakeShare
mkStakeShare p =
  if 0 <= p && p <= 1
    then Just $ StakeShare p
    else Nothing

-- | Distribution density function
newtype Distr =
  Distr (Map.Map HashKey StakeShare)

newtype Production =
  Production (Map.Map HashKey Natural)

-- | Type of stake as pair of coins associated to a hash key.
newtype Stake =
  Stake (HashKey, Coin)
  deriving (Show, Eq, Ord)

-- | Extract hash of staking key from base address.
getStakeHK :: Addr -> Maybe HashKey
getStakeHK (AddrTxin _ hk) = Just hk
getStakeHK _               = Nothing

-- | Get Stake of base addresses in TxOut set.
baseStake :: Set.Set TxOut -> Set.Set Stake
baseStake outs =
  Set.fromList
    [ Stake (fromJust $ getStakeHK a, c)
    | TxOut a c <- Set.toList outs
    , isJust $ getStakeHK a
    ]

-- | Extract pointer from pointer address.
getStakePtr :: Addr -> Maybe Ptr
getStakePtr (AddrPtr ptr) = Just ptr
getStakePtr _             = Nothing

-- | Calculate stake of pointer addresses in TxOut set.
ptrStake :: Set.Set TxOut -> Map.Map Ptr HashKey -> Set.Set Stake
ptrStake outs pointers =
  Set.fromList
    [ Stake (fromJust $ Map.lookup (fromJust $ getStakePtr a) pointers, c)
    | TxOut a c <- Set.toList outs
    , isJust $ getStakePtr a
    , isJust $ Map.lookup (fromJust $ getStakePtr a) pointers
    ]

-- | Calculate stake of all addresses in TxOut set.
stake :: Set.Set TxOut -> Map.Map Ptr HashKey -> Set.Set Stake
stake outs pointers = baseStake outs `Set.union` ptrStake outs pointers

-- | Check whether a hash key has active stake, i.e., currently delegates to an
-- active stake pool.
isActive :: HashKey -> Allocs -> Map.Map HashKey HashKey -> Allocs -> Bool
isActive vSk stakeKeys delegs stakePools =
  vSk `Set.member` Map.keysSet stakeKeys &&
  isJust vp && fromJust vp `Set.member` Map.keysSet stakePools
  where
    vp = Map.lookup vSk delegs

-- | Calculate total active state in the form of a mapping of hash keys to
-- coins.
activeStake ::
     Set.Set TxOut
  -> Map.Map Ptr HashKey
  -> Allocs
  -> Map.Map HashKey HashKey
  -> Allocs
  -> Map.Map HashKey Coin
activeStake outs pointers stakeKeys delegs stakePools =
  Map.fromList $
  map (makePair . sumKey) $
  groupBy (\(Stake (a, _)) (Stake (a', _)) -> a == a') $
  sort $
  filter
    (\(Stake (hk, _)) -> isActive hk stakeKeys delegs stakePools)
    (Set.toList $ stake outs pointers)
  where
    sumKey =
      foldl1 (\(Stake (key, coin)) (Stake (_, c')) -> Stake (key, coin <> c'))
    makePair (Stake (k, c)) = (k, c)

-- | Calculate pool refunds
poolRefunds :: PParams -> Allocs -> Slot -> Map.Map HashKey Coin
poolRefunds pc retiring cslot =
  Map.map (\s -> refund pval pmin lambda (cslot -* s)) retiring
  where
    (pval, pmin, lambda) = decayPool pc

-- | Calculate total possible refunds.
obligation :: PParams -> Allocs -> Allocs -> Slot -> Coin
obligation pc stakeKeys stakePools cslot =
  sum (map (\s -> refund dval dmin lambdad (cslot -* s)) $ Map.elems stakeKeys) +
  sum (map (\s -> refund pval pmin lambdap (cslot -* s)) $ Map.elems stakePools)
  where
    (dval, dmin, lambdad) = decayKey pc
    (pval, pmin, lambdap) = decayPool pc

-- | Calculate maximal pool reward
maxPool :: PParams -> Coin -> Rational -> Rational -> Coin
maxPool pc (Coin r) sigma pR = floor $ factor1 * factor2
  where
    (a0, nOpt) = pc ^. poolConsts
    z0 = 1 % fromIntegral nOpt
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = fromIntegral r / (1 + a0)
    factor2 = sigma' + p' * a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | Calulcate moving average
movingAvg :: PParams -> HashKey -> Natural -> Rational -> Distr -> Rational
movingAvg pc hk n expectedSlots (Distr averages) =
  let fraction = fromIntegral n / max expectedSlots 1
   in case Map.lookup hk averages of
        Nothing -> fraction
        Just (StakeShare prev) -> alpha * fraction + (1 - alpha) * prev
          where alpha = intervalValue $ pc ^. movingAvgWeight

-- | Calculate pool reward
poolRew ::
     PParams
  -> HashKey
  -> Natural
  -> Rational
  -> Distr
  -> Coin
  -> (Coin, Rational)
poolRew pc hk n expectedSlots averages (Coin maxP) =
  (floor $ e * fromIntegral maxP, avg)
  where
    avg = intervalValue $ pc ^. movingAvgExp
    gamma = movingAvg pc hk n expectedSlots averages
    e = fromRational avg ** fromRational gamma :: Double

-- | Calculate pool leader reward
leaderRew :: Coin -> StakePool -> StakeShare -> StakeShare -> Coin
leaderRew f@(Coin f') pool (StakeShare sigma) (StakeShare s)
  | f' <= c = f
  | otherwise =
    floor $ fromIntegral (c + (f' - c)) * (m' + (1 - m') * sigma / s)
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Calculate pool member reward
memberRew :: Coin -> StakePool -> StakeShare -> StakeShare -> Coin
memberRew f@(Coin f') pool (StakeShare sigma) (StakeShare s)
  | f' <= c = 0
  | otherwise = floor $ fromIntegral (f' - c) * (1 - m') * sigma / s
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m
