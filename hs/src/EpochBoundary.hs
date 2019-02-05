{-|
Module      : EpochBoundary
Description : Functions and definitions for rules at epoch boundary.

This modules implements the necessary functions for the changes that can happen at epoch boundaries.
-}
module EpochBoundary
  ( Stake
  , Production(..)
  , baseStake
  , ptrStake
  , stake
  , isActive
  , activeStake
  , obligation
  , poolRefunds
  , maxPool
  , groupByPool
  ) where

import           Coin
import           Delegation.Certificates (StakeKeys(..), StakePools(..),
                                          decayKey, decayPool, refund)
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
baseStake :: [TxOut] -> Set.Set Stake
baseStake outs =
  Set.fromList $ map Stake $ Map.toList $
  Map.fromListWith (+)
    [ (fromJust $ getStakeHK a, c)
    | TxOut a c <- outs
    , isJust $ getStakeHK a
    ]

-- | Extract pointer from pointer address.
getStakePtr :: Addr -> Maybe Ptr
getStakePtr (AddrPtr ptr) = Just ptr
getStakePtr _             = Nothing

-- | Calculate stake of pointer addresses in TxOut set.
ptrStake :: [TxOut] -> Map.Map Ptr HashKey -> Set.Set Stake
ptrStake outs pointers =
  Set.fromList $ map Stake $ Map.toList $
  Map.fromListWith (+)
    [ (fromJust $ Map.lookup (fromJust $ getStakePtr a) pointers, c)
    | TxOut a c <- outs
    , isJust $ getStakePtr a
    , isJust $ Map.lookup (fromJust $ getStakePtr a) pointers
    ]

-- | Calculate stake of all addresses in TxOut set.
stake :: [TxOut] -> Map.Map Ptr HashKey -> Set.Set Stake
stake outs pointers = baseStake outs `Set.union` ptrStake outs pointers

-- | Check whether a hash key has active stake, i.e., currently delegates to an
-- active stake pool.
isActive :: HashKey -> StakeKeys -> Map.Map HashKey HashKey -> StakePools -> Bool
isActive vSk (StakeKeys stakeKeys) delegs (StakePools stakePools) =
  vSk `Set.member` Map.keysSet stakeKeys && isJust vp && fromJust vp `Set.member`
  Map.keysSet stakePools
  where
    vp = Map.lookup vSk delegs

-- | Calculate total active state in the form of a mapping of hash keys to
-- coins.
activeStake ::
     [TxOut]
  -> Map.Map Ptr HashKey
  -> StakeKeys
  -> Map.Map HashKey HashKey
  -> StakePools
  -> Map.Map HashKey Coin
activeStake outs pointers stakeKeys delegs stakePools =
  Map.fromList $ map (makePair . sumKey) $
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
poolRefunds :: PParams -> Map.Map HashKey Epoch -> Slot -> Map.Map HashKey Coin
poolRefunds pc retirees cslot =
  Map.map (\e -> refund pval pmin lambda (cslot -* slotFromEpoch e)) retirees
  where
    (pval, pmin, lambda) = decayPool pc

-- | Calculate total possible refunds.
obligation :: PParams -> StakeKeys -> StakePools -> Slot -> Coin
obligation pc (StakeKeys stakeKeys) (StakePools stakePools) cslot =
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

-- | Pool individual reward
groupByPool ::
     Map.Map HashKey Coin
  -> Map.Map HashKey HashKey
  -> Map.Map HashKey (Map.Map HashKey Coin)
groupByPool active delegs =
  Map.fromListWith
    Map.union
    [ (delegs Map.! hk, Map.restrictKeys active (Set.singleton hk))
    | hk <- Map.keys delegs
    ]
