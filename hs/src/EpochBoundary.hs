{-|
Module      : EpochBoundary
Description : Functions and definitions for rules at epoch boundary.

This modules implements the necessary functions for the changes that can happen at epoch boundaries.
-}
module EpochBoundary
  ( Stake
  , baseStake
  , ptrStake
  , stake
  , isActive
  , activeStake
  , obligation
  , poolRefunds
  , maxPool
  ) where

import           Coin
import           Delegation.Certificates (Allocs, decayKey, decayPool, refund)
import           Keys
import           PrtclConsts
import           Slot
import           UTxO

import           Data.List               (groupBy, sort)
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust, isJust)
import           Data.Ratio
import qualified Data.Set                as Set

import           Lens.Micro              ((^.))

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
poolRefunds :: PrtclConsts -> Allocs -> Slot -> Map.Map HashKey Coin
poolRefunds pc retiring cslot =
  Map.map (\s -> refund pval pmin lambda (cslot -* s)) retiring
  where
    (pval, pmin, lambda) = decayPool pc

-- | Calculate total possible refunds.
obligation :: PrtclConsts -> Allocs -> Allocs -> Slot -> Coin
obligation pc stakeKeys stakePools cslot =
  sum (map (\s -> refund dval dmin lambdad (cslot -* s)) $ Map.elems stakeKeys) +
  sum (map (\s -> refund pval pmin lambdap (cslot -* s)) $ Map.elems stakePools)
  where
    (dval, dmin, lambdad) = decayKey pc
    (pval, pmin, lambdap) = decayPool pc

-- | Calculate maximal pool reward
maxPool :: PrtclConsts -> Coin -> Rational -> Rational -> Coin
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
