{-|
Module      : EpochBoundary
Description : Functions and definitions for rules at epoch boundary.

This modules implements the necessary functions for the changes that can happen at epoch boundaries.
-}
module EpochBoundary
  ( Stake(..)
  , Production(..)
  , rewardStake
  , consolidate
  , baseStake
  , ptrStake
  , poolStake
  , obligation
  , poolRefunds
  , maxPool
  , groupByPool
  ) where

import           Coin
import           Delegation.Certificates (StakeKeys (..), StakePools (..),
                                          decayKey, decayPool, refund)
import           Delegation.PoolParams   (RewardAcnt (..))
import           Keys
import           PParams
import           Slot
import           UTxO

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
  Stake (Map.Map HashKey Coin)
  deriving (Show, Eq, Ord)

-- | Extract hash of staking key from base address.
getStakeHK :: Addr -> Maybe HashKey
getStakeHK (AddrTxin _ hk) = Just hk
getStakeHK _               = Nothing

consolidate :: UTxO -> Map.Map Addr Coin
consolidate (UTxO u) =
  Map.fromListWith (+) (map (\(_, TxOut a c) -> (a, c)) $ Map.toList u)

-- | Get Stake of base addresses in TxOut set.
baseStake :: Map.Map Addr Coin -> Stake
baseStake vals =
  Stake $
  Map.fromListWith
    (+)
    [(fromJust $ getStakeHK a, c) | (a, c) <- Map.toList vals, isJust $ getStakeHK a]

-- | Extract pointer from pointer address.
getStakePtr :: Addr -> Maybe Ptr
getStakePtr (AddrPtr ptr) = Just ptr
getStakePtr _             = Nothing

-- | Calculate stake of pointer addresses in TxOut set.
ptrStake :: Map.Map Addr Coin -> Map.Map Ptr HashKey -> Stake
ptrStake vals pointers =
  Stake $
  Map.fromListWith
    (+)
    [ (fromJust $ Map.lookup (fromJust $ getStakePtr a) pointers, c)
    | (a, c) <- Map.toList vals
    , isJust $ getStakePtr a
    , isJust $ Map.lookup (fromJust $ getStakePtr a) pointers
    ]

rewardStake :: Map.Map RewardAcnt Coin -> Stake
rewardStake rewards =
  Stake $
  Map.foldlWithKey
    (\m rewKey c -> Map.insert (getRwdHK rewKey) c m)
    Map.empty
    rewards

poolStake ::
     HashKey
  -> Set.Set HashKey
  -> Map.Map HashKey HashKey
  -> Stake
  -> Stake
poolStake operator owners delegations (Stake stake) =
    Stake $ Map.insert operator pstake (Map.withoutKeys poolStake' owners')
    where
      owners'    = Set.insert operator owners
      poolStake' = Map.mapMaybeWithKey (\k _ -> Map.lookup k stake) delegations
      pstake     = Map.foldl (+) (Coin 0) $ Map.restrictKeys poolStake' owners'


-- | Calculate pool refunds
poolRefunds :: PParams -> Map.Map HashKey Epoch -> Slot -> Map.Map HashKey Coin
poolRefunds pp retirees cslot =
  Map.map
    (\e ->
       refund pval pmin lambda (cslot -* slotFromEpoch e (pp ^. slotsPerEpoch)))
    retirees
  where
    (pval, pmin, lambda) = decayPool pp

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
