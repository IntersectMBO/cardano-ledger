{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

{-|
Module      : EpochBoundary
Description : Functions and definitions for rules at epoch boundary.

This modules implements the necessary functions for the changes that can happen at epoch boundaries.
-}
module EpochBoundary
  ( Stake(..)
  , BlocksMade(..)
  , SnapShots(..)
  , pstakeMark
  , pstakeSet
  , pstakeGo
  , poolsSS
  , blocksSS
  , feeSS
  , emptySnapShots
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
import           Delegation.PoolParams   (RewardAcnt (..), PoolParams(..))
import           Keys
import           PParams
import           Slot
import           UTxO

import qualified Data.Map.Strict         as Map
import           Data.Maybe              (mapMaybe)
import           Data.Ratio
import qualified Data.Set                as Set

import           Numeric.Natural         (Natural)

import           Lens.Micro              ((^.))
import           Lens.Micro.TH           (makeLenses)

-- | Blocks made
newtype BlocksMade =
    BlocksMade (Map.Map HashKey Natural)
               deriving (Show, Eq)

-- | Type of stake as map from hash key to coins associated.
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
  Stake $ Map.fromListWith (+) (mapMaybe convert $ Map.toList vals)
 where
   convert :: (Addr, Coin) -> Maybe (HashKey, Coin)
   convert (a, c) =
     (,c) <$> getStakeHK a

-- | Extract pointer from pointer address.
getStakePtr :: Addr -> Maybe Ptr
getStakePtr (AddrPtr ptr) = Just ptr
getStakePtr _             = Nothing

-- | Calculate stake of pointer addresses in TxOut set.
ptrStake :: Map.Map Addr Coin -> Map.Map Ptr HashKey -> Stake
ptrStake vals pointers =
  Stake $ Map.fromListWith (+) (mapMaybe convert $ Map.toList vals)
  where
    convert :: (Addr, Coin) -> Maybe (HashKey, Coin)
    convert (a, c) =
      case getStakePtr a of
        Nothing -> Nothing
        Just s -> (,c) <$> Map.lookup s pointers

rewardStake :: Map.Map RewardAcnt Coin -> Stake
rewardStake rewards =
  Stake $
  Map.foldlWithKey
    (\m rewKey c -> Map.insert (getRwdHK rewKey) c m)
    Map.empty
    rewards

-- | Get stake of one pool
poolStake ::
     HashKey
  -> Map.Map HashKey HashKey
  -> Stake
  -> Stake
poolStake hk delegs (Stake stake) =
  Stake $ Map.restrictKeys stake (Map.keysSet restricted)
  where restricted = Map.filter (== hk) delegs

-- | Calculate pool refunds
poolRefunds :: PParams -> Map.Map HashKey Epoch -> Slot -> Map.Map HashKey Coin
poolRefunds pp retirees cslot =
  Map.map
    (\e ->
       refund pval pmin lambda (cslot -* slotFromEpoch e))
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

data SnapShots =
    SnapShots
    { _pstakeMark :: (Stake, Map.Map HashKey HashKey)
    , _pstakeSet  :: (Stake, Map.Map HashKey HashKey)
    , _pstakeGo   :: (Stake, Map.Map HashKey HashKey)
    , _poolsSS    :: Map.Map HashKey PoolParams
    , _blocksSS   :: BlocksMade
    , _feeSS      :: Coin
    } deriving (Show, Eq)

makeLenses ''SnapShots

emptySnapShots :: SnapShots
emptySnapShots =
    SnapShots snapEmpty snapEmpty snapEmpty Map.empty blocksEmpty (Coin 0)
    where pooledEmpty = Map.empty
          blocksEmpty = BlocksMade Map.empty
          stakeEmpty  = Stake Map.empty
          snapEmpty   = (stakeEmpty, pooledEmpty)
