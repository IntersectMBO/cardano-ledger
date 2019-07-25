{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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
import           Delegation.Certificates (StakeKeys (..), StakePools (..), decayKey, decayPool,
                     refund)
import           Keys
import           PParams hiding (a0, nOpt)
import           Slot
import           Tx
import           TxData
import           UTxO hiding (dom)

import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Ratio
import qualified Data.Set as Set

import           Numeric.Natural (Natural)

import           Lens.Micro.TH (makeLenses)

import           Ledger.Core (dom, (▷), (◁))

-- | Blocks made
newtype BlocksMade hashAlgo dsignAlgo
  = BlocksMade (Map.Map (KeyHash hashAlgo dsignAlgo) Natural)
  deriving (Show, Eq)

-- | Type of stake as map from hash key to coins associated.
newtype Stake hashAlgo dsignAlgo
  = Stake (Map.Map (StakeCredential hashAlgo dsignAlgo) Coin)
  deriving (Show, Eq, Ord)

-- | Extract hash of staking key from base address.
getStakeHK :: Addr hashAlgo dsignAlgo -> Maybe (StakeCredential hashAlgo dsignAlgo)
getStakeHK (AddrBase _ hk) = Just hk
getStakeHK _               = Nothing

consolidate :: UTxO hashAlgo dsignAlgo -> Map.Map (Addr hashAlgo dsignAlgo) Coin
consolidate (UTxO u) =
  Map.fromListWith (+) (map (\(_, TxOut a c) -> (a, c)) $ Map.toList u)

-- | Get Stake of base addresses in TxOut set.
baseStake :: Map.Map (Addr hashAlgo dsignAlgo) Coin -> Stake hashAlgo dsignAlgo
baseStake vals =
  Stake $ Map.fromListWith (+) (mapMaybe convert $ Map.toList vals)
 where
   convert
     :: (Addr hashAlgo dsignAlgo, Coin)
     -> Maybe (StakeCredential hashAlgo dsignAlgo, Coin)
   convert (a, c) =
     (,c) <$> getStakeHK a

-- | Extract pointer from pointer address.
getStakePtr :: Addr hashAlgo dsignAlgo -> Maybe Ptr
getStakePtr (AddrPtr ptr) = Just ptr
getStakePtr _             = Nothing

-- | Calculate stake of pointer addresses in TxOut set.
ptrStake
  :: forall hashAlgo dsignAlgo
   . Map.Map (Addr hashAlgo dsignAlgo) Coin
  -> Map.Map Ptr (StakeCredential hashAlgo dsignAlgo)
  -> Stake hashAlgo dsignAlgo
ptrStake vals pointers =
  Stake $ Map.fromListWith (+) (mapMaybe convert $ Map.toList vals)
  where
    convert
      :: (Addr hashAlgo dsignAlgo, Coin)
      -> Maybe (StakeCredential hashAlgo dsignAlgo, Coin)
    convert (a, c) =
      case getStakePtr a of
        Nothing -> Nothing
        Just s -> (,c) <$> Map.lookup s pointers

rewardStake
  :: Map.Map (RewardAcnt hashAlgo dsignAlgo) Coin
  -> Stake hashAlgo dsignAlgo
rewardStake rewards =
  Stake $
  Map.foldlWithKey
    (\m rewKey c -> Map.insert (getRwdHK rewKey) c m)
    Map.empty
    rewards

-- | Get stake of one pool
poolStake
  :: KeyHash hashAlgo dsignAlgo
  -> Map.Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
  -> Stake hashAlgo dsignAlgo
  -> Stake hashAlgo dsignAlgo
poolStake hk delegs (Stake stake) =
  Stake $ dom (delegs ▷ Set.singleton hk) ◁ stake

-- | Calculate pool refunds
poolRefunds
  :: PParams
  -> Map.Map (KeyHash hashAlgo dsignAlgo) Epoch
  -> Slot
  -> Map.Map (KeyHash hashAlgo dsignAlgo) Coin
poolRefunds pp retirees cslot =
  Map.map
    (\e ->
       refund pval pmin lambda (cslot -* slotFromEpoch e))
    retirees
  where
    (pval, pmin, lambda) = decayPool pp

-- | Calculate total possible refunds.
obligation
  :: PParams
  -> StakeKeys hashAlgo dsignAlgo
  -> StakePools hashAlgo dsignAlgo
  -> Slot
  -> Coin
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
    a0 = _a0 pc
    nOpt = _nOpt pc
    z0 = 1 % fromIntegral nOpt
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = fromIntegral r / (1 + a0)
    factor2 = sigma' + p' * a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | Pool individual reward
groupByPool
  :: Map.Map (KeyHash hashAlgo dsignAlgo) Coin
  -> Map.Map (KeyHash hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
  -> Map.Map
       (KeyHash hashAlgo dsignAlgo)
       (Map.Map (KeyHash hashAlgo dsignAlgo) Coin)
groupByPool active delegs =
  Map.fromListWith
    Map.union
    [ (delegs Map.! hk, Set.singleton hk ◁ active)
    | hk <- Map.keys delegs
    ]

data SnapShots hashAlgo dsignAlgo
  = SnapShots
    { _pstakeMark
      :: ( Stake hashAlgo dsignAlgo
         , Map.Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
         )
    , _pstakeSet
      :: ( Stake hashAlgo dsignAlgo
         , Map.Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
         )
    , _pstakeGo
      :: ( Stake hashAlgo dsignAlgo
         , Map.Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
         )
    , _poolsSS
      :: Map.Map (KeyHash hashAlgo dsignAlgo) (PoolParams hashAlgo dsignAlgo)
    , _blocksSS :: BlocksMade hashAlgo dsignAlgo
    , _feeSS :: Coin
    } deriving (Show, Eq)

makeLenses ''SnapShots

emptySnapShots :: SnapShots hashAlgo dsignAlgo
emptySnapShots =
    SnapShots snapEmpty snapEmpty snapEmpty Map.empty blocksEmpty (Coin 0)
    where pooledEmpty = Map.empty
          blocksEmpty = BlocksMade Map.empty
          stakeEmpty  = Stake Map.empty
          snapEmpty   = (stakeEmpty, pooledEmpty)
