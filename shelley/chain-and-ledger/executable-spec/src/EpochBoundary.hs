{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , feeSS
  , emptySnapShots
  , rewardStake
  , aggregateOuts
  , baseStake
  , ptrStake
  , poolStake
  , obligation
  , poolRefunds
  , maxPool
  , groupByPool
  , (⊎)
  ) where

--import           Coin (Coin (..))
import           Delegation.Certificates (StakeCreds (..), StakePools (..), decayKey, decayPool,
                     refund)
import           Keys (KeyHash)
import           PParams (PParams (..))
import           Slot (SlotNo, (-*))
import           TxData (Addr (..), Credential, PoolParams, Ptr, RewardAcnt, TxOut (..),
                  addr, value, getRwdCred)
import           UTxO (UTxO (..))

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Map.Strict (Map, empty)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Ratio ((%))
import qualified Data.Set as Set

import           GHC.Generics (Generic)
import           Lens.Micro.TH (makeLenses)
import           Numeric.Natural (Natural)

import           Ledger.Core (dom, (▷), (◁))

import           Scripts

-- | Blocks made
newtype BlocksMade crypto
  = BlocksMade (Map (KeyHash crypto) Natural)
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks)

-- | Type of stake as map from hash key to values associated.
newtype Stake crypto
  = Stake (Map (Credential crypto) Coin)
  deriving (Show, Eq, Ord, ToCBOR, FromCBOR, NoUnexpectedThunks)

-- | Add two stake distributions
(⊎)
  :: Stake crypto
  -> Stake crypto
  -> Stake crypto
(Stake lhs) ⊎ (Stake rhs) = Stake $ Map.unionWith (+) lhs rhs

-- | Extract hash of staking key from base address.
getStakeHK :: Addr crypto -> Maybe (Credential crypto)
getStakeHK (AddrBase _ hk) = Just hk
getStakeHK _               = Nothing

aggregateOuts :: UTxO crypto -> Map (Addr crypto) (Value crypto)
aggregateOuts (UTxO u) =
  Map.fromListWith (+) (map (\(_, txout) -> (_addr txout, _value txout)) $ Map.toList u)

-- | Get Stake of base addresses in TxOut set.
baseStake
  :: Map (Addr crypto) (Value crypto)
  -> [(Credential crypto, Value crypto)]
baseStake vals =
  mapMaybe convert $ Map.toList vals
  where
   convert
     :: (Addr crypto, Value crypto)
     -> Maybe (Credential crypto, Value crypto)
   convert (a, c) =
     (,c) <$> getStakeHK a

-- | Extract pointer from pointer address.
getStakePtr :: Addr crypto -> Maybe Ptr
getStakePtr (AddrPtr _ ptr) = Just ptr
getStakePtr _               = Nothing

-- | Calculate stake of pointer addresses in TxOut set.
ptrStake
  :: forall crypto
   . Map (Addr crypto) (Value crypto)
  -> Map Ptr (Credential crypto)
  -> [(Credential crypto, Value crypto)]
ptrStake vals pointers =
  mapMaybe convert $ Map.toList vals
  where
    convert
      :: (Addr crypto, Value crypto)
      -> Maybe (Credential crypto, Value crypto)
    convert (a, c) =
      case getStakePtr a of
        Nothing -> Nothing
        Just s -> (,c) <$> Map.lookup s pointers

rewardStake
  :: forall crypto
   . Map (RewardAcnt crypto) (Value crypto)
  -> [(Credential crypto, Value crypto)]
rewardStake rewards =
  map convert $ Map.toList rewards
  where
    convert (rwdKey, c) = (getRwdCred rwdKey, c)

-- | Get stake of one pool
poolStake
  :: KeyHash crypto
  -> Map (Credential crypto) (KeyHash crypto)
  -> Stake crypto
  -> Stake crypto
poolStake hk delegs (Stake stake) =
  Stake $ dom (delegs ▷ Set.singleton hk) ◁ stake

-- | Calculate pool refunds
poolRefunds
  :: PParams crypto
  -> Map (KeyHash crypto) SlotNo
  -> SlotNo
  -> Map (KeyHash crypto) (Value crypto)
poolRefunds pp retirees cslot =
  Map.map
    (\s ->
       refund pval pmin lambda (cslot -* s))
    retirees
  where
    (pval, pmin, lambda) = decayPool pp

-- | Calculate total possible refunds.
obligation
  :: PParams crypto
  -> StakeCreds crypto
  -> StakePools crypto
  -> SlotNo
  -> (Value crypto)
obligation pc (StakeCreds stakeKeys) (StakePools stakePools) cslot =
  sum (map (\s -> refund dval dmin lambdad (cslot -* s)) $ Map.elems stakeKeys) +
  sum (map (\s -> refund pval pmin lambdap (cslot -* s)) $ Map.elems stakePools)
  where
    (dval, dmin, lambdad) = decayKey pc
    (pval, pmin, lambdap) = decayPool pc

-- | Calculate maximal pool reward
maxPool :: (PParams crypto) -> (Value crypto) -> Rational -> Rational -> (Value crypto)
maxPool pc (Value r) sigma pR = floor $ factor1 * factor2
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
  :: Map (KeyHash crypto) (Value crypto)
  -> Map (KeyHash crypto) (KeyHash crypto)
  -> Map
       (KeyHash crypto)
       (Map (KeyHash crypto) (Value crypto))
groupByPool active delegs =
  Map.fromListWith
    Map.union
    [ (delegs Map.! hk, Set.singleton hk ◁ active)
    | hk <- Map.keys delegs
    ]

-- | Snapshot of the stake distribution.
data SnapShots crypto
  = SnapShots
    { _pstakeMark
      :: ( Stake crypto
         , Map (Credential crypto) (KeyHash crypto)
         )
    , _pstakeSet
      :: ( Stake crypto
         , Map (Credential crypto) (KeyHash crypto)
         )
    , _pstakeGo
      :: ( Stake crypto
         , Map (Credential crypto) (KeyHash crypto)
         )
    , _poolsSS
      :: Map (KeyHash crypto) (PoolParams crypto)
    , _feeSS :: Value crypto
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (SnapShots crypto)

instance
  Crypto crypto
  => ToCBOR (SnapShots crypto)
  where
    toCBOR (SnapShots mark set go ps fs) =
      encodeListLen 5
      <> toCBOR mark
      <> toCBOR set
      <> toCBOR go
      <> toCBOR ps
      <> toCBOR fs

instance
  Crypto crypto
  => FromCBOR (SnapShots crypto)
  where
    fromCBOR = do
      enforceSize "SnapShots" 5
      mark <- fromCBOR
      set <- fromCBOR
      go <- fromCBOR
      ps <- fromCBOR
      f <- fromCBOR
      pure $ SnapShots mark set go ps f

makeLenses ''SnapShots

emptySnapShots :: SnapShots crypto
emptySnapShots =
    SnapShots snapEmpty snapEmpty snapEmpty Map.empty (Value empty)
    where pooledEmpty = Map.empty
          stakeEmpty  = Stake Map.empty
          snapEmpty   = (stakeEmpty, pooledEmpty)
