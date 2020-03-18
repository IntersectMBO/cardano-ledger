{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : EpochBoundary
-- Description : Functions and definitions for rules at epoch boundary.
--
-- This modules implements the necessary functions for the changes that can happen at epoch boundaries.
module Shelley.Spec.Ledger.EpochBoundary
  ( Stake (..),
    BlocksMade (..),
    SnapShot (..),
    SnapShots (..),
    emptySnapShot,
    emptySnapShots,
    rewardStake,
    aggregateOuts,
    baseStake,
    ptrStake,
    poolStake,
    obligation,
    maxPool,
    groupByPool,
    (⊎),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Core (dom, (▷), (◁))
import Shelley.Spec.Ledger.Credential (Credential, Ptr, StakeReference (..))
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates
  ( StakeCreds (..),
    StakePools (..),
  )
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), _a0, _nOpt)
import Shelley.Spec.Ledger.TxData (PoolParams, RewardAcnt, TxOut (..), getRwdCred)
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- | Blocks made
newtype BlocksMade crypto
  = BlocksMade (Map (KeyHash 'StakePool crypto) Natural)
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks, Generic, NFData)

-- | Type of stake as map from hash key to coins associated.
newtype Stake crypto = Stake {unStake :: (Map (Credential 'Staking crypto) Coin)}
  deriving (Show, Eq, Ord, ToCBOR, FromCBOR, NoUnexpectedThunks, NFData)

-- | Add two stake distributions
(⊎) ::
  Stake crypto ->
  Stake crypto ->
  Stake crypto
(Stake lhs) ⊎ (Stake rhs) = Stake $ Map.unionWith (+) lhs rhs

-- | Extract hash of staking key from base address.
getStakeHK :: Addr crypto -> Maybe (Credential 'Staking crypto)
getStakeHK (Addr _ _ (StakeRefBase hk)) = Just hk
getStakeHK _ = Nothing

-- | Add up only the Ada with the getCoin function
aggregateOuts :: Crypto crypto => UTxO crypto -> Map (Addr crypto) Coin
aggregateOuts (UTxO u) =
  Map.fromListWith (+) (map (\(_, ot) -> (getAddress ot, getCoin ot)) $ Map.toList u)

-- | Get Stake of base addresses in TxOut set.
baseStake ::
  Map (Addr crypto) Coin ->
  [(Credential 'Staking crypto, Coin)]
baseStake vals =
  mapMaybe convert $ Map.toList vals
  where
    convert ::
      (Addr crypto, Coin) ->
      Maybe (Credential 'Staking crypto, Coin)
    convert (a, c) =
      (,c) <$> getStakeHK a

-- | Extract pointer from pointer address.
getStakePtr :: Addr crypto -> Maybe Ptr
getStakePtr (Addr _ _ (StakeRefPtr ptr)) = Just ptr
getStakePtr _ = Nothing

-- | Calculate stake of pointer addresses in TxOut set.
ptrStake ::
  forall crypto.
  Map (Addr crypto) Coin ->
  Map Ptr (Credential 'Staking crypto) ->
  [(Credential 'Staking crypto, Coin)]
ptrStake vals pointers =
  mapMaybe convert $ Map.toList vals
  where
    convert ::
      (Addr crypto, Coin) ->
      Maybe (Credential 'Staking crypto, Coin)
    convert (a, c) =
      case getStakePtr a of
        Nothing -> Nothing
        Just s -> (,c) <$> Map.lookup s pointers

rewardStake ::
  forall crypto.
  Map (RewardAcnt crypto) Coin ->
  [(Credential 'Staking crypto, Coin)]
rewardStake rewards =
  map convert $ Map.toList rewards
  where
    convert (rwdKey, c) = (getRwdCred rwdKey, c)

-- | Get stake of one pool
poolStake ::
  KeyHash 'StakePool crypto ->
  Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto) ->
  Stake crypto ->
  Stake crypto
poolStake hk delegs (Stake stake) =
  Stake $ dom (delegs ▷ Set.singleton hk) ◁ stake

-- | Calculate total possible refunds.
obligation ::
  PParams ->
  StakeCreds crypto ->
  StakePools crypto ->
  Coin
obligation pp (StakeCreds stakeKeys) (StakePools stakePools) =
  (_keyDeposit pp) * (fromIntegral $ length stakeKeys)
    + (_poolDeposit pp) * (fromIntegral $ length stakePools)

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
groupByPool ::
  Map (KeyHash 'Staking crypto) Coin ->
  Map (KeyHash 'Staking crypto) (KeyHash 'StakePool crypto) ->
  Map
    (KeyHash 'StakePool crypto)
    (Map (KeyHash 'Staking crypto) Coin)
groupByPool active delegs =
  Map.fromListWith
    Map.union
    [ (pool, Set.singleton hk ◁ active)
      | (hk, pool) <- Map.toList delegs
    ]

-- | Snapshot of the stake distribution.
data SnapShot crypto = SnapShot
  { _stake :: !(Stake crypto),
    _delegations :: !(Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    _poolParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto))
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (SnapShot crypto)

instance NFData (SnapShot crypto)

instance
  Crypto crypto =>
  ToCBOR (SnapShot crypto)
  where
  toCBOR
    ( SnapShot
        { _stake = s,
          _delegations = d,
          _poolParams = p
        }
      ) =
      encodeListLen 3
        <> toCBOR s
        <> toCBOR d
        <> toCBOR p

instance
  Crypto crypto =>
  FromCBOR (SnapShot crypto)
  where
  fromCBOR = do
    enforceSize "SnapShot" 3
    s <- fromCBOR
    d <- fromCBOR
    p <- fromCBOR
    pure $ SnapShot s d p

-- | Snapshots of the stake distribution.
data SnapShots crypto = SnapShots
  { _pstakeMark :: !(SnapShot crypto),
    _pstakeSet :: !(SnapShot crypto),
    _pstakeGo :: !(SnapShot crypto),
    _feeSS :: !Coin
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (SnapShots crypto)

instance NFData (SnapShots crypto)

instance
  Crypto crypto =>
  ToCBOR (SnapShots crypto)
  where
  toCBOR (SnapShots mark set go fs) =
    encodeListLen 4
      <> toCBOR mark
      <> toCBOR set
      <> toCBOR go
      <> toCBOR fs

instance
  Crypto crypto =>
  FromCBOR (SnapShots crypto)
  where
  fromCBOR = do
    enforceSize "SnapShots" 4
    mark <- fromCBOR
    set <- fromCBOR
    go <- fromCBOR
    f <- fromCBOR
    pure $ SnapShots mark set go f

emptySnapShot :: SnapShot crypto
emptySnapShot = SnapShot (Stake Map.empty) Map.empty Map.empty

emptySnapShots :: SnapShots crypto
emptySnapShots = SnapShots emptySnapShot emptySnapShot emptySnapShot (Coin 0)
