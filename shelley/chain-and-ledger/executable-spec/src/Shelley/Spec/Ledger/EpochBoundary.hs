{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    aggregateUtxoCoinByCredential,
    poolStake,
    obligation,
    maxPool,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.Constraints (UsesValue)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era
import Cardano.Ledger.Val ((<+>), (<×>))
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Control.SetAlgebra (dom, eval, setSingleton, (▷), (◁))
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.Coin
  ( Coin (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Shelley.Spec.Ledger.Credential (Credential, Ptr, StakeReference (..))
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), _a0, _nOpt)
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.TxBody (PoolParams, TxOut (TxOut))
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- | Blocks made
newtype BlocksMade crypto = BlocksMade
  { unBlocksMade :: Map (KeyHash 'StakePool crypto) Natural
  }
  deriving (Eq, NoThunks, Generic, NFData)
  deriving (Show) via Quiet (BlocksMade crypto)

deriving instance (CC.Crypto crypto) => ToJSON (BlocksMade crypto)

deriving instance (CC.Crypto crypto) => FromJSON (BlocksMade crypto)

deriving instance CC.Crypto crypto => ToCBOR (BlocksMade crypto)

deriving instance CC.Crypto crypto => FromCBOR (BlocksMade crypto)

-- | Type of stake as map from hash key to coins associated.
newtype Stake crypto = Stake
  { unStake :: Map (Credential 'Staking crypto) Coin
  }
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving newtype instance
  CC.Crypto crypto => ToCBOR (Stake crypto)

deriving newtype instance
  CC.Crypto crypto => FromCBOR (Stake crypto)

-- A TxOut has 4 different shapes, depending on the shape its embedded of Addr.
-- Credentials are stored in only 2 of the 4 cases.
-- 1) TxOut (Addr _ _ (StakeRefBase cred)) coin   -> HERE
-- 2) TxOut (Addr _ _ (StakeRefPtr ptr)) coin     -> HERE
-- 3) TxOut (Addr _ _ StakeRefNull) coin          -> NOT HERE
-- 4) TxOut (AddrBootstrap _) coin                -> NOT HERE
-- Unfortunately TxOut is a pattern, that deserializes the address. This can be expensive, so if
-- we only deserialize the parts that we need, for the 2 cases that count, we can speed
-- things up considerably. That is the role of deserialiseAddrStakeRef. It returns (Just stake)
-- for the two cases that matter, and Nothing for the other two cases.

-- | Sum up all the Coin for each staking Credential
aggregateUtxoCoinByCredential ::
  forall era.
  UsesValue era =>
  Map Ptr (Credential 'Staking (Crypto era)) ->
  UTxO era ->
  Map (Credential 'Staking (Crypto era)) Coin ->
  Map (Credential 'Staking (Crypto era)) Coin
aggregateUtxoCoinByCredential ptrs (UTxO u) initial =
  Map.foldr accum initial u
  where
    accum (TxOut (Addr _ _ (StakeRefPtr p)) c) ans =
      case Map.lookup p ptrs of
        Just cred -> Map.insertWith (<>) cred (Val.coin c) ans
        Nothing -> ans
    accum (TxOut (Addr _ _ (StakeRefBase hk)) c) ans =
      Map.insertWith (<>) hk (Val.coin c) ans
    accum _other ans = ans

-- | Get stake of one pool
poolStake ::
  KeyHash 'StakePool crypto ->
  Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto) ->
  Stake crypto ->
  Stake crypto
poolStake hk delegs (Stake stake) =
  Stake $ eval (dom (delegs ▷ setSingleton hk) ◁ stake)

-- | Calculate total possible refunds.
obligation ::
  PParams era ->
  Map (Credential 'Staking (Crypto era)) Coin ->
  Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era)) ->
  Coin
obligation pp rewards stakePools =
  (length rewards <×> _keyDeposit pp) <+> (length stakePools <×> _poolDeposit pp)

-- | Calculate maximal pool reward
maxPool :: PParams era -> Coin -> Rational -> Rational -> Coin
maxPool pc r sigma pR = rationalToCoinViaFloor $ factor1 * factor2
  where
    a0 = _a0 pc
    nOpt = _nOpt pc
    z0 = 1 % fromIntegral nOpt
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = coinToRational r / (1 + a0)
    factor2 = sigma' + p' * a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | Snapshot of the stake distribution.
data SnapShot crypto = SnapShot
  { _stake :: !(Stake crypto),
    _delegations :: !(Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    _poolParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto))
  }
  deriving (Show, Eq, Generic)

instance NoThunks (SnapShot crypto)

instance NFData (SnapShot crypto)

instance
  CC.Crypto crypto =>
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
  CC.Crypto crypto =>
  FromCBOR (SnapShot crypto)
  where
  fromCBOR = do
    decodeRecordNamed "SnapShot" (const 3) $ do
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

instance NoThunks (SnapShots crypto)

instance NFData (SnapShots crypto)

instance
  CC.Crypto crypto =>
  ToCBOR (SnapShots crypto)
  where
  toCBOR (SnapShots mark set go fs) =
    encodeListLen 4
      <> toCBOR mark
      <> toCBOR set
      <> toCBOR go
      <> toCBOR fs

instance
  CC.Crypto crypto =>
  FromCBOR (SnapShots crypto)
  where
  fromCBOR = do
    decodeRecordNamed "SnapShots" (const 4) $ do
      mark <- fromCBOR
      set <- fromCBOR
      go <- fromCBOR
      f <- fromCBOR
      pure $ SnapShots mark set go f

emptySnapShot :: SnapShot crypto
emptySnapShot = SnapShot (Stake Map.empty) Map.empty Map.empty

emptySnapShots :: SnapShots crypto
emptySnapShots = SnapShots emptySnapShot emptySnapShot emptySnapShot (Coin 0)
