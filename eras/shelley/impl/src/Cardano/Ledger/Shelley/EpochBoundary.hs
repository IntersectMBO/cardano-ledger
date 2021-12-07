{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : EpochBoundary
-- Description : Functions and definitions for rules at epoch boundary.
--
-- This modules implements the necessary functions for the changes that can happen at epoch boundaries.
module Cardano.Ledger.Shelley.EpochBoundary
  ( Stake (..),
    sumAllStake,
    SnapShot (..),
    SnapShots (..),
    emptySnapShot,
    emptySnapShots,
    poolStake,
    obligation,
    maxPool,
    maxPool',
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.BaseTypes (BoundedRational (..), NonNegativeInterval)
import Cardano.Ledger.Coin
  ( Coin (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Serialization (decodeRecordNamedT)
import Cardano.Ledger.Shelley.TxBody (PoolParams)
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import Control.Monad.Trans (lift)
import Control.SetAlgebra (dom, eval, setSingleton, (▷), (◁))
import Data.Compact.VMap as VMap
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import Data.Ratio ((%))
import Data.Sharing
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import Lens.Micro (_1, _2)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | Type of stake as map from hash key to coins associated.
newtype Stake crypto = Stake
  { unStake :: VMap VB VP (Credential 'Staking crypto) (CompactForm Coin)
  }
  deriving (Show, Eq, NFData, Generic)

deriving newtype instance Typeable crypto => NoThunks (Stake crypto)

deriving newtype instance
  CC.Crypto crypto => ToCBOR (Stake crypto)

instance CC.Crypto crypto => FromSharedCBOR (Stake crypto) where
  type Share (Stake crypto) = Share (VMap VB VP (Credential 'Staking crypto) (CompactForm Coin))
  getShare = getShare . unStake
  fromSharedCBOR = fmap Stake . fromSharedCBOR

sumAllStake :: Stake crypto -> Coin
sumAllStake = VMap.foldMap fromCompact . unStake

-- | Get stake of one pool
poolStake ::
  KeyHash 'StakePool crypto ->
  VMap VB VB (Credential 'Staking crypto) (KeyHash 'StakePool crypto) ->
  Stake crypto ->
  Stake crypto
poolStake hk delegs (Stake stake) =
  Stake $ fromMap (eval (dom (toMap delegs ▷ setSingleton hk) ◁ toMap stake))

-- | Calculate total possible refunds.
obligation ::
  forall crypto pp.
  (HasField "_keyDeposit" pp Coin, HasField "_poolDeposit" pp Coin) =>
  pp ->
  Map (Credential 'Staking crypto) Coin ->
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  Coin
obligation pp rewards stakePools =
  (length rewards <×> getField @"_keyDeposit" pp)
    <+> (length stakePools <×> getField @"_poolDeposit" pp)

-- | Calculate maximal pool reward
maxPool' ::
  NonNegativeInterval ->
  Natural ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool' a0 nOpt r sigma pR = rationalToCoinViaFloor $ factor1 * factor2
  where
    z0 = 1 % fromIntegral nOpt
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = coinToRational r / (1 + unboundRational a0)
    factor2 = sigma' + p' * unboundRational a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | Version of maxPool' that extracts a0 and nOpt from a PParam with the right HasField instances
maxPool ::
  (HasField "_a0" pp NonNegativeInterval, HasField "_nOpt" pp Natural) =>
  pp ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool pc r sigma pR = maxPool' a0 nOpt r sigma pR
  where
    a0 = getField @"_a0" pc
    nOpt = getField @"_nOpt" pc

-- | Snapshot of the stake distribution.
data SnapShot crypto = SnapShot
  { _stake :: !(Stake crypto),
    _delegations :: !(VMap VB VB (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    _poolParams :: !(VMap VB VB (KeyHash 'StakePool crypto) (PoolParams crypto))
  }
  deriving (Show, Eq, Generic)

instance Typeable crypto => NoThunks (SnapShot crypto)

instance NFData (SnapShot crypto)

instance
  CC.Crypto crypto =>
  ToCBOR (SnapShot crypto)
  where
  toCBOR
    SnapShot
      { _stake = s,
        _delegations = d,
        _poolParams = p
      } =
      encodeListLen 3
        <> toCBOR s
        <> toCBOR d
        <> toCBOR p

instance CC.Crypto crypto => FromSharedCBOR (SnapShot crypto) where
  type
    Share (SnapShot crypto) =
      (Interns (Credential 'Staking crypto), Interns (KeyHash 'StakePool crypto))
  fromSharedPlusCBOR =
    decodeRecordNamedT "SnapShot" (const 3) $ do
      _stake <- fromSharedPlusLensCBOR _1
      _delegations <- fromSharedPlusCBOR
      _poolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 _2)
      pure SnapShot {_stake, _delegations, _poolParams}

-- | Snapshots of the stake distribution.
data SnapShots crypto = SnapShots
  { _pstakeMark :: SnapShot crypto, -- Lazy on purpose
    _pstakeSet :: !(SnapShot crypto),
    _pstakeGo :: !(SnapShot crypto),
    _feeSS :: !Coin
  }
  deriving (Show, Eq, Generic)

instance Typeable crypto => NoThunks (SnapShots crypto)

instance NFData (SnapShots crypto)

instance
  CC.Crypto crypto =>
  ToCBOR (SnapShots crypto)
  where
  toCBOR (SnapShots {_pstakeMark, _pstakeSet, _pstakeGo, _feeSS}) =
    encodeListLen 4
      <> toCBOR _pstakeMark
      <> toCBOR _pstakeSet
      <> toCBOR _pstakeGo
      <> toCBOR _feeSS

instance CC.Crypto crypto => FromSharedCBOR (SnapShots crypto) where
  type Share (SnapShots crypto) = Share (SnapShot crypto)
  fromSharedPlusCBOR =
    decodeRecordNamedT "SnapShots" (const 4) $ do
      !_pstakeMark <- fromSharedPlusCBOR
      _pstakeSet <- fromSharedPlusCBOR
      _pstakeGo <- fromSharedPlusCBOR
      _feeSS <- lift fromCBOR
      pure SnapShots {_pstakeMark, _pstakeSet, _pstakeGo, _feeSS}

instance Default (SnapShots crypto) where
  def = emptySnapShots

emptySnapShot :: SnapShot crypto
emptySnapShot = SnapShot (Stake VMap.empty) VMap.empty VMap.empty

emptySnapShots :: SnapShots crypto
emptySnapShots = SnapShots emptySnapShot emptySnapShot emptySnapShot (Coin 0)
