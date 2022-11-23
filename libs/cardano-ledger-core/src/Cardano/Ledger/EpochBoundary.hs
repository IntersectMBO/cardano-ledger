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
module Cardano.Ledger.EpochBoundary
  ( Stake (..),
    sumAllStake,
    sumStakePerPool,
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

import Cardano.Ledger.BaseTypes (BoundedRational (..), NonNegativeInterval)
import Cardano.Ledger.Binary
  ( FromCBOR (fromCBOR),
    FromSharedCBOR (..),
    Interns,
    ToCBOR (toCBOR),
    decodeRecordNamedT,
    encodeListLen,
    fromSharedPlusLensCBOR,
    toMemptyLens,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    CompactForm (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import Control.Monad.Trans (lift)
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Typeable
import Data.VMap as VMap
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import Lens.Micro (_1, _2)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | Type of stake as map from hash key to coins associated.
newtype Stake c = Stake
  { unStake :: VMap VB VP (Credential 'Staking c) (CompactForm Coin)
  }
  deriving (Show, Eq, NFData, Generic)

deriving newtype instance Typeable c => NoThunks (Stake c)

deriving newtype instance
  CC.Crypto c => ToCBOR (Stake c)

instance CC.Crypto c => FromSharedCBOR (Stake c) where
  type Share (Stake c) = Share (VMap VB VP (Credential 'Staking c) (CompactForm Coin))
  getShare = getShare . unStake
  fromSharedCBOR = fmap Stake . fromSharedCBOR

sumAllStake :: Stake c -> Coin
sumAllStake = fromCompact . CompactCoin . VMap.foldl (\acc (CompactCoin c) -> acc + c) 0 . unStake
{-# INLINE sumAllStake #-}

-- | Get stake of one pool
poolStake ::
  KeyHash 'StakePool c ->
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Stake c ->
  Stake c
poolStake hk delegs (Stake stake) =
  -- Stake $ (eval (dom (delegs ▷ setSingleton hk) ◁ stake))
  Stake $ VMap.filter (\cred _ -> VMap.lookup cred delegs == Just hk) stake

sumStakePerPool ::
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Stake c ->
  Map (KeyHash 'StakePool c) Coin
sumStakePerPool delegs (Stake stake) = VMap.foldlWithKey accum Map.empty stake
  where
    accum !acc cred compactCoin =
      case VMap.lookup cred delegs of
        Nothing -> acc
        Just kh -> Map.insertWith (<+>) kh (fromCompact compactCoin) acc

-- | Calculate total possible refunds.
obligation ::
  forall c pp t.
  ( HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin,
    Foldable (t (Credential 'Staking c))
  ) =>
  pp ->
  t (Credential 'Staking c) Coin ->
  Map (KeyHash 'StakePool c) (PoolParams c) ->
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
data SnapShot c = SnapShot
  { ssStake :: !(Stake c),
    ssDelegations :: !(VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c)),
    ssPoolParams :: !(VMap VB VB (KeyHash 'StakePool c) (PoolParams c))
  }
  deriving (Show, Eq, Generic)

instance Typeable c => NoThunks (SnapShot c)

instance NFData (SnapShot c)

instance
  CC.Crypto c =>
  ToCBOR (SnapShot c)
  where
  toCBOR
    SnapShot
      { ssStake = s,
        ssDelegations = d,
        ssPoolParams = p
      } =
      encodeListLen 3
        <> toCBOR s
        <> toCBOR d
        <> toCBOR p

instance CC.Crypto c => FromSharedCBOR (SnapShot c) where
  type
    Share (SnapShot c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  fromSharedPlusCBOR = decodeRecordNamedT "SnapShot" (const 3) $ do
    ssStake <- fromSharedPlusLensCBOR _1
    ssDelegations <- fromSharedPlusCBOR
    ssPoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 _2)
    pure SnapShot {ssStake, ssDelegations, ssPoolParams}

-- | Snapshots of the stake distribution.
data SnapShots c = SnapShots
  { ssStakeMark :: SnapShot c, -- Lazy on purpose
    ssStakeSet :: !(SnapShot c),
    ssStakeGo :: !(SnapShot c),
    ssFee :: !Coin
  }
  deriving (Show, Eq, Generic)

instance Typeable c => NoThunks (SnapShots c)

instance NFData (SnapShots c)

instance
  CC.Crypto c =>
  ToCBOR (SnapShots c)
  where
  toCBOR (SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}) =
    encodeListLen 4
      <> toCBOR ssStakeMark
      <> toCBOR ssStakeSet
      <> toCBOR ssStakeGo
      <> toCBOR ssFee

instance CC.Crypto c => FromSharedCBOR (SnapShots c) where
  type Share (SnapShots c) = Share (SnapShot c)
  fromSharedPlusCBOR = decodeRecordNamedT "SnapShots" (const 4) $ do
    !ssStakeMark <- fromSharedPlusCBOR
    ssStakeSet <- fromSharedPlusCBOR
    ssStakeGo <- fromSharedPlusCBOR
    ssFee <- lift fromCBOR
    pure SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}

instance Default (SnapShots c) where
  def = emptySnapShots

emptySnapShot :: SnapShot c
emptySnapShot = SnapShot (Stake VMap.empty) VMap.empty VMap.empty

emptySnapShots :: SnapShots c
emptySnapShots = SnapShots emptySnapShot emptySnapShot emptySnapShot (Coin 0)
