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
module Cardano.Ledger.EpochBoundary (
  Stake (..),
  sumAllStake,
  sumStakePerPool,
  SnapShot (..),
  SnapShots (..),
  emptySnapShot,
  emptySnapShots,
  poolStake,
  maxPool,
  maxPool',
  calculatePoolDistr,
  calculatePoolDistr',
  calculatePoolStake,
)
where

import Cardano.Ledger.BaseTypes (BoundedRational (..), NonNegativeInterval)
import Cardano.Ledger.Binary (
  FromCBOR (fromCBOR),
  FromSharedCBOR (..),
  Interns,
  ToCBOR (toCBOR),
  decodeRecordNamedT,
  encodeListLen,
  fromSharedPlusLensCBOR,
  toMemptyLens,
 )
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (..),
  coinToRational,
  rationalToCoinViaFloor,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (ppVrf))
import Cardano.Ledger.TreeDiff (ToExpr)
import Cardano.Ledger.Val ((<+>))
import Control.DeepSeq (NFData)
import Control.Monad.Trans (lift)
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Typeable
import Data.VMap as VMap
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Lens.Micro ((^.), _1, _2)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
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

-- | Version of `maxPool'` that extracts `ppA0L` and `ppNOptL` from a `PParams`
maxPool ::
  EraPParams era =>
  PParams era ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool pp r sigma pR = maxPool' a0 nOpt r sigma pR
  where
    a0 = pp ^. ppA0L
    nOpt = pp ^. ppNOptL

-- | Snapshot of the stake distribution.
data SnapShot c = SnapShot
  { ssStake :: !(Stake c)
  , ssDelegations :: !(VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c))
  , ssPoolParams :: !(VMap VB VB (KeyHash 'StakePool c) (PoolParams c))
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
      { ssStake = s
      , ssDelegations = d
      , ssPoolParams = p
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
--
-- Note that ssStakeMark and ssStakeMarkPoolDistr are lazy on
-- purpose since we only want to force the thunk after one stability window
-- when we know that they are stable (so that we do not compute them if we do not have to).
-- See more info in the [Optimize TICKF ADR](https://github.com/input-output-hk/cardano-ledger/blob/master/docs/adr/2022-12-12_007-optimize-ledger-view.md)
data SnapShots c = SnapShots
  { ssStakeMark :: SnapShot c -- Lazy on purpose
  , ssStakeMarkPoolDistr :: PoolDistr c -- Lazy on purpose
  , ssStakeSet :: !(SnapShot c)
  , ssStakeGo :: !(SnapShot c)
  , ssFee :: !Coin
  }
  deriving (Show, Eq, Generic)
  deriving (NoThunks) via AllowThunksIn '["ssStakeMark", "ssStakeMarkPoolDistr"] (SnapShots c)

instance NFData (SnapShots c)

instance
  CC.Crypto c =>
  ToCBOR (SnapShots c)
  where
  toCBOR (SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}) =
    encodeListLen 4
      <> toCBOR ssStakeMark
      -- We intentionaly do not serialize the redundant ssStakeMarkPoolDistr
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
    let ssStakeMarkPoolDistr = calculatePoolDistr ssStakeMark
    pure SnapShots {ssStakeMark, ssStakeMarkPoolDistr, ssStakeSet, ssStakeGo, ssFee}

instance Default (SnapShots c) where
  def = emptySnapShots

emptySnapShot :: SnapShot c
emptySnapShot = SnapShot (Stake VMap.empty) VMap.empty VMap.empty

emptySnapShots :: SnapShots c
emptySnapShots = SnapShots emptySnapShot (calculatePoolDistr emptySnapShot) emptySnapShot emptySnapShot (Coin 0)

-- =======================================

instance ToExpr (SnapShots c)

instance ToExpr (SnapShot c)

instance ToExpr (Stake c)

-- | Sum up the Coin (as CompactForm Coin = Word64) for each StakePool
calculatePoolStake ::
  (KeyHash 'StakePool c -> Bool) ->
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Stake c ->
  Map.Map (KeyHash 'StakePool c) Word64
calculatePoolStake includeHash delegs stake = VMap.foldlWithKey accum Map.empty delegs
  where
    accum ans cred keyHash =
      if includeHash keyHash
        then case VMap.lookup cred (unStake stake) of
          Nothing -> ans
          Just (CompactCoin c) -> Map.insertWith (+) keyHash c ans
        else ans

calculatePoolDistr :: SnapShot c -> PoolDistr c
calculatePoolDistr = calculatePoolDistr' (const True)

calculatePoolDistr' :: forall c. (KeyHash 'StakePool c -> Bool) -> SnapShot c -> PoolDistr c
calculatePoolDistr' includeHash (SnapShot stake delegs poolParams) =
  let Coin total = sumAllStake stake
      -- total could be zero (in particular when shrinking)
      nonZeroTotal :: Integer
      nonZeroTotal = if total == 0 then 1 else total
      poolStakeMap :: Map.Map (KeyHash 'StakePool c) Word64
      poolStakeMap = calculatePoolStake includeHash delegs stake
   in PoolDistr $
        Map.intersectionWith
          (\word64 poolparam -> IndividualPoolStake (toInteger word64 % nonZeroTotal) (ppVrf poolparam))
          poolStakeMap
          (VMap.toMap poolParams)
