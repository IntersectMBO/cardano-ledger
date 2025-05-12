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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : EpochBoundary
-- Description : Functions and definitions for rules at epoch boundary.
--
-- This modules implements the necessary functions for the changes that can happen at epoch boundaries.
module Cardano.Ledger.State.SnapShots (
  Stake (..),
  sumAllStake,
  sumAllStakeCompact,
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
  ssStakeMarkL,
  ssStakeMarkPoolDistrL,
  ssStakeSetL,
  ssStakeGoL,
  ssFeeL,
  ssStakeL,
  ssStakeDistrL,
  ssDelegationsL,
  ssPoolParamsL,
) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  NonNegativeInterval,
  NonZero (..),
  knownNonZeroBounded,
  nonZeroOr,
  recipNonZero,
  toIntegerNonZero,
  toRatioNonZero,
  (%.),
  (/.),
 )
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecShareCBOR (..),
  EncCBOR (encCBOR),
  Interns,
  decNoShareCBOR,
  decSharePlusLensCBOR,
  decodeRecordNamedT,
  encodeListLen,
  toMemptyLens,
 )
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (..),
  coinToRational,
  compactCoinNonZero,
  fromCompactCoinNonZero,
  rationalToCoinViaFloor,
  unCoinNonZero,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.PoolParams (PoolParams (ppVrf))
import Cardano.Ledger.State.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Val ((<+>))
import Control.DeepSeq (NFData)
import Control.Monad.Trans (lift)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.VMap as VMap
import Data.Word (Word16)
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Lens.Micro (Lens', lens, (^.), _1, _2)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | Type of stake as map from staking credential to coins associated. Any staking credential that
-- has no stake will not appear in this Map, even if it is registered. For this reason, this data
-- type should not be used for infering whether credential is registered or not.
newtype Stake = Stake
  { unStake :: VMap VB VP (Credential 'Staking) (CompactForm Coin)
  }
  deriving (Show, Eq, NFData, Generic, ToJSON, NoThunks, EncCBOR)

instance DecShareCBOR Stake where
  type Share Stake = Share (VMap VB VP (Credential 'Staking) (CompactForm Coin))
  getShare = getShare . unStake
  decShareCBOR = fmap Stake . decShareCBOR

sumAllStake :: Stake -> Coin
sumAllStake = fromCompact . sumAllStakeCompact
{-# INLINE sumAllStake #-}

sumAllStakeCompact :: Stake -> CompactForm Coin
sumAllStakeCompact = VMap.foldl (<>) mempty . unStake
{-# INLINE sumAllStakeCompact #-}

-- | Get stake of one pool
poolStake ::
  KeyHash 'StakePool ->
  VMap VB VB (Credential 'Staking) (KeyHash 'StakePool) ->
  Stake ->
  Stake
poolStake hk delegs (Stake stake) =
  -- Stake $ (eval (dom (delegs ▷ setSingleton hk) ◁ stake))
  Stake $ VMap.filter (\cred _ -> VMap.lookup cred delegs == Just hk) stake

-- | Compute amount of stake each pool has. Any registered stake pool that has no stake will not be
-- inlcuded in the resulting map
sumStakePerPool ::
  VMap VB VB (Credential 'Staking) (KeyHash 'StakePool) ->
  Stake ->
  Map (KeyHash 'StakePool) Coin
sumStakePerPool delegs (Stake stake) = VMap.foldlWithKey accum Map.empty stake
  where
    accum !acc cred compactCoin =
      case VMap.lookup cred delegs of
        Nothing -> acc
        Just kh -> Map.insertWith (<+>) kh (fromCompact compactCoin) acc

-- | Calculate maximal pool reward
maxPool' ::
  NonNegativeInterval ->
  NonZero Word16 ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool' a0 nOpt r sigma pR = rationalToCoinViaFloor $ factor1 * factor2
  where
    nonZeroZ0 = recipNonZero . toRatioNonZero $ toIntegerNonZero nOpt
    z0 = unNonZero nonZeroZ0
    sigma' = min sigma z0
    p' = min pR z0
    factor1 =
      -- This division is safe, because a0 is non-negative and we're adding one
      -- to it
      coinToRational r / (1 + unboundRational a0)
    factor2 = sigma' + p' * unboundRational a0 * factor3
    factor3 = (sigma' - p' * factor4) /. nonZeroZ0
    factor4 = (z0 - sigma') /. nonZeroZ0

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
    nOpt = (pp ^. ppNOptL) `nonZeroOr` knownNonZeroBounded @1

-- | Snapshot of the stake distribution.
data SnapShot = SnapShot
  { ssStake :: !Stake
  , ssDelegations :: !(VMap VB VB (Credential 'Staking) (KeyHash 'StakePool))
  , ssPoolParams :: !(VMap VB VB (KeyHash 'StakePool) PoolParams)
  }
  deriving (Show, Eq, Generic)

instance NoThunks SnapShot

instance NFData SnapShot

instance EncCBOR SnapShot where
  encCBOR SnapShot {ssStake, ssDelegations, ssPoolParams} =
    encodeListLen 3
      <> encCBOR ssStake
      <> encCBOR ssDelegations
      <> encCBOR ssPoolParams

instance DecShareCBOR SnapShot where
  type Share SnapShot = (Interns (Credential 'Staking), Interns (KeyHash 'StakePool))
  decSharePlusCBOR = decodeRecordNamedT "SnapShot" (const 3) $ do
    ssStake <- decSharePlusLensCBOR _1
    ssDelegations <- decSharePlusCBOR
    ssPoolParams <- decSharePlusLensCBOR (toMemptyLens _1 _2)
    pure SnapShot {ssStake, ssDelegations, ssPoolParams}

instance ToJSON SnapShot where
  toJSON = object . toSnapShotPair
  toEncoding = pairs . mconcat . toSnapShotPair

toSnapShotPair :: KeyValue e a => SnapShot -> [a]
toSnapShotPair ss@(SnapShot _ _ _) =
  let SnapShot {..} = ss
   in [ "stake" .= ssStake
      , "delegations" .= ssDelegations
      , "poolParams" .= ssPoolParams
      ]

-- | Snapshots of the stake distribution.
--
-- Note that ssStakeMark and ssStakeMarkPoolDistr are lazy on
-- purpose since we only want to force the thunk after one stability window
-- when we know that they are stable (so that we do not compute them if we do not have to).
-- See more info in the [Optimize TICKF ADR](https://github.com/intersectmbo/cardano-ledger/blob/master/docs/adr/2022-12-12_007-optimize-ledger-view.md)
data SnapShots = SnapShots
  { ssStakeMark :: SnapShot -- Lazy on purpose
  , ssStakeMarkPoolDistr :: PoolDistr -- Lazy on purpose
  , ssStakeSet :: !SnapShot
  , ssStakeGo :: !SnapShot
  , ssFee :: !Coin
  }
  deriving (Show, Eq, Generic)
  -- TODO: switch `AllowThunksIn` to `OnlyCheckWhnfNamed`
  deriving (NoThunks) via AllowThunksIn '["ssStakeMark", "ssStakeMarkPoolDistr"] SnapShots

instance NFData SnapShots

instance EncCBOR SnapShots where
  encCBOR (SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}) =
    encodeListLen 4
      <> encCBOR ssStakeMark
      -- We intentionaly do not serialize the redundant ssStakeMarkPoolDistr
      <> encCBOR ssStakeSet
      <> encCBOR ssStakeGo
      <> encCBOR ssFee

instance DecCBOR SnapShots where
  decCBOR = decNoShareCBOR

instance DecShareCBOR SnapShots where
  type Share SnapShots = Share SnapShot
  decSharePlusCBOR = decodeRecordNamedT "SnapShots" (const 4) $ do
    !ssStakeMark <- decSharePlusCBOR
    ssStakeSet <- decSharePlusCBOR
    ssStakeGo <- decSharePlusCBOR
    ssFee <- lift decCBOR
    let ssStakeMarkPoolDistr = calculatePoolDistr ssStakeMark
    pure SnapShots {ssStakeMark, ssStakeMarkPoolDistr, ssStakeSet, ssStakeGo, ssFee}

instance Default SnapShots where
  def = emptySnapShots

instance ToJSON SnapShots where
  toJSON = object . toSnapShotsPair
  toEncoding = pairs . mconcat . toSnapShotsPair

toSnapShotsPair :: KeyValue e a => SnapShots -> [a]
toSnapShotsPair ss@(SnapShots !_ _ _ _ _) =
  -- ssStakeMarkPoolDistr is omitted on purpose
  let SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee} = ss
   in [ "pstakeMark" .= ssStakeMark
      , "pstakeSet" .= ssStakeSet
      , "pstakeGo" .= ssStakeGo
      , "feeSS" .= ssFee
      ]

emptySnapShot :: SnapShot
emptySnapShot = SnapShot (Stake VMap.empty) VMap.empty VMap.empty

emptySnapShots :: SnapShots
emptySnapShots =
  SnapShots emptySnapShot (calculatePoolDistr emptySnapShot) emptySnapShot emptySnapShot (Coin 0)

-- =======================================

-- | Sum up the Coin (as CompactForm Coin = Word64) for each StakePool
calculatePoolStake ::
  (KeyHash 'StakePool -> Bool) ->
  VMap VB VB (Credential 'Staking) (KeyHash 'StakePool) ->
  Stake ->
  Map.Map (KeyHash 'StakePool) Word64
calculatePoolStake includeHash delegs stake = VMap.foldlWithKey accum Map.empty delegs
  where
    accum ans cred keyHash =
      if includeHash keyHash
        then
          let CompactCoin c = fromMaybe mempty $ VMap.lookup cred (unStake stake)
           in Map.insertWith (+) keyHash c ans
        else ans

calculatePoolDistr :: SnapShot -> PoolDistr
calculatePoolDistr = calculatePoolDistr' (const True)

calculatePoolDistr' :: (KeyHash 'StakePool -> Bool) -> SnapShot -> PoolDistr
calculatePoolDistr' includeHash (SnapShot stake delegs poolParams) =
  let CompactCoin total = sumAllStakeCompact stake
      -- total could be zero (in particular when shrinking)
      nonZeroTotalCompact = compactCoinNonZero $ total `nonZeroOr` knownNonZeroBounded @1
      nonZeroTotalInteger = unCoinNonZero $ fromCompactCoinNonZero nonZeroTotalCompact
      poolStakeMap = calculatePoolStake includeHash delegs stake
   in PoolDistr
        ( Map.intersectionWith
            ( \word64 poolparam ->
                IndividualPoolStake
                  (toInteger word64 %. nonZeroTotalInteger)
                  (CompactCoin word64)
                  (ppVrf poolparam)
            )
            poolStakeMap
            (VMap.toMap poolParams)
        )
        (unNonZero nonZeroTotalCompact)

-- ======================================================
-- Lenses
-- ===============================================

-- SnapShots

ssStakeMarkL :: Lens' SnapShots SnapShot
ssStakeMarkL = lens ssStakeMark (\ds u -> ds {ssStakeMark = u})

ssStakeMarkPoolDistrL :: Lens' SnapShots PoolDistr
ssStakeMarkPoolDistrL = lens ssStakeMarkPoolDistr (\ds u -> ds {ssStakeMarkPoolDistr = u})

ssStakeSetL :: Lens' SnapShots SnapShot
ssStakeSetL = lens ssStakeSet (\ds u -> ds {ssStakeSet = u})

ssStakeGoL :: Lens' SnapShots SnapShot
ssStakeGoL = lens ssStakeGo (\ds u -> ds {ssStakeGo = u})

ssFeeL :: Lens' SnapShots Coin
ssFeeL = lens ssFee (\ds u -> ds {ssFee = u})

-- SnapShot

ssStakeL :: Lens' SnapShot Stake
ssStakeL = lens ssStake (\ds u -> ds {ssStake = u})

ssStakeDistrL :: Lens' SnapShot (VMap VB VP (Credential 'Staking) (CompactForm Coin))
ssStakeDistrL = lens (unStake . ssStake) (\ds u -> ds {ssStake = Stake u})

ssDelegationsL :: Lens' SnapShot (VMap VB VB (Credential 'Staking) (KeyHash 'StakePool))
ssDelegationsL = lens ssDelegations (\ds u -> ds {ssDelegations = u})

ssPoolParamsL :: Lens' SnapShot (VMap VB VB (KeyHash 'StakePool) PoolParams)
ssPoolParamsL = lens ssPoolParams (\ds u -> ds {ssPoolParams = u})
