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
{-# LANGUAGE StandaloneDeriving #-}
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
)
where

import Cardano.Ledger.BaseTypes (BoundedRational (..), NonNegativeInterval)
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
  rationalToCoinViaFloor,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (ppVrf))
import Cardano.Ledger.Val ((<+>))
import Control.DeepSeq (NFData)
import Control.Monad.Trans (lift)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Typeable
import Data.VMap as VMap
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Lens.Micro (Lens', lens, (^.), _1, _2)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Numeric.Natural (Natural)

-- | Type of stake as map from hash key to coins associated.
newtype Stake c = Stake
  { unStake :: VMap VB VP (Credential 'Staking c) (CompactForm Coin)
  }
  deriving (Show, Eq, NFData, Generic, ToJSON)

deriving newtype instance Typeable c => NoThunks (Stake c)

deriving newtype instance
  Crypto c => EncCBOR (Stake c)

instance Crypto c => DecShareCBOR (Stake c) where
  type Share (Stake c) = Share (VMap VB VP (Credential 'Staking c) (CompactForm Coin))
  getShare = getShare . unStake
  decShareCBOR = fmap Stake . decShareCBOR

sumAllStake :: Stake c -> Coin
sumAllStake = fromCompact . sumAllStakeCompact
{-# INLINE sumAllStake #-}

sumAllStakeCompact :: Stake c -> CompactForm Coin
sumAllStakeCompact = VMap.foldl (<>) mempty . unStake
{-# INLINE sumAllStakeCompact #-}

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
  Crypto c =>
  EncCBOR (SnapShot c)
  where
  encCBOR
    SnapShot
      { ssStake = s
      , ssDelegations = d
      , ssPoolParams = p
      } =
      encodeListLen 3
        <> encCBOR s
        <> encCBOR d
        <> encCBOR p

instance Crypto c => DecShareCBOR (SnapShot c) where
  type
    Share (SnapShot c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  decSharePlusCBOR = decodeRecordNamedT "SnapShot" (const 3) $ do
    ssStake <- decSharePlusLensCBOR _1
    ssDelegations <- decSharePlusCBOR
    ssPoolParams <- decSharePlusLensCBOR (toMemptyLens _1 _2)
    pure SnapShot {ssStake, ssDelegations, ssPoolParams}

instance Crypto c => ToJSON (SnapShot c) where
  toJSON = object . toSnapShotPair
  toEncoding = pairs . mconcat . toSnapShotPair

toSnapShotPair :: (KeyValue e a, Crypto c) => SnapShot c -> [a]
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
data SnapShots c = SnapShots
  { ssStakeMark :: SnapShot c -- Lazy on purpose
  , ssStakeMarkPoolDistr :: PoolDistr c -- Lazy on purpose
  , ssStakeSet :: !(SnapShot c)
  , ssStakeGo :: !(SnapShot c)
  , ssFee :: !Coin
  }
  deriving (Show, Eq, Generic)
  -- TODO: switch `AllowThunksIn` to `OnlyCheckWhnfNamed`
  deriving (NoThunks) via AllowThunksIn '["ssStakeMark", "ssStakeMarkPoolDistr"] (SnapShots c)

instance NFData (SnapShots c)

instance
  Crypto c =>
  EncCBOR (SnapShots c)
  where
  encCBOR (SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}) =
    encodeListLen 4
      <> encCBOR ssStakeMark
      -- We intentionaly do not serialize the redundant ssStakeMarkPoolDistr
      <> encCBOR ssStakeSet
      <> encCBOR ssStakeGo
      <> encCBOR ssFee

instance Crypto c => DecCBOR (SnapShots c) where
  decCBOR = decNoShareCBOR

instance Crypto c => DecShareCBOR (SnapShots c) where
  type Share (SnapShots c) = Share (SnapShot c)
  decSharePlusCBOR = decodeRecordNamedT "SnapShots" (const 4) $ do
    !ssStakeMark <- decSharePlusCBOR
    ssStakeSet <- decSharePlusCBOR
    ssStakeGo <- decSharePlusCBOR
    ssFee <- lift decCBOR
    let ssStakeMarkPoolDistr = calculatePoolDistr ssStakeMark
    pure SnapShots {ssStakeMark, ssStakeMarkPoolDistr, ssStakeSet, ssStakeGo, ssFee}

instance Default (SnapShots c) where
  def = emptySnapShots

instance Crypto c => ToJSON (SnapShots c) where
  toJSON = object . toSnapShotsPair
  toEncoding = pairs . mconcat . toSnapShotsPair

toSnapShotsPair :: (KeyValue e a, Crypto crypto) => SnapShots crypto -> [a]
toSnapShotsPair ss@(SnapShots !_ _ _ _ _) =
  -- ssStakeMarkPoolDistr is omitted on purpose
  let SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee} = ss
   in [ "pstakeMark" .= ssStakeMark
      , "pstakeSet" .= ssStakeSet
      , "pstakeGo" .= ssStakeGo
      , "feeSS" .= ssFee
      ]

emptySnapShot :: SnapShot c
emptySnapShot = SnapShot (Stake VMap.empty) VMap.empty VMap.empty

emptySnapShots :: SnapShots c
emptySnapShots =
  SnapShots emptySnapShot (calculatePoolDistr emptySnapShot) emptySnapShot emptySnapShot (Coin 0)

-- =======================================

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
  let total = sumAllStakeCompact stake
      -- total could be zero (in particular when shrinking)
      nonZeroTotalCompact = if total == mempty then CompactCoin 1 else total
      nonZeroTotalInteger = unCoin $ fromCompact nonZeroTotalCompact
      poolStakeMap = calculatePoolStake includeHash delegs stake
   in PoolDistr
        ( Map.intersectionWith
            ( \word64 poolparam ->
                IndividualPoolStake
                  (toInteger word64 % nonZeroTotalInteger)
                  (CompactCoin word64)
                  (ppVrf poolparam)
            )
            poolStakeMap
            (VMap.toMap poolParams)
        )
        nonZeroTotalCompact

-- ======================================================
-- Lenses
-- ===============================================

-- SnapShots

ssStakeMarkL :: Lens' (SnapShots c) (SnapShot c)
ssStakeMarkL = lens ssStakeMark (\ds u -> ds {ssStakeMark = u})

ssStakeMarkPoolDistrL :: Lens' (SnapShots c) (PoolDistr c)
ssStakeMarkPoolDistrL = lens ssStakeMarkPoolDistr (\ds u -> ds {ssStakeMarkPoolDistr = u})

ssStakeSetL :: Lens' (SnapShots c) (SnapShot c)
ssStakeSetL = lens ssStakeSet (\ds u -> ds {ssStakeSet = u})

ssStakeGoL :: Lens' (SnapShots c) (SnapShot c)
ssStakeGoL = lens ssStakeGo (\ds u -> ds {ssStakeGo = u})

ssFeeL :: Lens' (SnapShots c) Coin
ssFeeL = lens ssFee (\ds u -> ds {ssFee = u})

-- SnapShot

ssStakeL :: Lens' (SnapShot c) (Stake c)
ssStakeL = lens ssStake (\ds u -> ds {ssStake = u})

ssStakeDistrL :: Lens' (SnapShot c) (VMap VB VP (Credential 'Staking c) (CompactForm Coin))
ssStakeDistrL = lens (unStake . ssStake) (\ds u -> ds {ssStake = Stake u})

ssDelegationsL :: Lens' (SnapShot c) (VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c))
ssDelegationsL = lens ssDelegations (\ds u -> ds {ssDelegations = u})

ssPoolParamsL :: Lens' (SnapShot c) (VMap VB VB (KeyHash 'StakePool c) (PoolParams c))
ssPoolParamsL = lens ssPoolParams (\ds u -> ds {ssPoolParams = u})
