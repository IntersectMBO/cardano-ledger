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
  sumCredentialsCompactStake,
  sumStakePerPool,
  StakePoolSnapShot (..),
  mkStakePoolSnapShot,
  SnapShot (..),
  SnapShots (..),
  emptySnapShot,
  emptySnapShots,
  snapShotFromInstantStake,
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

import Cardano.Ledger.Address (raCredential)
import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  KeyValuePairs (..),
  NonNegativeInterval,
  NonZero (..),
  ToKeyValuePairs (..),
  UnitInterval,
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
  decodeMap,
  decodeRecordNamedT,
  encodeListLen,
  toMemptyLens,
 )
import Cardano.Ledger.Binary.Decoding (interns)
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (..),
  coinToRational,
  fromCompactCoinNonZero,
  knownNonZeroCoin,
  knownNonZeroCompactCoin,
  rationalToCoinViaFloor,
  unCoinNonZero,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), credKeyHash)
import Cardano.Ledger.State.Account
import Cardano.Ledger.State.CertState (DState (..), PState (..))
import Cardano.Ledger.State.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.State.Stake
import Cardano.Ledger.State.StakePool (
  StakePoolParams (sppVrf),
  StakePoolState (..),
  stakePoolStateToStakePoolParams,
 )
import Cardano.Ledger.Val ((<+>))
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (get)
import Data.Aeson (ToJSON (..), (.=))
import Data.Default (Default, def)
import Data.Foldable (foldMap')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.), _1, _2)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | Type of stake as map from staking credential to coins associated. Any staking credential that
-- has no stake will not appear in this Map, even if it is registered. For this reason, this data
-- type should not be used for infering whether credential is registered or not.
newtype Stake = Stake
  { unStake :: VMap VB VP (Credential Staking) (CompactForm Coin)
  }
  deriving (Show, Eq, NFData, Generic, ToJSON, NoThunks, EncCBOR)

instance DecShareCBOR Stake where
  type Share Stake = Share (VMap VB VP (Credential Staking) (CompactForm Coin))
  getShare = getShare . unStake
  decShareCBOR = fmap Stake . decShareCBOR

sumAllStake :: Stake -> Coin
sumAllStake = fromCompact . sumAllStakeCompact
{-# INLINE sumAllStake #-}

sumAllStakeCompact :: Stake -> CompactForm Coin
sumAllStakeCompact = VMap.foldl (<>) mempty . unStake
{-# INLINE sumAllStakeCompact #-}

sumCredentialsCompactStake :: Foldable f => Stake -> f (Credential 'Staking) -> CompactForm Coin
sumCredentialsCompactStake (Stake stake) = foldMap' (fromMaybe mempty . (`VMap.lookup` stake))
{-# INLINE sumCredentialsCompactStake #-}

-- | Get stake of one pool
poolStake ::
  KeyHash StakePool ->
  VMap VB VB (Credential Staking) (KeyHash StakePool) ->
  Stake ->
  Stake
poolStake hk delegs (Stake stake) =
  -- Stake $ (eval (dom (delegs ▷ setSingleton hk) ◁ stake))
  Stake $ VMap.filter (\cred _ -> VMap.lookup cred delegs == Just hk) stake

-- | Compute amount of stake each pool has. Any registered stake pool that has no stake will not be
-- included in the resulting map
sumStakePerPool ::
  VMap VB VB (Credential Staking) (KeyHash StakePool) ->
  Stake ->
  Map (KeyHash StakePool) Coin
sumStakePerPool delegs (Stake stake) = VMap.foldlWithKey accum Map.empty stake
  where
    accum !acc cred compactCoin =
      case VMap.lookup cred delegs of
        Nothing -> acc
        Just kh -> Map.insertWith (<+>) kh (fromCompact compactCoin) acc
{-# DEPRECATED sumStakePerPool "As no longer necessary" #-}

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

data StakePoolSnapShot = StakePoolSnapShot
  { spssStake :: CompactForm Coin
  , spssStakeRatio :: Rational
  -- ^ Ratio of the stake pool stake `spssStake` over the total `ssTotalActiveStake` for that snapshot
  , spssSelfDelegatedOwnersStake :: CompactForm Coin
  -- ^ Sum of all the stake that is associated with the owners of the pool. Unlike owners that are
  -- specified in the `StakePoolParams`, owners used in computing this field are also ensured to be
  -- delegating to the stake pool they own.
  , spssVrf :: !(VRFVerKeyHash 'StakePoolVRF)
  , spssPledge :: !Coin
  , spssCost :: !Coin
  , spssMargin :: !UnitInterval
  , spssAccount :: !(Credential 'Staking)
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs StakePoolSnapShot

mkStakePoolSnapShot ::
  -- | Active Stake
  Stake ->
  -- | Total Active Stake
  NonZero Coin ->
  -- | Stake Pool State
  StakePoolState ->
  StakePoolSnapShot
mkStakePoolSnapShot activeStake totalActiveStake stakePoolState =
  StakePoolSnapShot
    { spssStake = stakePoolStake
    , spssStakeRatio = unCoin (fromCompact stakePoolStake) %. unCoinNonZero totalActiveStake
    , spssSelfDelegatedOwners = selfDelegatedOwners
    , spssSelfDelegatedOwnersStake =
        sumCredentialsCompactStake activeStake selfDelegatedOwners
    , spssVrf = spsVrf
    , spssPledge = spsPledge
    , spssCost = spsCost
    , spssMargin = spsMargin
    , spssAccount = raCredential spsRewardAccount
    }
  where
    StakePoolState {spsVrf, spsPledge, spsCost, spsMargin, spsRewardAccount, spsOwners, spsDelegators} =
      stakePoolState
    selfDelegatedOwners = Set.map KeyHashObj spsOwners `Set.intersection` spsDelegators
    stakePoolStake = sumCredentialsCompactStake activeStake spsDelegators

instance NoThunks StakePoolSnapShot

instance NFData StakePoolSnapShot

instance ToKeyValuePairs StakePoolSnapShot where
  toKeyValuePairs ss@(StakePoolSnapShot _ _ _ _ _ _ _ _ _) =
    let StakePoolSnapShot {..} = ss
     in [ "stake" .= spssStake
        , "stakeRatio" .= spssStakeRatio
        , "selfDelegatedOwners" .= spssSelfDelegatedOwners
        , "selfDelegatedOwnersStake" .= spssSelfDelegatedOwnersStake
        , "vrf" .= spssVrf
        , "pledge" .= spssPledge
        , "cost" .= spssCost
        , "margin" .= spssMargin
        , "account" .= spssAccount
        ]

instance EncCBOR StakePoolSnapShot where
  encCBOR spss@(StakePoolSnapShot _ _ _ _ _ _ _ _ _) =
    let StakePoolSnapShot {..} = spss
     in encodeListLen 9
          <> encCBOR spssStake
          <> encCBOR spssStakeRatio
          <> encCBOR spssSelfDelegatedOwners
          <> encCBOR spssSelfDelegatedOwnersStake
          <> encCBOR spssVrf
          <> encCBOR spssPledge
          <> encCBOR spssCost
          <> encCBOR spssMargin
          <> encCBOR spssAccount

instance DecShareCBOR StakePoolSnapShot where
  type Share StakePoolSnapShot = Interns (Credential 'Staking)
  decSharePlusCBOR = decodeRecordNamedT "StakePoolSnapShot" (const 9) $ do
    credInterns <- get
    spssStake <- lift decCBOR
    spssStakeRatio <- lift decCBOR
    let unwrap cred =
          fromMaybe (error $ "Impossible: Unwrapping an intern " <> show cred) $ credKeyHash cred
    spssSelfDelegatedOwners <- Set.map (unwrap . interns credInterns . KeyHashObj) <$> lift decCBOR
    spssSelfDelegatedOwnersStake <- lift decCBOR
    spssVrf <- lift decCBOR
    spssPledge <- lift decCBOR
    spssCost <- lift decCBOR
    spssMargin <- lift decCBOR
    spssAccount <- interns credInterns <$> lift decCBOR
    pure StakePoolSnapShot {..}

-- | Snapshot of the stake distribution.
data SnapShot = SnapShot
  { ssStake :: !Stake -- TODO: rename to `ssActiveStake`
  , ssTotalActiveStake :: NonZero Coin
  -- ^ Total active stake is primarely used in denominator, therefore it cannot be zero and is
  -- defaulted to 1. This is a reasonable assumption for a system that relies on non-zero active
  -- stake to produce blocks.
  , ssDelegations :: !(VMap VB VB (Credential Staking) (KeyHash StakePool)) -- TODO: remove
  , ssPoolParams :: !(VMap VB VB (KeyHash StakePool) StakePoolParams) -- TODO: remove
  , ssStakePoolSnapShots :: !(Map (KeyHash StakePool) StakePoolSnapShot)
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs SnapShot

instance NoThunks SnapShot

instance NFData SnapShot

instance EncCBOR SnapShot where
  encCBOR ss@(SnapShot _ _ _ _ _) =
    let SnapShot {..} = ss
     in encodeListLen 5
          <> encCBOR ssStake
          <> encCBOR ssTotalActiveStake
          <> encCBOR ssDelegations
          <> encCBOR ssPoolParams
          <> encCBOR ssStakePoolSnapShots

instance DecShareCBOR SnapShot where
  type Share SnapShot = (Interns (Credential Staking), Interns (KeyHash StakePool))
  decSharePlusCBOR = decodeRecordNamedT "SnapShot" (const 5) $ do
    ssStake <- decSharePlusLensCBOR _1
    ssTotalActiveStake <- lift decCBOR
    ssDelegations <- decSharePlusCBOR
    ssPoolParams <- decSharePlusLensCBOR (toMemptyLens _1 _2)
    (stakeCredInterns, stakePoolIdInterns) <- get
    ssStakePoolSnapShots <-
      lift $ decodeMap (interns stakePoolIdInterns <$> decCBOR) (decShareCBOR stakeCredInterns)
    pure SnapShot {..}

instance ToKeyValuePairs SnapShot where
  toKeyValuePairs ss@(SnapShot _ _ _ _ _) =
    let SnapShot {..} = ss
     in [ "stake" .= ssStake
        , "totalActiveStake" .= ssTotalActiveStake
        , "delegations" .= ssDelegations
        , "poolParams" .= ssPoolParams
        , "stakePoolSnapShots" .= ssStakePoolSnapShots
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
  deriving (ToJSON) via KeyValuePairs SnapShots
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

instance ToKeyValuePairs SnapShots where
  toKeyValuePairs ss@(SnapShots !_ _ _ _ _) =
    -- ssStakeMarkPoolDistr is omitted on purpose
    let SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee} = ss
     in [ "pstakeMark" .= ssStakeMark
        , "pstakeSet" .= ssStakeSet
        , "pstakeGo" .= ssStakeGo
        , "feeSS" .= ssFee
        ]

emptySnapShot :: SnapShot
emptySnapShot = SnapShot (Stake VMap.empty) (knownNonZeroCoin @1) VMap.empty VMap.empty mempty

emptySnapShots :: SnapShots
emptySnapShots =
  SnapShots emptySnapShot (calculatePoolDistr emptySnapShot) emptySnapShot emptySnapShot (Coin 0)

snapShotFromInstantStake ::
  forall era. EraStake era => InstantStake era -> DState era -> PState era -> SnapShot
snapShotFromInstantStake iStake dState PState {psStakePools} =
  SnapShot
    { ssStake = activeStake
    , ssTotalActiveStake = totalActiveStake
    , ssDelegations = VMap.fromDistinctAscListN delegsCount delegsAscList
    , ssPoolParams =
        VMap.fromDistinctAscListN
          (Map.size psStakePools)
          [(poolId, stakePoolStateToStakePoolParams poolId ps) | (poolId, ps) <- Map.toAscList psStakePools]
    , ssStakePoolSnapShots =
        Map.map (mkStakePoolSnapShot activeStake totalActiveStake) psStakePools
    }
  where
    activeStake = Stake $ VMap.fromMap $ resolveInstantStake iStake accounts
    totalActiveStake =
      fromCompact (sumAllStakeCompact activeStake) `nonZeroOr` knownNonZeroCoin @1
    accounts = dsAccounts dState
    keepAndCountDelegations ::
      Credential Staking ->
      AccountState era ->
      ([(Credential Staking, KeyHash StakePool)], Int) ->
      ([(Credential Staking, KeyHash StakePool)], Int)
    keepAndCountDelegations cred accountState acc@(!delegs, !count) =
      case accountState ^. stakePoolDelegationAccountStateL of
        Nothing -> acc
        Just deleg -> ((cred, deleg) : delegs, count + 1)
    (delegsAscList, delegsCount) =
      Map.foldrWithKey keepAndCountDelegations ([], 0) $ accounts ^. accountsMapL
{-# INLINE snapShotFromInstantStake #-}

-- =======================================

-- | Sum up the Coin (as CompactForm Coin = Word64) for each StakePool
calculatePoolStake ::
  (KeyHash StakePool -> Bool) ->
  VMap VB VB (Credential Staking) (KeyHash StakePool) ->
  Stake ->
  Map.Map (KeyHash StakePool) (CompactForm Coin)
calculatePoolStake includeHash delegs stake = VMap.foldlWithKey accum Map.empty delegs
  where
    accum ans cred keyHash =
      if includeHash keyHash
        then
          let !c = fromMaybe mempty $ VMap.lookup cred (unStake stake)
           in Map.insertWith (<>) keyHash c ans
        else ans

calculatePoolDistr :: SnapShot -> PoolDistr
calculatePoolDistr = calculatePoolDistr' (const True)

calculatePoolDistr' :: (KeyHash StakePool -> Bool) -> SnapShot -> PoolDistr
calculatePoolDistr' includeHash (SnapShot stake activeStake delegs poolParams stakePoolSnapShot) =
  let total = sumAllStakeCompact stake
      -- total could be zero (in particular when shrinking)
      nonZeroTotal = fromCompactCoinNonZero $ total `nonZeroOr` knownNonZeroCompactCoin @1
      poolStakeMap = calculatePoolStake includeHash delegs stake
      oldPoolDistr =
        PoolDistr
          ( Map.intersectionWith
              ( \stakePoolStake@(CompactCoin w64) poolParam ->
                  IndividualPoolStake
                    (toInteger w64 %. unCoinNonZero nonZeroTotal)
                    stakePoolStake
                    (sppVrf poolParam)
              )
              poolStakeMap
              (VMap.toMap poolParams)
          )
          nonZeroTotal
      toIndividualPoolStake poolId StakePoolSnapShot {spssStake, spssStakeRatio, spssVrf} = do
        guard (includeHash poolId)
        pure
          IndividualPoolStake
            { individualPoolStake = spssStakeRatio
            , individualTotalPoolStake = spssStake
            , individualPoolStakeVrf = spssVrf
            }
      poolDistr =
        PoolDistr
          { unPoolDistr = Map.mapMaybeWithKey toIndividualPoolStake stakePoolSnapShot
          , pdTotalActiveStake = activeStake
          }
   in assert (oldPoolDistr == poolDistr) poolDistr

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

ssStakeDistrL :: Lens' SnapShot (VMap VB VP (Credential Staking) (CompactForm Coin))
ssStakeDistrL = lens (unStake . ssStake) (\ds u -> ds {ssStake = Stake u})

ssDelegationsL :: Lens' SnapShot (VMap VB VB (Credential Staking) (KeyHash StakePool))
ssDelegationsL = lens ssDelegations (\ds u -> ds {ssDelegations = u})

ssPoolParamsL :: Lens' SnapShot (VMap VB VB (KeyHash StakePool) StakePoolParams)
ssPoolParamsL = lens ssPoolParams (\ds u -> ds {ssPoolParams = u})
