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

module Cardano.Ledger.State.SnapShots (
  sumStakePerPool,
  StakePoolSnapShot (..),
  mkStakePoolSnapShot,
  SnapShot (..),
  SnapShots (..),
  emptySnapShot,
  emptySnapShots,
  snapShotFromInstantStake,
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
  KeyValuePairs (..),
  Network,
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
  decodeRecordNamedT,
  decodeVMap,
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Data.Word (Word16)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro (Lens', lens, (^.), _1, _2)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

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

-- | This type is the collection of all the necessary data per stake pool that is derived from the
-- `StakePoolState`, `InstantStake` and `Accounts` that is later used for reward
-- calculation
data StakePoolSnapShot = StakePoolSnapShot
  { spssStake :: !(CompactForm Coin)
  -- ^ Total stake delegated to this stake pool.
  , spssStakeRatio :: !Rational
  -- ^ Ratio of the stake pool stake `spssStake` over the total `ssTotalActiveStake` for that snapshot
  , spssSelfDelegatedOwners :: !(Set (KeyHash Staking))
  -- ^ Unlike owners that are specified in the `StakePoolParams`, the owners listed in this field
  -- are also ensured to be delegating to the stake pool they claim to own.
  , spssSelfDelegatedOwnersStake :: !Coin
  -- ^ Sum of all the stake that is associated with the owners of the pool listed in
  -- `spssSelfDelegatedOwners`
  , spssVrf :: !(VRFVerKeyHash StakePoolVRF)
  -- ^ Corresponding field in the `StakePoolState` is `spsVrf`.
  , spssPledge :: !Coin
  -- ^ Corresponding field in the `StakePoolState` is `spsPledge`.
  , spssCost :: !Coin
  -- ^ Corresponding field in the `StakePoolState` is `spsCost`.
  , spssMargin :: !UnitInterval
  -- ^ Corresponding field in the `StakePoolState` is `spsMargin`.
  , spssNumDelegators :: !Int
  -- ^ Number of delegators, which is the count from the `spsDelegators` field.  We don't need the
  -- actual delegators, since at this point the actual stake has already been resolved.  This count
  -- is only needed to preserve older behavior where we filter out stake pools from `PoolDistr` that
  -- do not have any delegations.
  , spssAccountId :: !(Credential Staking)
  -- ^ This is the account where stake pools rewards will be deposited to. Corresponding field in
  -- the `StakePoolState` is `spsAccountAddress`.
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
        fromCompact $
          sumCredentialsCompactStake activeStake $
            -- Conversion to a list allows us to tap into list fusion, thus avoiding unnecessary
            -- extra Set allocation and `O(n*log(n))` mappping over a Set.
            map KeyHashObj (Set.elems selfDelegatedOwners)
    , spssVrf = spsVrf
    , spssPledge = spsPledge
    , spssCost = spsCost
    , spssMargin = spsMargin
    , spssNumDelegators = Set.size spsDelegators
    , spssAccountId = spsAccountAddress
    }
  where
    StakePoolState {spsVrf, spsPledge, spsCost, spsMargin, spsAccountAddress, spsOwners, spsDelegators} =
      stakePoolState
    selfDelegatedOwners =
      Set.filter (\ownerKeyHash -> KeyHashObj ownerKeyHash `Set.member` spsDelegators) spsOwners
    stakePoolStake = sumCredentialsCompactStake activeStake spsDelegators

instance NoThunks StakePoolSnapShot

instance NFData StakePoolSnapShot

instance ToKeyValuePairs StakePoolSnapShot where
  toKeyValuePairs ss@(StakePoolSnapShot _ _ _ _ _ _ _ _ _ _) =
    let StakePoolSnapShot {..} = ss
     in [ "stake" .= spssStake
        , "stakeRatio" .= spssStakeRatio
        , "selfDelegatedOwners" .= spssSelfDelegatedOwners
        , "selfDelegatedOwnersStake" .= spssSelfDelegatedOwnersStake
        , "vrf" .= spssVrf
        , "pledge" .= spssPledge
        , "cost" .= spssCost
        , "margin" .= spssMargin
        , "numDelegators" .= spssNumDelegators
        , "accountId" .= spssAccountId
        ]

instance EncCBOR StakePoolSnapShot where
  encCBOR spss@(StakePoolSnapShot _ _ _ _ _ _ _ _ _ _) =
    let StakePoolSnapShot {..} = spss
     in encodeListLen 10
          <> encCBOR spssStake
          <> encCBOR spssStakeRatio
          <> encCBOR spssSelfDelegatedOwners
          <> encCBOR spssSelfDelegatedOwnersStake
          <> encCBOR spssVrf
          <> encCBOR spssPledge
          <> encCBOR spssCost
          <> encCBOR spssMargin
          <> encCBOR spssNumDelegators
          <> encCBOR spssAccountId

instance DecShareCBOR StakePoolSnapShot where
  type Share StakePoolSnapShot = Interns (Credential Staking)
  decSharePlusCBOR = decodeRecordNamedT "StakePoolSnapShot" (const 10) $ do
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
    spssNumDelegators <- lift decCBOR
    spssAccountId <- interns credInterns <$> lift decCBOR
    pure StakePoolSnapShot {..}

-- | Snapshot of the stake distribution.
data SnapShot = SnapShot
  { ssStake :: !Stake -- TODO: rename to `ssActiveStake`

  -- ^ All of the stake for registered staking credentials that have a delegation to a stake pool.
  , ssTotalActiveStake :: NonZero Coin -- Note: lazy on purpose

  -- ^ Total active stake, which is the sum of all of the stake from `ssStake`, which is why it is
  -- lazy.  It is primarily used in a denominator, therefore it cannot be zero and is defaulted to
  -- 1. This is a reasonable assumption for a system that relies on non-zero active stake to produce
  -- blocks.
  , ssDelegations :: VMap VB VB (Credential Staking) (KeyHash StakePool) -- TODO: remove (lazy on purpose)
  , ssPoolParams :: VMap VB VB (KeyHash StakePool) StakePoolParams -- TODO: remove (lazy on purpose)
  , ssStakePoolsSnapShot :: !(VMap VB VB (KeyHash StakePool) StakePoolSnapShot)
  -- ^ Snapshot of stake pools' information that is relevant only for the reward calculation logic.
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs SnapShot
  deriving
    (NoThunks)
    via AllowThunksIn '["ssTotalActiveStake", "ssDelegations", "ssPoolParams"] SnapShot

instance NFData SnapShot

instance EncCBOR SnapShot where
  encCBOR ss@(SnapShot _ _ _ _ _) =
    let SnapShot {..} = ss
     in encodeListLen 4
          <> encCBOR ssStake
          -- `ssTotalActiveStake` is ommitted on purpose
          <> encCBOR ssDelegations
          <> encCBOR ssPoolParams
          <> encCBOR ssStakePoolsSnapShot

instance DecShareCBOR SnapShot where
  type Share SnapShot = (Interns (Credential Staking), Interns (KeyHash StakePool))
  decSharePlusCBOR = decodeRecordNamedT "SnapShot" (const 4) $ do
    ssStake <- decSharePlusLensCBOR _1
    let ssTotalActiveStake = sumAllStake ssStake `nonZeroOr` knownNonZeroCoin @1
    ssDelegations <- decSharePlusCBOR
    ssPoolParams <- decSharePlusLensCBOR (toMemptyLens _1 _2)
    (stakeCredInterns, stakePoolIdInterns) <- get
    ssStakePoolsSnapShot <-
      lift $ decodeVMap (interns stakePoolIdInterns <$> decCBOR) (decShareCBOR stakeCredInterns)
    pure SnapShot {..}

instance ToKeyValuePairs SnapShot where
  toKeyValuePairs ss@(SnapShot _ _ _ _ _) =
    let SnapShot {..} = ss
     in [ "stake" .= ssStake
        , "delegations" .= ssDelegations
        , "poolParams" .= ssPoolParams
        , "stakePoolsSnapShot" .= ssStakePoolsSnapShot
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
  forall era.
  (HasCallStack, EraStake era) =>
  InstantStake era ->
  DState era ->
  PState era ->
  Network ->
  SnapShot
snapShotFromInstantStake instantStake dState PState {psStakePools} network =
  assert
    ( delegatorsPerStakePool
        == reverseDelegatorsPerStakePool
        || error
          ( "Delegs:\n  "
              ++ show delegatorsPerStakePool
              ++ "\n/=\nReverse Delegs:\n  "
              ++ show reverseDelegatorsPerStakePool
          )
    )
    $ SnapShot
      { ssStake = activeStake
      , ssTotalActiveStake = totalActiveStake
      , ssDelegations = delegs
      , ssPoolParams = poolParams
      , ssStakePoolsSnapShot = stakePoolsSnapShot
      }
  where
    reverseDelegatorsPerStakePool =
      Map.mapMaybe
        (\sps -> spsDelegators sps <$ guard (not (Set.null (spsDelegators sps))))
        psStakePools
    poolParams =
      VMap.fromDistinctAscListN
        (Map.size psStakePools)
        [ (poolId, stakePoolStateToStakePoolParams poolId network ps)
        | (poolId, ps) <- Map.toAscList psStakePools
        ]
    stakePoolsSnapShot =
      VMap.map (mkStakePoolSnapShot activeStake totalActiveStake) $ VMap.fromMap psStakePools
    activeStake = resolveInstantStake instantStake accounts
    totalActiveStake = sumAllStake activeStake `nonZeroOr` knownNonZeroCoin @1
    accounts = dsAccounts dState
    delegs = VMap.fromDistinctAscListN delegsCount delegsAscList
    delegatorsPerStakePool =
      VMap.foldlWithKey
        (\acc cred poolId -> Map.insertWith (<>) poolId (Set.singleton cred) acc)
        mempty
        delegs
    keepAndCountDelegations ::
      Credential Staking ->
      AccountState era ->
      ([(Credential Staking, KeyHash StakePool)], Int) ->
      ([(Credential Staking, KeyHash StakePool)], Int)
    keepAndCountDelegations cred accountState acc@(!curDelegs, !curCount) =
      case accountState ^. stakePoolDelegationAccountStateL of
        Nothing -> acc
        Just deleg -> ((cred, deleg) : curDelegs, curCount + 1)
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

calculatePoolDistr :: HasCallStack => SnapShot -> PoolDistr
calculatePoolDistr = calculatePoolDistr' (const True)

calculatePoolDistr' :: HasCallStack => (KeyHash StakePool -> Bool) -> SnapShot -> PoolDistr
calculatePoolDistr' includeHash (SnapShot stake activeStake delegs poolParams stakePoolSnapShot) =
  let total = sumAllStakeCompact stake
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
      toIndividualPoolStake poolId spss = do
        guard (includeHash poolId)
        guard (spssNumDelegators spss > 0)
        Just
          IndividualPoolStake
            { individualPoolStake = spssStakeRatio spss
            , individualTotalPoolStake = spssStake spss
            , individualPoolStakeVrf = spssVrf spss
            }
      poolDistr =
        PoolDistr
          { unPoolDistr = VMap.toMap $ VMap.mapMaybeWithKey toIndividualPoolStake stakePoolSnapShot
          , pdTotalActiveStake = activeStake
          }
      showFailure =
        error $
          "PoolDistr is not the same:\nOld PoolDistr:\n"
            <> show oldPoolDistr
            <> "\nNew PoolDistr:\n"
            <> show poolDistr
   in assert (oldPoolDistr == poolDistr || showFailure) poolDistr

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
