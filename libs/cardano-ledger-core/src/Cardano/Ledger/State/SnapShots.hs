{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
  mkSnapShot,
  leiosCandidates,
  ssLeiosCommitteeL,
  SnapShots (..),
  emptySnapShot,
  emptySnapShots,
  snapShotFromInstantStake,
  resetStakePoolsSnapShot,
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
  ssStake,
  ssStakeL,
  ssActiveStakeL,
) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  KeyValuePairs (..),
  NonNegativeInterval,
  NonZero (..),
  StrictMaybe,
  ToKeyValuePairs (..),
  UnitInterval,
  knownNonZeroBounded,
  nonZeroOr,
  recipNonZero,
  toIntegerNonZero,
  toRatioNonZero,
  unsafeNonZero,
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
  decodeListLen,
  decodeRecordNamedT,
  decodeVMap,
  encodeListLen,
  getDecoderVersion,
  ifEncodingVersionAtLeast,
  natVersion,
 )
import Cardano.Ledger.Binary.Decoding (interns)
import Cardano.Ledger.Coin (
  Coin (..),
  coinToRational,
  knownNonZeroCoin,
  rationalToCoinViaFloor,
  unCoinNonZero,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), credKeyHash)
import Cardano.Ledger.State.CertState (DState (..), PState (..))
import Cardano.Ledger.State.LeiosCommittee (
  LeiosCandidate (..),
  LeiosCommittee (..),
  emptyLeiosCommittee,
  selectLeiosCommittee,
 )
import Cardano.Ledger.State.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.State.Stake
import Cardano.Ledger.State.StakePool (LeiosKey, StakePoolState (..))
import Cardano.Ledger.Val ((<+>))
import Control.DeepSeq (NFData)
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
import Data.VMap (VB, VMap (..))
import qualified Data.VMap as VMap
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.), _1)
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
  , spssLeiosKey :: !(StrictMaybe LeiosKey)
  -- ^ Corresponding field in the `StakePoolState` is `spsLeiosKey`.
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
  , spssAccountId :: !AccountId
  -- ^ This is the account where stake pools rewards will be deposited to. Corresponding field in
  -- the `StakePoolState` is `spsAccountAddress`.
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs StakePoolSnapShot

mkStakePoolSnapShot ::
  -- | Active Stake
  ActiveStake ->
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
          sumCredentialsCompactActiveStake activeStake $
            -- Conversion to a list allows us to tap into list fusion, thus avoiding unnecessary
            -- extra Set allocation and `O(n*log(n))` mappping over a Set.
            map KeyHashObj (Set.elems selfDelegatedOwners)
    , spssVrf = spsVrf
    , spssLeiosKey = spsLeiosKey
    , spssPledge = spsPledge
    , spssCost = spsCost
    , spssMargin = spsMargin
    , spssNumDelegators = Set.size spsDelegators
    , spssAccountId = spsAccountId
    }
  where
    StakePoolState
      { spsVrf
      , spsLeiosKey
      , spsPledge
      , spsCost
      , spsMargin
      , spsAccountId
      , spsOwners
      , spsDelegators
      } =
        stakePoolState
    selfDelegatedOwners =
      Set.filter (\ownerKeyHash -> KeyHashObj ownerKeyHash `Set.member` spsDelegators) spsOwners
    stakePoolStake = sumCredentialsCompactActiveStake activeStake spsDelegators

instance NoThunks StakePoolSnapShot

instance NFData StakePoolSnapShot

instance ToKeyValuePairs StakePoolSnapShot where
  toKeyValuePairs ss@(StakePoolSnapShot _ _ _ _ _ _ _ _ _ _ _) =
    let StakePoolSnapShot {..} = ss
     in [ "stake" .= spssStake
        , "stakeRatio" .= spssStakeRatio
        , "selfDelegatedOwners" .= spssSelfDelegatedOwners
        , "selfDelegatedOwnersStake" .= spssSelfDelegatedOwnersStake
        , "vrf" .= spssVrf
        , "leiosKey" .= spssLeiosKey
        , "pledge" .= spssPledge
        , "cost" .= spssCost
        , "margin" .= spssMargin
        , "numDelegators" .= spssNumDelegators
        , "accountId" .= spssAccountId
        ]

instance EncCBOR StakePoolSnapShot where
  encCBOR spss@(StakePoolSnapShot _ _ _ _ _ _ _ _ _ _ _) =
    let StakePoolSnapShot {..} = spss
     in encodeListLen 11
          <> encCBOR spssStake
          <> encCBOR spssStakeRatio
          <> encCBOR spssSelfDelegatedOwners
          <> encCBOR spssSelfDelegatedOwnersStake
          <> encCBOR spssVrf
          <> encCBOR spssLeiosKey
          <> encCBOR spssPledge
          <> encCBOR spssCost
          <> encCBOR spssMargin
          <> encCBOR spssNumDelegators
          <> encCBOR spssAccountId

instance DecShareCBOR StakePoolSnapShot where
  type Share StakePoolSnapShot = Interns (Credential Staking)
  decSharePlusCBOR = decodeRecordNamedT "StakePoolSnapShot" (const 11) $ do
    credInterns <- get
    spssStake <- lift decCBOR
    spssStakeRatio <- lift decCBOR
    let unwrap cred =
          fromMaybe (error $ "Impossible: Unwrapping an intern " <> show cred) $ credKeyHash cred
    spssSelfDelegatedOwners <- Set.map (unwrap . interns credInterns . KeyHashObj) <$> lift decCBOR
    spssSelfDelegatedOwnersStake <- lift decCBOR
    spssVrf <- lift decCBOR
    spssLeiosKey <- lift decCBOR
    spssPledge <- lift decCBOR
    spssCost <- lift decCBOR
    spssMargin <- lift decCBOR
    spssNumDelegators <- lift decCBOR
    spssAccountId <- AccountId . interns credInterns <$> lift decCBOR
    pure StakePoolSnapShot {..}

-- | Snapshot of the stake distribution.
data SnapShot = SnapShot
  { ssActiveStake :: !ActiveStake
  -- ^ All of the stake for registered staking credentials that have a delegation to a stake pool.
  , ssTotalActiveStake :: !(NonZero Coin)
  -- ^ Total active stake, which is the sum of all of the stake from `ssActiveStake`. It is primarily used
  -- in a denominator, therefore it cannot be zero and is defaulted to 1. This is a reasonable
  -- assumption for a system that relies on non-zero active stake to produce blocks.
  , ssStakePoolsSnapShot :: !(VMap VB VB (KeyHash StakePool) StakePoolSnapShot)
  -- ^ Snapshot of stake pools' information that is relevant only for the reward calculation logic.
  , ssLeiosCommittee :: LeiosCommittee
  -- ^ The Leios voting committee for the epoch this snapshot governs
  -- (CIP-0164), selected from 'ssStakePoolsSnapShot'. Empty before Dijkstra.
  -- Lazy on purpose, so that pre-Dijkstra eras never do the work, since it
  -- depends only on the pool snapshot.
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs SnapShot
  deriving (NoThunks) via AllowThunksIn '["ssLeiosCommittee"] SnapShot

instance NFData SnapShot

instance EncCBOR SnapShot where
  encCBOR ss@(SnapShot _ _ _ _) =
    let SnapShot {..} = ss
     in ifEncodingVersionAtLeast
          (natVersion @12)
          ( encodeListLen 3
              <> encCBOR ssActiveStake
              -- `ssTotalActiveStake` is ommitted on purpose
              <> encCBOR ssStakePoolsSnapShot
              <> encCBOR ssLeiosCommittee
          )
          ( encodeListLen 2
              <> encCBOR ssActiveStake
              <> encCBOR ssStakePoolsSnapShot
          )

instance DecShareCBOR SnapShot where
  type Share SnapShot = (Interns (Credential Staking), Interns (KeyHash StakePool))
  decSharePlusCBOR = do
    dijkstraOnwards <- lift $ (>= natVersion @12) <$> getDecoderVersion
    n <- lift decodeListLen
    case n of
      2 -> do
        -- New format: [ActiveStake, StakePoolsSnapShot]
        activeStake <- decSharePlusLensCBOR _1
        (stakeCredInterns, stakePoolIdInterns) <- get
        stakePoolsSnapShot <-
          lift $ decodeVMap (interns stakePoolIdInterns <$> decCBOR) (decShareCBOR stakeCredInterns)
        pure $ mkSnapShot 0 activeStake stakePoolsSnapShot
      3 | dijkstraOnwards -> do
        -- Dijkstra format: [ActiveStake, StakePoolsSnapShot, LeiosCommittee]
        activeStake <- decSharePlusLensCBOR _1
        (stakeCredInterns, stakePoolIdInterns) <- get
        stakePoolsSnapShot <-
          lift $ decodeVMap (interns stakePoolIdInterns <$> decCBOR) (decShareCBOR stakeCredInterns)
        ssLeiosCommittee <- lift decCBOR
        pure (mkSnapShot 0 activeStake stakePoolsSnapShot) {ssLeiosCommittee}
      3 -> do
        -- Old format: [Stake, Delegations, StakePoolsSnapShot]
        oldStake <- decSharePlusLensCBOR _1
        (oldDelegations :: VMap VB VB (Credential Staking) (KeyHash StakePool)) <-
          decSharePlusCBOR
        (stakeCredInterns, stakePoolIdInterns) <- get
        stakePoolsSnapShot <-
          lift $ decodeVMap (interns stakePoolIdInterns <$> decCBOR) (decShareCBOR stakeCredInterns)
        let activeStake =
              ActiveStake $
                VMap.fromDistinctAscList
                  [ (cred, StakeWithDelegation (unsafeNonZero cc) deleg)
                  | (cred, cc) <- VMap.toAscList $ unStake oldStake
                  , Just deleg <- [VMap.lookup cred oldDelegations]
                  ]
        pure $ mkSnapShot 0 activeStake stakePoolsSnapShot
      _ -> lift $ fail $ "Expected 2 or 3 fields for SnapShot, got " <> show n

instance ToKeyValuePairs SnapShot where
  toKeyValuePairs ss@(SnapShot _ _ _ _) =
    let SnapShot {..} = ss
     in [ "activeStake" .= ssActiveStake
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
emptySnapShot = SnapShot (ActiveStake VMap.empty) (knownNonZeroCoin @1) mempty emptyLeiosCommittee

emptySnapShots :: SnapShots
emptySnapShots =
  SnapShots emptySnapShot (calculatePoolDistr emptySnapShot) emptySnapShot emptySnapShot (Coin 0)

mkSnapShot ::
  -- | Size of the Leios voting committee to seat from these pools (CIP-0164).
  -- Zero before Dijkstra, so the committee is empty and, being lazy, costs
  -- nothing there.
  Word16 ->
  ActiveStake ->
  VMap VB VB (KeyHash StakePool) StakePoolSnapShot ->
  SnapShot
mkSnapShot committeeSize ssActiveStake ssStakePoolsSnapShot =
  let ssTotalActiveStake = sumAllActiveStake ssActiveStake
   in SnapShot
        { ssActiveStake
        , ssTotalActiveStake
        , ssStakePoolsSnapShot
        , ssLeiosCommittee = selectLeiosCommittee committeeSize (leiosCandidates ssStakePoolsSnapShot)
        }
{-# INLINE mkSnapShot #-}

ssLeiosCommitteeL :: Lens' SnapShot LeiosCommittee
ssLeiosCommitteeL = lens ssLeiosCommittee (\ss u -> ss {ssLeiosCommittee = u})

-- | Project each stake pool in a snapshot to its standing for Leios committee
-- selection (CIP-0164). 'selectLeiosCommittee' ranks and seats these.
leiosCandidates ::
  VMap VB VB (KeyHash StakePool) StakePoolSnapShot -> V.Vector LeiosCandidate
leiosCandidates = V.map (uncurry toCandidate) . VG.convert . unVMap
  where
    toCandidate poolId spss =
      LeiosCandidate poolId (spssStake spss) (spssStakeRatio spss) (spssLeiosKey spss)

-- | Completely overwrite the @StakePoolSnapShot@ from the given stake pools
-- state, and reseat the Leios voting committee from it (CIP-0164).
resetStakePoolsSnapShot ::
  -- | Size of the Leios voting committee to seat. Zero before Dijkstra.
  Word16 ->
  VMap.VMap VMap.VB VMap.VB (KeyHash StakePool) StakePoolState ->
  SnapShot ->
  SnapShot
resetStakePoolsSnapShot committeeSize stakePoolsState ss@SnapShot {..} =
  let pools = VMap.map (mkStakePoolSnapShot ssActiveStake ssTotalActiveStake) stakePoolsState
   in ss
        { ssStakePoolsSnapShot = pools
        , ssLeiosCommittee = selectLeiosCommittee committeeSize (leiosCandidates pools)
        }
{-# INLINE resetStakePoolsSnapShot #-}

snapShotFromInstantStake ::
  forall era.
  EraStake era =>
  -- | Size of the Leios voting committee to seat. Zero before Dijkstra.
  Word16 ->
  InstantStake era ->
  DState era ->
  PState era ->
  SnapShot
snapShotFromInstantStake committeeSize instantStake dState PState {psStakePools} =
  resetStakePoolsSnapShot committeeSize (VMap.fromMap psStakePools) $
    mkSnapShot committeeSize activeStake VMap.empty
  where
    activeStake = resolveInstantStake instantStake $ dsAccounts dState
{-# INLINE snapShotFromInstantStake #-}

-- =======================================

-- | Sum up the Coin (as CompactForm Coin = Word64) for each StakePool
calculatePoolStake ::
  (KeyHash StakePool -> Bool) ->
  ActiveStake ->
  Map.Map (KeyHash StakePool) (CompactForm Coin)
calculatePoolStake includeHash (ActiveStake m) = VMap.foldlWithKey accum Map.empty m
  where
    accum ans _cred swd =
      if includeHash $ swdDelegation swd
        then Map.insertWith (<>) (swdDelegation swd) (unNonZero $ swdStake swd) ans
        else ans

calculatePoolDistr :: SnapShot -> PoolDistr
calculatePoolDistr = calculatePoolDistr' (const True)

calculatePoolDistr' :: (KeyHash StakePool -> Bool) -> SnapShot -> PoolDistr
calculatePoolDistr' includeHash (SnapShot _ activeStake stakePoolSnapShot _) =
  let toIndividualPoolStake poolId spss = do
        guard (includeHash poolId)
        guard (spssNumDelegators spss > 0)
        Just
          IndividualPoolStake
            { individualPoolStake = spssStakeRatio spss
            , individualTotalPoolStake = spssStake spss
            , individualPoolStakeVrf = spssVrf spss
            , individualPoolStakeBls = spssLeiosKey spss
            }
      poolDistr =
        PoolDistr
          { unPoolDistr = VMap.toMap $ VMap.mapMaybeWithKey toIndividualPoolStake stakePoolSnapShot
          , pdTotalActiveStake = activeStake
          }
   in poolDistr

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

ssActiveStakeL :: Lens' SnapShot ActiveStake
ssActiveStakeL = lens ssActiveStake (\ds u -> ds {ssActiveStake = u})

ssStake :: SnapShot -> ActiveStake
ssStake = ssActiveStake
{-# DEPRECATED ssStake "In favor of `ssActiveStake`" #-}

ssStakeL :: Lens' SnapShot ActiveStake
ssStakeL = lens ssActiveStake (\ds u -> ds {ssActiveStake = u})
{-# DEPRECATED ssStakeL "In favor of `ssActiveStakeL`" #-}
