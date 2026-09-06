{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  MarkSnapShot (..),
  SetSnapShot (..),
  GoSnapShot (..),
  mkSetSnapShot,
  mkGoSnapShot,
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
  msSnapShotL,
  ssSnapShotL,
  ssLeiosCommitteeL,
  gsSnapShotL,
) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  EpochInterval (..),
  EpochNo (..),
  KeyValuePairs (..),
  NonNegativeInterval,
  NonZero (..),
  StrictMaybe (..),
  ToKeyValuePairs (..),
  UnitInterval,
  addEpochInterval,
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
  selectLeiosCommittee,
 )
import Cardano.Ledger.State.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.State.Stake
import Cardano.Ledger.State.StakePool (BlsKeyState (..), StakePoolState (..))
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
import Lens.Micro (Lens', lens, (&), (^.), _1)
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
  MaxPledgeLeverage ->
  Coin
maxPool' a0 nOpt r sigma pR maxPledgeLeverage = rationalToCoinViaFloor $ factor1 * factor2
  where
    nonZeroZ0 = recipNonZero . toRatioNonZero $ toIntegerNonZero nOpt
    z0 = unNonZero nonZeroZ0
    sigma' =
      min sigma z0 -- original Shelley behavior
        & case unMaxPledgeLeverage maxPledgeLeverage of
          SNothing -> id
          SJust l -> min (unboundRational l * pR) -- Dijkstra CIP-50 behavior
    p' = min pR z0
    factor1 =
      -- This division is safe, because a0 is non-negative and we're adding one
      -- to it
      coinToRational r / (1 + unboundRational a0)
    factor2 = sigma' + p' * unboundRational a0 * factor3
    factor3 = (sigma' - p' * factor4) /. nonZeroZ0
    factor4 = (z0 - sigma') /. nonZeroZ0

-- | Version of `maxPool'` that extracts `ppA0L`, `ppNOptL` and `ppMaxPledgeLeverageG` from a `PParams`.
maxPool ::
  EraPParams era =>
  PParams era ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool pp r sigma pR = maxPool' a0 nOpt r sigma pR maxPledgeLeverage
  where
    a0 = pp ^. ppA0L
    nOpt = (pp ^. ppNOptL) `nonZeroOr` knownNonZeroBounded @1
    maxPledgeLeverage = pp ^. ppMaxPledgeLeverageG

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
  , spssBlsKey :: !(StrictMaybe BlsKeyState)
  -- ^ Corresponding field in the `StakePoolState` is `spsBlsKey`.
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
    , spssBlsKey = spsBlsKey
    , spssPledge = spsPledge
    , spssCost = spsCost
    , spssMargin = spsMargin
    , spssNumDelegators = Set.size spsDelegators
    , spssAccountId = spsAccountId
    }
  where
    StakePoolState
      { spsVrf
      , spsBlsKey
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
        , "blsKey" .= spssBlsKey
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
          <> encCBOR spssBlsKey
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
    spssBlsKey <- lift decCBOR
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
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs SnapShot

instance NFData SnapShot

instance NoThunks SnapShot

instance EncCBOR SnapShot where
  encCBOR ss@(SnapShot _ _ _) =
    let SnapShot {..} = ss
     in encodeListLen 2
          <> encCBOR ssActiveStake
          -- `ssTotalActiveStake` is ommitted on purpose
          <> encCBOR ssStakePoolsSnapShot

instance DecShareCBOR SnapShot where
  type Share SnapShot = (Interns (Credential Staking), Interns (KeyHash StakePool))
  decSharePlusCBOR = do
    n <- lift decodeListLen
    case n of
      2 -> do
        -- New format: [ActiveStake, StakePoolsSnapShot]
        activeStake <- decSharePlusLensCBOR _1
        (stakeCredInterns, stakePoolIdInterns) <- get
        stakePoolsSnapShot <-
          lift $ decodeVMap (interns stakePoolIdInterns <$> decCBOR) (decShareCBOR stakeCredInterns)
        pure $ mkSnapShot activeStake stakePoolsSnapShot
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
        pure $ mkSnapShot activeStake stakePoolsSnapShot
      _ -> lift $ fail $ "Expected 2 or 3 fields for SnapShot, got " <> show n

instance ToKeyValuePairs SnapShot where
  toKeyValuePairs ss@(SnapShot _ _ _) =
    let SnapShot {..} = ss
     in [ "activeStake" .= ssActiveStake
        , "stakePoolsSnapShot" .= ssStakePoolsSnapShot
        ]

-- | The freshest snapshot, taken at the boundary into 'msEpochNo'. Alongside
-- the stake standing it records the inputs that the Leios committee will be
-- selected from when this snapshot rotates into 'SetSnapShot'.
data MarkSnapShot = MarkSnapShot
  { msSnapShot :: SnapShot
  -- ^ Lazy on purpose. See ADR-7.
  , msEpochNo :: !EpochNo
  -- ^ Epoch number at the beginning of which this snapshot was created.
  , msLeiosCommitteeSize :: !Word16
  -- ^ Size of the Leios voting committee as set by @ppLeiosCommitteeSize@ upon
  -- snapshot creation (CIP-0164). Zero before Dijkstra.
  , msMaxKeyAge :: !EpochInterval
  -- ^ Maximum age a Leios voting key stays honoured, judged against the epoch
  -- the committee is active (CIP-0164). Recorded here rather than recomputed at
  -- rotation because it is derived from 'Cardano.Ledger.BaseTypes.Globals',
  -- which the pure snapshot rotation cannot read. Zero before Dijkstra.
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs MarkSnapShot

instance NFData MarkSnapShot

deriving via AllowThunksIn '["msSnapShot"] MarkSnapShot instance NoThunks MarkSnapShot

instance EncCBOR MarkSnapShot where
  encCBOR ms@(MarkSnapShot _ _ _ _) =
    let MarkSnapShot {..} = ms
     in encodeListLen 4
          <> encCBOR msSnapShot
          <> encCBOR msEpochNo
          <> encCBOR msLeiosCommitteeSize
          <> encCBOR msMaxKeyAge

instance DecShareCBOR MarkSnapShot where
  type Share MarkSnapShot = Share SnapShot
  decSharePlusCBOR = decodeRecordNamedT "MarkSnapShot" (const 4) $ do
    msSnapShot <- decSharePlusCBOR
    msEpochNo <- lift decCBOR
    msLeiosCommitteeSize <- lift decCBOR
    msMaxKeyAge <- lift decCBOR
    pure MarkSnapShot {msSnapShot, msEpochNo, msLeiosCommitteeSize, msMaxKeyAge}

instance ToKeyValuePairs MarkSnapShot where
  toKeyValuePairs ms@(MarkSnapShot _ _ _ _) =
    let MarkSnapShot {..} = ms
     in [ "snapShot" .= msSnapShot
        , "epochNo" .= msEpochNo
        , "leiosCommitteeSize" .= msLeiosCommitteeSize
        , "maxKeyAge" .= msMaxKeyAge
        ]

-- | The snapshot that drives leader election ('Cardano.Ledger.State.PoolDistr')
-- and Leios voting for the epoch after the one it was marked in. The Leios
-- committee materializes here: it is fully derived from 'ssSnapShot' and the
-- inputs the mark recorded, so it is never serialized and rebuilding it after
-- decoding is free until forced.
data SetSnapShot = SetSnapShot
  { ssSnapShot :: !SnapShot
  , ssPoolDistr :: !PoolDistr
  , ssEpochNo :: !EpochNo
  -- ^ Epoch at the beginning of which the stake was snapshotted, carried over
  -- from 'msEpochNo'.
  , ssLeiosCommitteeSize :: !Word16
  -- ^ Committee size recorded at snapshot creation, carried over from
  -- 'msLeiosCommitteeSize'.
  , ssMaxKeyAge :: !EpochInterval
  -- ^ Maximum honoured Leios voting key age, carried over from 'msMaxKeyAge'.
  , ssLeiosCommittee :: LeiosCommittee
  -- ^ The Leios voting committee governing the epoch this snapshot is the
  -- leader-election distribution of (CIP-0164). Lazy on purpose: derived from
  -- the other fields, and pre-Dijkstra eras never force it.
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs SetSnapShot

instance NFData SetSnapShot

deriving via AllowThunksIn '["ssLeiosCommittee"] SetSnapShot instance NoThunks SetSnapShot

instance EncCBOR SetSnapShot where
  encCBOR ss@(SetSnapShot _ _ _ _ _ _) =
    let SetSnapShot {..} = ss
     in -- `ssPoolDistr` and `ssLeiosCommittee` are omitted on purpose: both are
        -- derived from the fields that are serialized.
        encodeListLen 4
          <> encCBOR ssSnapShot
          <> encCBOR ssEpochNo
          <> encCBOR ssLeiosCommitteeSize
          <> encCBOR ssMaxKeyAge

instance DecShareCBOR SetSnapShot where
  type Share SetSnapShot = Share SnapShot
  decSharePlusCBOR = decodeRecordNamedT "SetSnapShot" (const 4) $ do
    snapShot <- decSharePlusCBOR
    epochNo <- lift decCBOR
    committeeSize <- lift decCBOR
    maxKeyAge <- lift decCBOR
    pure $
      mkSetSnapShot
        (calculatePoolDistr snapShot)
        MarkSnapShot
          { msSnapShot = snapShot
          , msEpochNo = epochNo
          , msLeiosCommitteeSize = committeeSize
          , msMaxKeyAge = maxKeyAge
          }

instance ToKeyValuePairs SetSnapShot where
  toKeyValuePairs ss@(SetSnapShot _ _ _ _ _ _) =
    let SetSnapShot {..} = ss
     in [ "snapShot" .= ssSnapShot
        , "epochNo" .= ssEpochNo
        , "leiosCommitteeSize" .= ssLeiosCommitteeSize
        , "maxKeyAge" .= ssMaxKeyAge
        , "leiosCommittee" .= ssLeiosCommittee
        ]

-- | The oldest snapshot, consumed by the reward calculation.
data GoSnapShot = GoSnapShot
  { gsSnapShot :: !SnapShot
  , gsPoolDistr :: !PoolDistr
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs GoSnapShot

instance NFData GoSnapShot

instance NoThunks GoSnapShot

instance EncCBOR GoSnapShot where
  encCBOR gs@(GoSnapShot _ _) =
    let GoSnapShot {..} = gs
     in -- `gsPoolDistr` is omitted on purpose: it is derived from the snapshot.
        encodeListLen 1
          <> encCBOR gsSnapShot

instance DecShareCBOR GoSnapShot where
  type Share GoSnapShot = Share SnapShot
  decSharePlusCBOR = decodeRecordNamedT "GoSnapShot" (const 1) $ do
    gsSnapShot <- decSharePlusCBOR
    pure GoSnapShot {gsSnapShot, gsPoolDistr = calculatePoolDistr gsSnapShot}

instance ToKeyValuePairs GoSnapShot where
  toKeyValuePairs gs@(GoSnapShot _ _) =
    let GoSnapShot {..} = gs
     in ["snapShot" .= gsSnapShot]

-- | Rotate a mark snapshot into the set position, seating the Leios voting
-- committee from the stake standing and inputs the mark recorded (CIP-0164).
mkSetSnapShot ::
  -- | The pool distribution of the mark snapshot; passed in so the memoized
  -- 'ssStakeMarkPoolDistr' is reused at the epoch boundary. See ADR-7.
  PoolDistr ->
  MarkSnapShot ->
  SetSnapShot
mkSetSnapShot poolDistr MarkSnapShot {msSnapShot, msEpochNo, msLeiosCommitteeSize, msMaxKeyAge} =
  SetSnapShot
    { ssSnapShot = msSnapShot
    , ssPoolDistr = poolDistr
    , ssEpochNo = msEpochNo
    , ssLeiosCommitteeSize = msLeiosCommitteeSize
    , ssMaxKeyAge = msMaxKeyAge
    , ssLeiosCommittee =
        selectLeiosCommittee
          -- The mark records the epoch it was created in; when it rotates into
          -- the set position it is the leader-election stake distribution for
          -- the next epoch, which is the epoch a voting key is honoured against
          -- (CIP-0164).
          (addEpochInterval msEpochNo (EpochInterval 1))
          msMaxKeyAge
          msLeiosCommitteeSize
          (leiosCandidates (ssStakePoolsSnapShot msSnapShot))
    }

-- | Rotate a set snapshot into the go position.
mkGoSnapShot :: SetSnapShot -> GoSnapShot
mkGoSnapShot SetSnapShot {ssSnapShot, ssPoolDistr} =
  GoSnapShot {gsSnapShot = ssSnapShot, gsPoolDistr = ssPoolDistr}

-- | Snapshots of the stake distribution.
--
-- Note that ssStakeMark and ssStakeMarkPoolDistr are lazy on
-- purpose since we only want to force the thunk after one stability window
-- when we know that they are stable (so that we do not compute them if we do not have to).
-- See more info in the [Optimize TICKF ADR](https://github.com/intersectmbo/cardano-ledger/blob/master/docs/adr/2022-12-12_007-optimize-ledger-view.md)
data SnapShots era = SnapShots
  { ssStakeMark :: MarkSnapShot -- Lazy on purpose
  , ssStakeMarkPoolDistr :: PoolDistr -- Lazy on purpose
  , ssStakeSet :: !SetSnapShot
  , ssStakeGo :: !GoSnapShot
  , ssFee :: !Coin
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs (SnapShots era)
  -- TODO: switch `AllowThunksIn` to `OnlyCheckWhnfNamed`
  deriving (NoThunks) via AllowThunksIn '["ssStakeMark", "ssStakeMarkPoolDistr"] (SnapShots era)

instance NFData (SnapShots era)

instance EncCBOR (SnapShots era) where
  encCBOR (SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}) =
    encodeListLen 4
      <> encCBOR ssStakeMark
      -- We intentionaly do not serialize the redundant ssStakeMarkPoolDistr
      <> encCBOR ssStakeSet
      <> encCBOR ssStakeGo
      <> encCBOR ssFee

instance Era era => DecCBOR (SnapShots era) where
  decCBOR = decNoShareCBOR

instance DecShareCBOR (SnapShots era) where
  type Share (SnapShots era) = Share SnapShot
  decSharePlusCBOR = decodeRecordNamedT "SnapShots" (const 4) $ do
    !ssStakeMark <- decSharePlusCBOR
    ssStakeSet <- decSharePlusCBOR
    ssStakeGo <- decSharePlusCBOR
    ssFee <- lift decCBOR
    let ssStakeMarkPoolDistr = calculatePoolDistr (msSnapShot ssStakeMark)
    pure SnapShots {ssStakeMark, ssStakeMarkPoolDistr, ssStakeSet, ssStakeGo, ssFee}

instance Default (SnapShots era) where
  def = emptySnapShots

instance ToKeyValuePairs (SnapShots era) where
  toKeyValuePairs ss@(SnapShots !_ _ _ _ _) =
    -- ssStakeMarkPoolDistr is omitted on purpose
    let SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee} = ss
     in [ "pstakeMark" .= ssStakeMark
        , "pstakeSet" .= ssStakeSet
        , "pstakeGo" .= ssStakeGo
        , "feeSS" .= ssFee
        ]

emptySnapShot :: SnapShot
emptySnapShot = SnapShot (ActiveStake VMap.empty) (knownNonZeroCoin @1) mempty

emptySnapShots :: SnapShots era
emptySnapShots =
  SnapShots emptyMark (calculatePoolDistr emptySnapShot) emptySet emptyGo (Coin 0)
  where
    emptyMark = MarkSnapShot emptySnapShot (EpochNo 0) 0 (EpochInterval 0)
    emptySet = mkSetSnapShot (calculatePoolDistr emptySnapShot) emptyMark
    emptyGo = mkGoSnapShot emptySet

mkSnapShot ::
  ActiveStake ->
  VMap VB VB (KeyHash StakePool) StakePoolSnapShot ->
  SnapShot
mkSnapShot ssActiveStake ssStakePoolsSnapShot =
  let ssTotalActiveStake = sumAllActiveStake ssActiveStake
   in SnapShot {ssActiveStake, ssTotalActiveStake, ssStakePoolsSnapShot}
{-# INLINE mkSnapShot #-}

-- | Project each stake pool in a snapshot to its standing for Leios committee
-- selection (CIP-0164). 'selectLeiosCommittee' ranks and seats these.
leiosCandidates ::
  VMap VB VB (KeyHash StakePool) StakePoolSnapShot -> V.Vector LeiosCandidate
leiosCandidates = V.map (uncurry toCandidate) . VG.convert . unVMap
  where
    toCandidate poolId spss =
      LeiosCandidate poolId (spssStake spss) (spssStakeRatio spss) (spssBlsKey spss)

-- | Given stake pools state and SnapShot completely overwrite the StakePoolsSnapShot
resetStakePoolsSnapShot ::
  VMap.VMap VMap.VB VMap.VB (KeyHash StakePool) StakePoolState ->
  SnapShot ->
  SnapShot
resetStakePoolsSnapShot stakePoolsState ss@SnapShot {..} =
  ss
    { ssStakePoolsSnapShot =
        VMap.map (mkStakePoolSnapShot ssActiveStake ssTotalActiveStake) stakePoolsState
    }
{-# INLINE resetStakePoolsSnapShot #-}

snapShotFromInstantStake ::
  forall era.
  EraStake era =>
  InstantStake era ->
  DState era ->
  PState era ->
  SnapShot
snapShotFromInstantStake instantStake dState PState {psStakePools} =
  resetStakePoolsSnapShot (VMap.fromMap psStakePools) $
    mkSnapShot activeStake VMap.empty
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
calculatePoolDistr' includeHash (SnapShot _ activeStake stakePoolSnapShot) =
  let toIndividualPoolStake poolId spss = do
        guard (includeHash poolId)
        guard (spssNumDelegators spss > 0)
        Just
          IndividualPoolStake
            { individualPoolStake = spssStakeRatio spss
            , individualTotalPoolStake = spssStake spss
            , individualPoolStakeVrf = spssVrf spss
            , individualPoolStakeBls = bksKey <$> spssBlsKey spss
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

ssStakeMarkL :: Lens' (SnapShots era) MarkSnapShot
ssStakeMarkL = lens ssStakeMark (\ds u -> ds {ssStakeMark = u})

ssStakeMarkPoolDistrL :: Lens' (SnapShots era) PoolDistr
ssStakeMarkPoolDistrL = lens ssStakeMarkPoolDistr (\ds u -> ds {ssStakeMarkPoolDistr = u})

ssStakeSetL :: Lens' (SnapShots era) SetSnapShot
ssStakeSetL = lens ssStakeSet (\ds u -> ds {ssStakeSet = u})

ssStakeGoL :: Lens' (SnapShots era) GoSnapShot
ssStakeGoL = lens ssStakeGo (\ds u -> ds {ssStakeGo = u})

ssFeeL :: Lens' (SnapShots era) Coin
ssFeeL = lens ssFee (\ds u -> ds {ssFee = u})

-- MarkSnapShot / SetSnapShot / GoSnapShot

msSnapShotL :: Lens' MarkSnapShot SnapShot
msSnapShotL = lens msSnapShot (\ms u -> ms {msSnapShot = u})

ssSnapShotL :: Lens' SetSnapShot SnapShot
ssSnapShotL = lens ssSnapShot (\ss u -> ss {ssSnapShot = u})

ssLeiosCommitteeL :: Lens' SetSnapShot LeiosCommittee
ssLeiosCommitteeL = lens ssLeiosCommittee (\ss u -> ss {ssLeiosCommittee = u})

gsSnapShotL :: Lens' GoSnapShot SnapShot
gsSnapShotL = lens gsSnapShot (\gs u -> gs {gsSnapShot = u})

-- SnapShot

ssActiveStakeL :: Lens' SnapShot ActiveStake
ssActiveStakeL = lens ssActiveStake (\ds u -> ds {ssActiveStake = u})

ssStake :: SnapShot -> ActiveStake
ssStake = ssActiveStake
{-# DEPRECATED ssStake "In favor of `ssActiveStake`" #-}

ssStakeL :: Lens' SnapShot ActiveStake
ssStakeL = lens ssActiveStake (\ds u -> ds {ssActiveStake = u})
{-# DEPRECATED ssStakeL "In favor of `ssActiveStakeL`" #-}
