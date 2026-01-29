{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: use better names for record names
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cardano.Ledger.Shelley.API.Wallet (
  -- * UTxOs
  getUTxO,
  getUTxOSubset,
  getFilteredUTxO,

  -- * Stake Pools
  getPools,
  getTotalStake,
  poolsByTotalStakeFraction,
  RewardInfoPool (..),
  RewardParams (..),
  getRewardInfoPools,
  getRewardProvenance,
  getNonMyopicMemberRewards,

  -- * Transaction helpers
  addKeyWitnesses,

  -- * Ada pots
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
  getStakePools,
) where

import Cardano.Ledger.Address (compactAddr)
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Network,
  NonNegativeInterval,
  UnitInterval,
  epochInfoPure,
  unNonZero,
  (%?),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeDouble,
  encodeDouble,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (WitVKey (..))
import Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  RewardUpdate,
  UTxOState (..),
  circulation,
  createRUpd,
  curPParamsEpochStateL,
  esLStateL,
  lsCertStateL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.PoolRank (
  NonMyopic (..),
  PerformanceEstimate (..),
  calcNonMyopicMemberReward,
  getTopRankedPools,
  percentile',
 )
import Cardano.Ledger.Shelley.RewardProvenance (RewardProvenance)
import Cardano.Ledger.Shelley.Rewards (StakeShare (..))
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (epochInfoSize)
import qualified Cardano.Ledger.State as EB
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.Slot (EpochSize)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (runReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- UTxOs
--------------------------------------------------------------------------------

-- | Get the full UTxO.
getUTxO ::
  NewEpochState era ->
  UTxO era
getUTxO = utxosUtxo . lsUTxOState . esLState . nesEs

-- | Get the UTxO filtered by address.
getFilteredUTxO ::
  EraTxOut era =>
  NewEpochState era ->
  Set Addr ->
  UTxO era
getFilteredUTxO ss addrSet =
  UTxO $ Map.filter checkAddr fullUTxO
  where
    UTxO fullUTxO = getUTxO ss
    compactAddrSet = Set.map compactAddr addrSet
    checkAddr out =
      case out ^. addrEitherTxOutL of
        Left addr -> addr `Set.member` addrSet
        Right cAddr -> cAddr `Set.member` compactAddrSet
{-# INLINEABLE getFilteredUTxO #-}

getUTxOSubset ::
  NewEpochState era ->
  Set TxIn ->
  UTxO era
getUTxOSubset nes = txInsFilter (getUTxO nes)

--------------------------------------------------------------------------------
-- Stake pools and pool rewards
--------------------------------------------------------------------------------

-- | Get the /current/ registered stake pools.
getPools ::
  EraCertState era =>
  NewEpochState era ->
  Set (KeyHash StakePool)
getPools = Map.keysSet . f
  where
    f nes = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL

-- | Get the /current/ registered stake pool state for a given set of
-- stake pools. The result map will contain entries for all the given stake
-- pools that are currently registered.
getStakePools ::
  EraCertState era =>
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  Map (KeyHash StakePool) StakePoolState
getStakePools = Map.restrictKeys . f
  where
    f nes = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL

-- | Get pool sizes, but in terms of total stake
--
-- The stake distribution uses active stake (so that the leader schedule is not
-- affected by undelegated stake), but the wallet wants to display pool
-- saturation for rewards purposes. For that, it needs the fraction of total
-- stake.
--
-- The fields `individualTotalPoolStake` and `pdTotalActiveStake` continue to
-- remain based on active stake and not total stake.
--
-- This is not based on any snapshot, but uses the current ledger state.
poolsByTotalStakeFraction ::
  (EraStake era, EraCertState era) =>
  Globals ->
  NewEpochState era ->
  PoolDistr
poolsByTotalStakeFraction globals nes =
  PoolDistr poolsByTotalStake totalActiveStake
  where
    network = networkId globals
    snap = currentSnapshot nes network
    Coin totalStake = getTotalStake globals nes
    stakeRatio = unCoin (unNonZero totalActiveStake) %? totalStake
    PoolDistr poolsByActiveStake totalActiveStake = calculatePoolDistr snap
    poolsByTotalStake = Map.map toTotalStakeFrac poolsByActiveStake
    toTotalStakeFrac ::
      IndividualPoolStake ->
      IndividualPoolStake
    toTotalStakeFrac (IndividualPoolStake s c vrf) =
      IndividualPoolStake (s * stakeRatio) c vrf

-- | Calculate the current total stake.
getTotalStake :: Globals -> NewEpochState era -> Coin
getTotalStake globals ss =
  let supply = Coin . fromIntegral $ maxLovelaceSupply globals
      es = nesEs ss
   in circulation es supply

-- | Calculate the Non-Myopic Pool Member Rewards for a set of credentials.
-- For each given credential, this function returns a map from each stake
-- pool (identified by the key hash of the pool operator) to the
-- non-myopic pool member reward for that stake pool.
--
-- This is not based on any snapshot, but uses the current ledger state.
getNonMyopicMemberRewards ::
  (EraGov era, EraStake era, EraCertState era) =>
  Globals ->
  NewEpochState era ->
  Set (Either Coin (Credential Staking)) ->
  Map
    (Either Coin (Credential Staking))
    (Map (KeyHash StakePool) Coin)
getNonMyopicMemberRewards globals ss = Map.fromSet nmmRewards
  where
    maxSupply = Coin . fromIntegral $ maxLovelaceSupply globals
    totalStakeCoin@(Coin totalStake) = circulation es maxSupply
    toShare (Coin x) = StakeShare $ x %? totalStake
    memShare (Right cred) =
      toShare $ maybe mempty fromCompact $ VMap.lookup cred (EB.unStake stake)
    memShare (Left coin) = toShare coin
    es = nesEs ss
    pp = es ^. curPParamsEpochStateL
    NonMyopic {likelihoodsNM = ls, rewardPotNM = rPot} = esNonMyopic es
    network = networkId globals
    EB.SnapShot stake _ _ _ stakePoolsSnapShot = currentSnapshot ss network
    calcNMMRewards t poolId spss
      | spssPledge <= spssSelfDelegatedOwnersStake =
          calcNonMyopicMemberReward pp rPot poolId spssCost spssMargin s sigma t topPools hitRateEst
      | otherwise = mempty
      where
        StakePoolSnapShot {spssSelfDelegatedOwnersStake, spssPledge, spssCost, spssMargin} = spss
        s = toShare spssPledge
        hitRateEst = percentile' (histLookup poolId)
        sigma = toShare (fromCompact (spssStake spss))

    nmmRewards cred = VMap.toMap $ VMap.mapWithKey (calcNMMRewards $ memShare cred) stakePoolsSnapShot
    histLookup k = VMap.findWithDefault mempty k ls
    topPools =
      getTopRankedPools rPot totalStakeCoin pp $
        Map.intersectionWith (,) (VMap.toMap (VMap.map percentile' ls)) $
          VMap.toMap stakePoolsSnapShot

-- | Create a current snapshot of the ledger state.
--
-- When ranking pools, and reporting their saturation level, in the wallet, we
-- do not want to use one of the regular snapshots, but rather the most recent
-- ledger state.
currentSnapshot :: (EraStake era, EraCertState era) => NewEpochState era -> Network -> EB.SnapShot
currentSnapshot nes =
  snapShotFromInstantStake instantStake dstate pstate
  where
    ledgerState = esLState $ nesEs nes
    instantStake = ledgerState ^. instantStakeG
    dstate = ledgerState ^. lsCertStateL . certDStateL
    pstate = ledgerState ^. lsCertStateL . certPStateL

-- | Information about a stake pool
data RewardInfoPool = RewardInfoPool
  { stake :: Coin
  -- ^ Absolute stake delegated to this pool
  , ownerPledge :: Coin
  -- ^ Pledge of pool owner(s)
  , ownerStake :: Coin
  -- ^ Absolute stake delegated by pool owner(s)
  , cost :: Coin
  -- ^ Pool cost
  , margin :: UnitInterval
  -- ^ Pool margin
  , performanceEstimate :: Double
  -- ^ Number of blocks produced divided by expected number of blocks.
  -- Can be larger than @1.0@ for pool that gets lucky.
  -- (If some pools get unlucky, some pools must get lucky.)
  }
  deriving (Eq, Show, Generic)

instance NoThunks RewardInfoPool

instance NFData RewardInfoPool

deriving instance FromJSON RewardInfoPool

deriving instance ToJSON RewardInfoPool

-- | Global information that influences stake pool rewards
data RewardParams = RewardParams
  { nOpt :: Word16
  -- ^ Desired number of stake pools
  , a0 :: NonNegativeInterval
  -- ^ Influence of the pool owner's pledge on rewards
  , rPot :: Coin
  -- ^ Total rewards available for the given epoch
  , totalStake :: Coin
  -- ^ Maximum lovelace supply minus treasury
  }
  deriving (Eq, Show, Generic)

instance NoThunks RewardParams

instance NFData RewardParams

deriving instance FromJSON RewardParams

deriving instance ToJSON RewardParams

-- | Retrieve the information necessary to calculate stake pool member rewards
-- from the /current/ stake distribution.
--
-- This information includes the current stake distribution aggregated
-- by stake pools and pool owners,
-- the `current` pool costs and margins,
-- and performance estimates.
-- Also included are global information such as
-- the total stake or protocol parameters.
getRewardInfoPools ::
  (EraGov era, EraStake era, EraCertState era) =>
  Globals ->
  NewEpochState era ->
  (RewardParams, Map (KeyHash StakePool) RewardInfoPool)
getRewardInfoPools globals nes =
  (rewardParams, VMap.toMap $ VMap.mapWithKey mkRewardInfoPool ssStakePoolsSnapShot)
  where
    es = nesEs nes
    pp = es ^. curPParamsEpochStateL
    NonMyopic
      { likelihoodsNM = ls
      , rewardPotNM = rPot
      } = esNonMyopic es
    histLookup poolId = VMap.findWithDefault mempty poolId ls
    network = networkId globals

    EB.SnapShot {ssStakePoolsSnapShot} = currentSnapshot nes network

    rewardParams =
      RewardParams
        { a0 = pp ^. ppA0L
        , nOpt = pp ^. ppNOptL
        , totalStake = getTotalStake globals nes
        , rPot = rPot
        }
    mkRewardInfoPool poolId StakePoolSnapShot {..} =
      RewardInfoPool
        { stake = fromCompact spssStake
        , ownerStake = spssSelfDelegatedOwnersStake
        , ownerPledge = spssPledge
        , margin = spssMargin
        , cost = spssCost
        , performanceEstimate =
            unPerformanceEstimate $ percentile' $ histLookup poolId
        }

-- | Calculate stake pool rewards from the snapshot labeled `go`.
-- Also includes information on how the rewards were calculated
-- ('RewardProvenance').
--
-- For a calculation of rewards based on the current stake distribution,
-- see 'getRewardInfoPools'.
--
-- TODO: Deprecate 'getRewardProvenance', because wallets are more
-- likely to use 'getRewardInfoPools' for up-to-date information
-- on stake pool rewards.
getRewardProvenance ::
  forall era.
  (EraGov era, EraCertState era) =>
  Globals ->
  NewEpochState era ->
  (RewardUpdate, RewardProvenance)
getRewardProvenance globals newEpochState =
  ( runReader
      (createRUpd slotsPerEpoch blocksMade epochState maxSupply asc secparam)
      globals
  , def
  )
  where
    epochState = nesEs newEpochState
    maxSupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksMade = nesBprev newEpochState
    epochNo = nesEL newEpochState
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = epochInfoSize (epochInfoPure globals) epochNo
    asc = activeSlotCoeff globals
    secparam = securityParameter globals

--------------------------------------------------------------------------------
-- Transaction helpers
--------------------------------------------------------------------------------

addKeyWitnesses :: EraTx era => Tx t era -> Set (WitVKey Witness) -> Tx t era
addKeyWitnesses tx newWits = tx & witsTxL . addrTxWitsL %~ Set.union newWits

--------------------------------------------------------------------------------
-- CBOR instances
--------------------------------------------------------------------------------

instance EncCBOR RewardParams where
  encCBOR (RewardParams p1 p2 p3 p4) =
    encode $
      Rec RewardParams
        !> To p1
        !> To p2
        !> To p3
        !> To p4

instance DecCBOR RewardParams where
  decCBOR =
    decode $
      RecD RewardParams
        <! From
        <! From
        <! From
        <! From

instance EncCBOR RewardInfoPool where
  encCBOR (RewardInfoPool p1 p2 p3 p4 p5 d6) =
    encode $
      Rec RewardInfoPool
        !> To p1
        !> To p2
        !> To p3
        !> To p4
        !> To p5
        !> E encodeDouble d6

instance DecCBOR RewardInfoPool where
  decCBOR =
    decode $
      RecD RewardInfoPool
        <! From
        <! From
        <! From
        <! From
        <! From
        <! D decodeDouble
