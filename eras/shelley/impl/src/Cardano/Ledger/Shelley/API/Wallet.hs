{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: use better names for record names
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cardano.Ledger.Shelley.API.Wallet
  ( -- * UTxOs
    getUTxO,
    getUTxOSubset,
    getFilteredUTxO,

    -- * Stake Pools
    getPools,
    getPoolParameters,
    getTotalStake,
    poolsByTotalStakeFraction,
    RewardInfoPool (..),
    RewardParams (..),
    getRewardInfoPools,
    getRewardProvenance,
    getNonMyopicMemberRewards,

    -- * Transaction helpers
    CLI (..),
    evaluateTransactionBalance,
    addShelleyKeyWitnesses,
    -- -- * Ada pots
    AdaPots (..),
    totalAdaES,
    totalAdaPotsES,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeDouble,
    decodeFull,
    decodeFullDecoder,
    encodeDouble,
    serialize,
  )
import Cardano.Crypto.DSIGN.Class (decodeSignedDSIGN, sizeSigDSIGN, sizeVerKeyDSIGN)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes
  ( BlocksMade,
    Globals (..),
    NonNegativeInterval,
    ProtVer,
    UnitInterval,
    epochInfoPure,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress (compactAddr)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr
  ( IndividualPoolStake (..),
    PoolDistr (..),
  )
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots
  ( AdaPots (..),
    totalAdaES,
    totalAdaPotsES,
  )
import qualified Cardano.Ledger.Shelley.EpochBoundary as EB
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    RewardUpdate,
    UTxOState (..),
    circulation,
    consumed,
    createRUpd,
    incrementalStakeDistr,
    minfee,
    produced,
  )
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.PoolRank
  ( NonMyopic (..),
    PerformanceEstimate (..),
    getTopRankedPoolsVMap,
    nonMyopicMemberRew,
    percentile',
  )
import Cardano.Ledger.Shelley.RewardProvenance (RewardProvenance)
import Cardano.Ledger.Shelley.Rewards (StakeShare (..))
import Cardano.Ledger.Shelley.Rules.NewEpoch (calculatePoolDistr)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), ShelleyWitnesses, WitnessSetHKD (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (..), ShelleyEraTxBody, WitVKey (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Slot (epochInfoSize)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<->))
import Cardano.Slotting.Slot (EpochSize)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (runReader)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Coders
  ( Decode (..),
    Encode (..),
    decode,
    encode,
    (!>),
    (<!),
  )
import Data.Default.Class (Default (def))
import Data.Either (fromRight)
import Data.Foldable (foldMap')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import GHC.Records (HasField (..), getField)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- UTxOs
--------------------------------------------------------------------------------

-- | Get the full UTxO.
getUTxO ::
  NewEpochState era ->
  UTxO era
getUTxO = _utxo . lsUTxOState . esLState . nesEs

-- | Get the UTxO filtered by address.
getFilteredUTxO ::
  EraTxOut era =>
  NewEpochState era ->
  Set (Addr (Crypto era)) ->
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
  Set (TxIn (Crypto era)) ->
  UTxO era
getUTxOSubset ss txins =
  UTxO $ fullUTxO `Map.restrictKeys` txins
  where
    UTxO fullUTxO = getUTxO ss

--------------------------------------------------------------------------------
-- Stake pools and pool rewards
--------------------------------------------------------------------------------

-- | Get the /current/ registered stake pools.
getPools ::
  NewEpochState era ->
  Set (KeyHash 'StakePool (Crypto era))
getPools = Map.keysSet . f
  where
    f = _pParams . dpsPState . lsDPState . esLState . nesEs

-- | Get the /current/ registered stake pool parameters for a given set of
-- stake pools. The result map will contain entries for all the given stake
-- pools that are currently registered.
getPoolParameters ::
  NewEpochState era ->
  Set (KeyHash 'StakePool (Crypto era)) ->
  Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era))
getPoolParameters = Map.restrictKeys . f
  where
    f = _pParams . dpsPState . lsDPState . esLState . nesEs

-- | Get pool sizes, but in terms of total stake
--
-- The stake distribution uses active stake (so that the leader schedule is not
-- affected by undelegated stake), but the wallet wants to display pool
-- saturation for rewards purposes. For that, it needs the fraction of total
-- stake.
--
-- This is not based on any snapshot, but uses the current ledger state.
poolsByTotalStakeFraction ::
  forall era.
  Globals ->
  NewEpochState era ->
  PoolDistr (Crypto era)
poolsByTotalStakeFraction globals ss =
  PoolDistr poolsByTotalStake
  where
    snap@(EB.SnapShot stake _ _) = currentSnapshot ss
    Coin totalStake = getTotalStake globals ss
    Coin activeStake = EB.sumAllStake stake
    stakeRatio = activeStake % totalStake
    PoolDistr poolsByActiveStake = calculatePoolDistr snap
    poolsByTotalStake = Map.map toTotalStakeFrac poolsByActiveStake
    toTotalStakeFrac ::
      IndividualPoolStake (Crypto era) ->
      IndividualPoolStake (Crypto era)
    toTotalStakeFrac (IndividualPoolStake s vrf) =
      IndividualPoolStake (s * stakeRatio) vrf

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
  ( HasField "_a0" (PParams era) NonNegativeInterval,
    HasField "_nOpt" (PParams era) Natural
  ) =>
  Globals ->
  NewEpochState era ->
  Set (Either Coin (Credential 'Staking (Crypto era))) ->
  Map
    (Either Coin (Credential 'Staking (Crypto era)))
    (Map (KeyHash 'StakePool (Crypto era)) Coin)
getNonMyopicMemberRewards globals ss creds =
  Map.fromSet (\cred -> Map.map (mkNMMRewards $ memShare cred) poolData) creds
  where
    maxSupply = Coin . fromIntegral $ maxLovelaceSupply globals
    Coin totalStake = circulation es maxSupply
    toShare (Coin x) = StakeShare (x % totalStake)
    memShare (Right cred) =
      toShare $ maybe mempty fromCompact $ VMap.lookup cred (EB.unStake stake)
    memShare (Left coin) = toShare coin
    es = nesEs ss
    pp = esPp es
    NonMyopic {likelihoodsNM = ls, rewardPotNM = rPot} = esNonMyopic es
    EB.SnapShot stake delegs poolParams = currentSnapshot ss
    poolData =
      Map.fromDistinctAscList
        [ ( k,
            ( percentile' (histLookup k),
              p,
              toShare . EB.sumAllStake $ EB.poolStake k delegs stake
            )
          )
          | (k, p) <- VMap.toAscList poolParams
        ]
    histLookup k = Map.findWithDefault mempty k ls
    topPools =
      getTopRankedPoolsVMap
        rPot
        (Coin totalStake)
        pp
        poolParams
        (fmap percentile' ls)
    mkNMMRewards t (hitRateEst, poolp, sigma) =
      if checkPledge poolp
        then nonMyopicMemberRew pp rPot poolp s sigma t topPools hitRateEst
        else mempty
      where
        s = (toShare . _poolPledge) poolp
        checkPledge pool =
          let ostake = sumPoolOwnersStake pool stake
           in _poolPledge poolp <= ostake

sumPoolOwnersStake :: PoolParams crypto -> EB.Stake crypto -> Coin
sumPoolOwnersStake pool stake =
  let getStakeFor o =
        maybe mempty fromCompact $ VMap.lookup (KeyHashObj o) (EB.unStake stake)
   in foldMap' getStakeFor (_poolOwners pool)

-- | Create a current snapshot of the ledger state.
--
-- When ranking pools, and reporting their saturation level, in the wallet, we
-- do not want to use one of the regular snapshots, but rather the most recent
-- ledger state.
currentSnapshot :: NewEpochState era -> EB.SnapShot (Crypto era)
currentSnapshot ss =
  incrementalStakeDistr incrementalStake dstate pstate
  where
    ledgerState = esLState $ nesEs ss
    incrementalStake = _stakeDistro $ lsUTxOState ledgerState
    dstate = dpsDState $ lsDPState ledgerState
    pstate = dpsPState $ lsDPState ledgerState

-- | Information about a stake pool
data RewardInfoPool = RewardInfoPool
  { -- | Absolute stake delegated to this pool
    stake :: Coin,
    -- | Pledge of pool owner(s)
    ownerPledge :: Coin,
    -- | Absolute stake delegated by pool owner(s)
    ownerStake :: Coin,
    -- | Pool cost
    cost :: Coin,
    -- | Pool margin
    margin :: UnitInterval,
    -- | Number of blocks produced divided by expected number of blocks.
    -- Can be larger than @1.0@ for pool that gets lucky.
    -- (If some pools get unlucky, some pools must get lucky.)
    performanceEstimate :: Double
  }
  deriving (Eq, Show, Generic)

instance NoThunks RewardInfoPool

instance NFData RewardInfoPool

deriving instance FromJSON RewardInfoPool

deriving instance ToJSON RewardInfoPool

-- | Global information that influences stake pool rewards
data RewardParams = RewardParams
  { -- | Desired number of stake pools
    nOpt :: Natural,
    -- | Influence of the pool owner's pledge on rewards
    a0 :: NonNegativeInterval,
    -- | Total rewards available for the given epoch
    rPot :: Coin,
    -- | Maximum lovelace supply minus treasury
    totalStake :: Coin
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
  ( HasField "_a0" (PParams era) NonNegativeInterval,
    HasField "_nOpt" (PParams era) Natural
  ) =>
  Globals ->
  NewEpochState era ->
  (RewardParams, Map (KeyHash 'StakePool (Crypto era)) RewardInfoPool)
getRewardInfoPools globals ss =
  (mkRewardParams, VMap.toMap (VMap.mapWithKey mkRewardInfoPool poolParams))
  where
    es = nesEs ss
    pp = esPp es
    NonMyopic
      { likelihoodsNM = ls,
        rewardPotNM = rPot
      } = esNonMyopic es
    histLookup key = Map.findWithDefault mempty key ls

    EB.SnapShot stakes delegs poolParams = currentSnapshot ss

    mkRewardParams =
      RewardParams
        { a0 = getField @"_a0" pp,
          nOpt = getField @"_nOpt" pp,
          totalStake = getTotalStake globals ss,
          rPot = rPot
        }
    mkRewardInfoPool key poolp =
      RewardInfoPool
        { stake = pstake,
          ownerStake = ostake,
          ownerPledge = _poolPledge poolp,
          margin = _poolMargin poolp,
          cost = _poolCost poolp,
          performanceEstimate =
            unPerformanceEstimate $ percentile' $ histLookup key
        }
      where
        pstake = EB.sumAllStake $ EB.poolStake key delegs stakes
        ostake = sumPoolOwnersStake poolp stakes

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
  ( HasField "_a0" (PParams era) NonNegativeInterval,
    HasField "_d" (PParams era) UnitInterval,
    HasField "_nOpt" (PParams era) Natural,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_rho" (PParams era) UnitInterval,
    HasField "_tau" (PParams era) UnitInterval
  ) =>
  Globals ->
  NewEpochState era ->
  (RewardUpdate (Crypto era), RewardProvenance (Crypto era))
getRewardProvenance globals newepochstate =
  ( runReader
      (createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc secparam)
      globals,
    def
  )
  where
    epochstate = nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade (Crypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    asc = activeSlotCoeff globals
    secparam = securityParameter globals

--------------------------------------------------------------------------------
-- Transaction helpers
--------------------------------------------------------------------------------

-- | A collection of functons to help construction transactions
--  from the cardano-cli.
class
  ( EraTx era,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin
  ) =>
  CLI era
  where
  -- | The minimum fee calculation.
  -- Used for the default implentation of 'evaluateTransactionFee'.
  evaluateMinFee :: PParams era -> Tx era -> Coin

  -- | The consumed calculation.
  -- Used for the default implentation of 'evaluateTransactionBalance'.
  evaluateConsumed :: PParams era -> UTxO era -> TxBody era -> Value era

  addKeyWitnesses :: Tx era -> Set (WitVKey 'Witness (Crypto era)) -> Tx era

  -- | Evaluate the fee for a given transaction.
  evaluateTransactionFee ::
    -- | The current protocol parameters.
    PParams era ->
    -- | The transaction.
    Tx era ->
    -- | The number of key witnesses still to be added to the transaction.
    Word ->
    -- | The required fee.
    Coin
  evaluateTransactionFee pp tx numKeyWits =
    evaluateMinFee @era pp tx'
    where
      sigSize = fromIntegral $ sizeSigDSIGN (Proxy @(DSIGN (Crypto era)))
      dummySig =
        fromRight
          (error "corrupt dummy signature")
          (decodeFullDecoder "dummy signature" decodeSignedDSIGN (serialize $ LBS.replicate sigSize 0))
      vkeySize = fromIntegral $ sizeVerKeyDSIGN (Proxy @(DSIGN (Crypto era)))
      dummyVKey w =
        let padding = LBS.replicate paddingSize 0
            paddingSize = vkeySize - LBS.length sw
            sw = serialize w
            keyBytes = serialize $ padding <> sw
         in fromRight (error "corrupt dummy vkey") (decodeFull keyBytes)
      dummyKeyWits = Set.fromList $
        flip map [1 .. numKeyWits] $
          \x -> WitVKey (dummyVKey x) dummySig

      tx' = addKeyWitnesses @era tx dummyKeyWits

  -- | Evaluate the minimum lovelace that a given transaction output must contain.
  evaluateMinLovelaceOutput :: PParams era -> TxOut era -> Coin

-- | Evaluate the difference between the value currently being consumed by
-- a transaction and the number of lovelace being produced.
-- This value will be zero for a valid transaction.
evaluateTransactionBalance ::
  (CLI era, ShelleyEraTxBody era) =>
  -- | The current protocol parameters.
  PParams era ->
  -- | The UTxO relevant to the transaction.
  UTxO era ->
  -- | A predicate that a stake pool ID is new (i.e. unregistered).
  -- Typically this will be:
  --
  -- @
  --   (`Map.notMember` stakepools)
  -- @
  (KeyHash 'StakePool (Crypto era) -> Bool) ->
  -- | The transaction being evaluated for balance.
  TxBody era ->
  -- | The difference between what the transaction consumes and what it produces.
  Value era
evaluateTransactionBalance pp u isNewPool txb =
  evaluateConsumed pp u txb <-> produced pp isNewPool txb

--------------------------------------------------------------------------------
-- Shelley specifics
--------------------------------------------------------------------------------

addShelleyKeyWitnesses ::
  (EraTx era, TxWits era ~ ShelleyWitnesses era) =>
  ShelleyTx era ->
  Set (WitVKey 'Witness (Crypto era)) ->
  ShelleyTx era
addShelleyKeyWitnesses (ShelleyTx b ws aux) newWits = ShelleyTx b ws' aux
  where
    ws' = ws {addrWits = Set.union newWits (addrWits ws)}

instance CC.Crypto c => CLI (ShelleyEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses = addShelleyKeyWitnesses

  evaluateMinLovelaceOutput pp _out = _minUTxOValue pp

--------------------------------------------------------------------------------
-- CBOR instances
--------------------------------------------------------------------------------

instance ToCBOR RewardParams where
  toCBOR (RewardParams p1 p2 p3 p4) =
    encode $
      Rec RewardParams
        !> To p1
        !> To p2
        !> To p3
        !> To p4

instance FromCBOR RewardParams where
  fromCBOR =
    decode $
      RecD RewardParams
        <! From
        <! From
        <! From
        <! From

instance ToCBOR RewardInfoPool where
  toCBOR (RewardInfoPool p1 p2 p3 p4 p5 d6) =
    encode $
      Rec RewardInfoPool
        !> To p1
        !> To p2
        !> To p3
        !> To p4
        !> To p5
        !> E encodeDouble d6

instance FromCBOR RewardInfoPool where
  fromCBOR =
    decode $
      RecD RewardInfoPool
        <! From
        <! From
        <! From
        <! From
        <! From
        <! D decodeDouble
