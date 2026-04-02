{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Api.State.Query (
  -- * @GetFilteredDelegationsAndRewardAccounts@
  queryStakePoolDelegsAndRewards,

  -- * @GetSPOStakeDistr@
  querySPOStakeDistr,

  -- * @GetChainAccountState@
  module Cardano.Ledger.Api.State.Query.Epoch,

  -- * Governance queries
  module Cardano.Ledger.Api.State.Query.Governance,

  -- * @GetCurrentPParams@ / @GetFuturePParams@
  module Cardano.Ledger.Api.State.Query.PParams,

  -- * @GetStakePoolDefaultVote@
  queryStakePoolDefaultVote,
  DefaultVote (..),

  -- * @GetPoolState@
  queryPoolParameters,
  queryPoolState,
  QueryPoolStateResult (..),
  mkQueryPoolStateResult,

  -- * @GetStakeSnapshots@
  queryStakeSnapshots,
  StakeSnapshot (..),
  StakeSnapshots (..),
) where

import Cardano.Ledger.Api.State.Query.Epoch
import Cardano.Ledger.Api.State.Query.Governance
import Cardano.Ledger.Api.State.Query.PParams
import Cardano.Ledger.BaseTypes (EpochNo, Network, NonZero, ProtVer (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  DefaultVote (..),
  defaultStakePoolVote,
  psPoolDistr,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState
import Control.DeepSeq
import Control.Monad (guard)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.VMap as VMap
import GHC.Generics
import Lens.Micro

-- | Implementation for @GetFilteredDelegationsAndRewardAccounts@ query.
queryStakePoolDelegsAndRewards ::
  EraCertState era =>
  NewEpochState era ->
  Set (Credential Staking) ->
  ( Map (Credential Staking) (KeyHash StakePool)
  , Map (Credential Staking) Coin
  )
queryStakePoolDelegsAndRewards nes creds =
  let accountsMap = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      accountsMapFiltered = accountsMap `Map.restrictKeys` creds
   in ( Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMapFiltered
      , Map.map (fromCompact . (^. balanceAccountStateL)) accountsMapFiltered
      )

-- | Query pool stake distribution.
querySPOStakeDistr ::
  ConwayEraGov era =>
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  -- | Specify pool key hashes whose stake distribution should be returned. When this set is
  -- empty, distributions for all of the pools will be returned.
  Map (KeyHash StakePool) Coin
querySPOStakeDistr nes keys
  | null keys = Map.map fromCompact distr
  | otherwise = Map.map fromCompact $ distr `Map.restrictKeys` keys
  where
    distr = psPoolDistr . fst $ finishedPulserState nes

-- | Query a stake pool's account address delegatee which determines the pool's default vote
-- in absence of an explicit vote. Note that this is different from the delegatee determined
-- by the credential of the stake pool itself.
queryStakePoolDefaultVote ::
  (EraCertState era, ConwayEraAccounts era) =>
  NewEpochState era ->
  -- | Specify the key hash of the pool whose default vote should be returned.
  KeyHash StakePool ->
  DefaultVote
queryStakePoolDefaultVote nes poolId =
  defaultStakePoolVote poolId (nes ^. nesEsL . epochStateStakePoolsL) $
    nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL

-- | Used only for the `queryPoolState` query. This resembles the older way of
-- representing StakePoolState in Ledger.
data QueryPoolStateResult = QueryPoolStateResult
  { qpsrStakePoolParams :: !(Map (KeyHash StakePool) StakePoolParams)
  , qpsrFutureStakePoolParams :: !(Map (KeyHash StakePool) StakePoolParams)
  , qpsrRetiring :: !(Map (KeyHash StakePool) EpochNo)
  , qpsrDeposits :: !(Map (KeyHash StakePool) Coin)
  }
  deriving (Show, Eq)

instance EncCBOR QueryPoolStateResult where
  encCBOR (QueryPoolStateResult a b c d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance DecCBOR QueryPoolStateResult where
  decCBOR = decodeRecordNamed "QueryPoolStateResult" (const 4) $ do
    qpsrStakePoolParams <- decCBOR
    qpsrFutureStakePoolParams <- decCBOR
    qpsrRetiring <- decCBOR
    qpsrDeposits <- decCBOR
    pure
      QueryPoolStateResult {qpsrStakePoolParams, qpsrFutureStakePoolParams, qpsrRetiring, qpsrDeposits}

mkQueryPoolStateResult ::
  (forall x. Map.Map (KeyHash StakePool) x -> Map.Map (KeyHash StakePool) x) ->
  PState era ->
  Network ->
  QueryPoolStateResult
mkQueryPoolStateResult f ps network =
  QueryPoolStateResult
    { qpsrStakePoolParams =
        Map.mapWithKey (stakePoolStateToStakePoolParams network) restrictedStakePools
    , qpsrFutureStakePoolParams = f $ psFutureStakePoolParams ps
    , qpsrRetiring = f $ psRetiring ps
    , qpsrDeposits = Map.map (fromCompact . spsDeposit) restrictedStakePools
    }
  where
    restrictedStakePools = f $ psStakePools ps

-- | Query the QueryPoolStateResult. This is slightly different from the internal
-- representation used by Ledger and is intended to resemble how the internal
-- representation used to be.
queryPoolState ::
  EraCertState era =>
  NewEpochState era -> Maybe (Set (KeyHash StakePool)) -> Network -> QueryPoolStateResult
queryPoolState nes mPoolKeys network =
  let pstate = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL
      f :: forall x. Map.Map (KeyHash StakePool) x -> Map.Map (KeyHash StakePool) x
      f = case mPoolKeys of
        Nothing -> id
        Just keys -> (`Map.restrictKeys` keys)
   in mkQueryPoolStateResult f pstate network

-- | Query the current StakePoolParams.
queryPoolParameters ::
  EraCertState era =>
  Network ->
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  Map (KeyHash StakePool) StakePoolParams
queryPoolParameters network nes poolKeys =
  let pools = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
   in Map.mapWithKey (stakePoolStateToStakePoolParams network) $ Map.restrictKeys pools poolKeys

-- | The stake snapshot returns information about the mark, set, go ledger snapshots for a pool,
-- plus the total active stake for each snapshot that can be used in a 'sigma' calculation.
--
-- Each snapshot is taken at the end of a different era. The go snapshot is the current one and
-- was taken two epochs earlier, set was taken one epoch ago, and mark was taken immediately
-- before the start of the current epoch.
data StakeSnapshot = StakeSnapshot
  { ssMarkPool :: !Coin
  , ssSetPool :: !Coin
  , ssGoPool :: !Coin
  }
  deriving (Eq, Show, Generic)

instance NFData StakeSnapshot

instance EncCBOR StakeSnapshot where
  encCBOR
    StakeSnapshot
      { ssMarkPool
      , ssSetPool
      , ssGoPool
      } =
      encodeListLen 3
        <> encCBOR ssMarkPool
        <> encCBOR ssSetPool
        <> encCBOR ssGoPool

instance DecCBOR StakeSnapshot where
  decCBOR = do
    enforceSize "StakeSnapshot" 3
    StakeSnapshot
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR

data StakeSnapshots = StakeSnapshots
  { ssStakeSnapshots :: !(Map (KeyHash StakePool) StakeSnapshot)
  , ssMarkTotal :: !(NonZero Coin)
  , ssSetTotal :: !(NonZero Coin)
  , ssGoTotal :: !(NonZero Coin)
  }
  deriving (Eq, Show, Generic)

instance NFData StakeSnapshots

instance EncCBOR StakeSnapshots where
  encCBOR
    StakeSnapshots
      { ssStakeSnapshots
      , ssMarkTotal
      , ssSetTotal
      , ssGoTotal
      } =
      encodeListLen 4
        <> encCBOR ssStakeSnapshots
        <> encCBOR ssMarkTotal
        <> encCBOR ssSetTotal
        <> encCBOR ssGoTotal

instance DecCBOR StakeSnapshots where
  decCBOR = do
    enforceSize "StakeSnapshots" 4
    StakeSnapshots
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR
      <*> decCBOR

-- | Report stake per pool per snapshot as well as total active stake per snapshot.
--
-- /Note/ - Whenever poolIds are not supplied, we collect all of the pools, even if they don't have
-- any delegations. Otherwise we filter out for exact poolIds that were supplied. In both cases it
-- means that there can be pools that have zero stake in all three snapshot, but the meaning of that
-- can be very different:
--
-- * either the pool has no delegations, or
-- * it was explicitly requested even though it has no stake or not even registered
--
-- However, starting with Protocol Version 11 we remove this strange inconsistency and only ever
-- report stake pools with non-zero stake, which means pools without delegations (hence without any stake in any of the three snapshots) are no longer included in the results.
queryStakeSnapshots ::
  EraGov era =>
  NewEpochState era ->
  Maybe (Set (KeyHash StakePool)) ->
  StakeSnapshots
queryStakeSnapshots nes mPoolIds =
  let SnapShots
        { ssStakeMark
        , ssStakeSet
        , ssStakeGo
        } = esSnapshots $ nesEs nes

      mkStakeSnapshotMaybe poolId = do
        let
          markPoolStake = spssStake <$> VMap.lookup poolId (ssStakePoolsSnapShot ssStakeMark)
          setPoolStake = spssStake <$> VMap.lookup poolId (ssStakePoolsSnapShot ssStakeSet)
          goPoolStake = spssStake <$> VMap.lookup poolId (ssStakePoolsSnapShot ssStakeGo)
        -- Non-registered stake pools or ones that have no stake are of no interest to us.
        guard (fold [markPoolStake, setPoolStake, goPoolStake] > Just mempty)
        Just
          StakeSnapshot
            { ssMarkPool = maybe mempty fromCompact markPoolStake
            , ssSetPool = maybe mempty fromCompact setPoolStake
            , ssGoPool = maybe mempty fromCompact goPoolStake
            }
      mkStakeSnapshot poolId =
        let
          lookupStake =
            maybe mempty (fromCompact . spssStake) . VMap.lookup poolId . ssStakePoolsSnapShot
         in
          StakeSnapshot
            { ssMarkPool = lookupStake ssStakeMark
            , ssSetPool = lookupStake ssStakeSet
            , ssGoPool = lookupStake ssStakeGo
            }
      version = pvMajor (nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL)
      poolIds =
        case mPoolIds of
          Nothing
            | version < natVersion @11 ->
                foldMap
                  (VMap.keysSet . VMap.filter (\_ -> (> 0) . spssNumDelegators) . ssStakePoolsSnapShot)
                  [ssStakeMark, ssStakeSet, ssStakeGo]
            | otherwise ->
                foldMap
                  (VMap.keysSet . VMap.filter (\_ -> (> mempty) . spssStake) . ssStakePoolsSnapShot)
                  [ssStakeMark, ssStakeSet, ssStakeGo]
          Just ids -> ids
   in StakeSnapshots
        { ssStakeSnapshots =
            if version < natVersion @11
              then Map.fromSet mkStakeSnapshot poolIds
              else Map.mapMaybe id $ Map.fromSet mkStakeSnapshotMaybe poolIds
        , ssMarkTotal = ssTotalActiveStake ssStakeMark
        , ssSetTotal = ssTotalActiveStake ssStakeSet
        , ssGoTotal = ssTotalActiveStake ssStakeGo
        }
