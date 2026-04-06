{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
  module Cardano.Ledger.Api.State.Query.Snapshot,
) where

import Cardano.Ledger.Api.State.Query.Epoch
import Cardano.Ledger.Api.State.Query.Governance
import Cardano.Ledger.Api.State.Query.PParams
import Cardano.Ledger.Api.State.Query.Snapshot
import Cardano.Ledger.BaseTypes (EpochNo, Network)
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
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
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
