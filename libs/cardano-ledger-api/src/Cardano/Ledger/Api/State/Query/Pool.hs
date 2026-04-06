{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Api.State.Query.Pool (
  -- * Stable query result types
  QueryResultPoolState (..),

  -- * Conversion
  toQueryResultPoolState,

  -- * Queries
  queryPoolState,
  queryStakePoolParams,
  queryStakePoolDefaultVote,
  queryStakePools,
  querySPOStakeDistr,
  queryStakePoolDistrByTotalSupply,
  queryStakePoolDistrFromSnapshot,
  queryStakePoolRelays,
) where

import Cardano.Ledger.Api.State.Query.Governance (finishedPulserState)
import Cardano.Ledger.Api.State.Query.Snapshot (queryCurrentSnapshot)
import Cardano.Ledger.BaseTypes (Network, unNonZero, (%?))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..), unCoin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  DefaultVote (..),
  defaultStakePoolVote,
  psPoolDistr,
 )
import Cardano.Ledger.Conway.State (ConwayEraAccounts)
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  PState,
  certDStateL,
  certPStateL,
  circulation,
  epochStateStakePoolsL,
  esLStateL,
  lsCertStateL,
  nesEs,
  nesEsL,
  psStakePoolsL,
 )
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Ledger.State (
  EraCertState,
  EraStake,
  IndividualPoolStake (..),
  PoolDistr (..),
  StakePoolParams,
  StakePoolRelay,
  accountsL,
  calculatePoolDistr,
  psFutureStakePoolParams,
  psRetiring,
  psStakePools,
  sppRelays,
  spsDeposit,
  spsRelays,
  stakePoolStateToStakePoolParams,
 )
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

-- | Result of 'queryPoolState'. Stable query result type for pool
-- state.
data QueryResultPoolState = QueryResultPoolState
  { qrpsStakePoolParams :: !(Map (KeyHash StakePool) StakePoolParams)
  , qrpsFutureStakePoolParams :: !(Map (KeyHash StakePool) StakePoolParams)
  , qrpsRetiring :: !(Map (KeyHash StakePool) EpochNo)
  , qrpsDeposits :: !(Map (KeyHash StakePool) Coin)
  }
  deriving (Show, Eq, Ord, Generic)

deriving instance ToJSON QueryResultPoolState

instance NFData QueryResultPoolState

instance NoThunks QueryResultPoolState

instance EncCBOR QueryResultPoolState where
  encCBOR (QueryResultPoolState stakePoolParams futureStakePoolParams retiring deposits) =
    encode $
      Rec QueryResultPoolState
        !> To stakePoolParams
        !> To futureStakePoolParams
        !> To retiring
        !> To deposits

instance DecCBOR QueryResultPoolState where
  decCBOR =
    decode $
      RecD QueryResultPoolState
        <! From
        <! From
        <! From
        <! From

-- | Convert 'PState' to 'QueryResultPoolState'.
toQueryResultPoolState ::
  (forall x. Map (KeyHash StakePool) x -> Map (KeyHash StakePool) x) ->
  PState era ->
  Network ->
  QueryResultPoolState
toQueryResultPoolState f ps network =
  QueryResultPoolState
    { qrpsStakePoolParams =
        Map.mapWithKey (stakePoolStateToStakePoolParams network) restrictedStakePools
    , qrpsFutureStakePoolParams = f $ psFutureStakePoolParams ps
    , qrpsRetiring = f $ psRetiring ps
    , qrpsDeposits = Map.map (fromCompact . spsDeposit) restrictedStakePools
    }
  where
    restrictedStakePools = f $ psStakePools ps

-- | Query stake pool state.
--
-- Empty 'Set' returns all pools.
queryPoolState ::
  EraCertState era =>
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  Network ->
  QueryResultPoolState
queryPoolState nes poolKeys network =
  let pstate = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL
      f :: forall x. Map (KeyHash StakePool) x -> Map (KeyHash StakePool) x
      f = if Set.null poolKeys then id else (`Map.restrictKeys` poolKeys)
   in toQueryResultPoolState f pstate network

-- | Query stake pool parameters.
--
-- Empty 'Set' returns all pools.
queryStakePoolParams ::
  EraCertState era =>
  Network ->
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  Map (KeyHash StakePool) StakePoolParams
queryStakePoolParams network nes poolKeys =
  let pools = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
      filtered
        | Set.null poolKeys = pools
        | otherwise = Map.restrictKeys pools poolKeys
   in Map.mapWithKey (stakePoolStateToStakePoolParams network) filtered

-- | Query a stake pool's default vote, determined by the DRep delegatee
-- of the pool's reward account. In the absence of an explicit vote,
-- this default vote is used. Note that this is different from the
-- delegatee determined by the credential of the stake pool itself.
queryStakePoolDefaultVote ::
  (EraCertState era, ConwayEraAccounts era) =>
  NewEpochState era ->
  -- | Specify the key hash of the pool whose default vote should be
  -- returned.
  KeyHash StakePool ->
  DefaultVote
queryStakePoolDefaultVote nes poolId =
  defaultStakePoolVote poolId (nes ^. nesEsL . epochStateStakePoolsL) $
    nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL

-- | Query pool stake distribution.
--
-- Empty 'Set' returns all pools.
querySPOStakeDistr ::
  ConwayEraGov era =>
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  Map (KeyHash StakePool) Coin
querySPOStakeDistr nes keys
  | null keys = Map.map fromCompact distr
  | otherwise = Map.map fromCompact $ distr `Map.restrictKeys` keys
  where
    distr = psPoolDistr . fst $ finishedPulserState nes

-- | Query the set of all currently registered stake pools.
--
-- Returns the key hashes of every pool in the current PState.
queryStakePools ::
  EraCertState era =>
  NewEpochState era ->
  Set (KeyHash StakePool)
queryStakePools nes =
  Map.keysSet $ nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL

-- | Query stake pool distribution with fractions relative to total ADA
-- supply.
--
-- Unlike 'queryStakePoolDistrFromSnapshot' (which gives fractions of
-- active stake only), this adjusts each pool's fractional stake to
-- reflect its share of the total supply including undelegated ADA. Used
-- by wallets to display pool saturation.
--
-- This query uses 'currentSnapshot' (the __mark__ snapshot
-- computed from the current ledger state), whereas
-- 'queryStakePoolDistrFromSnapshot' reads the memoized @nesPd@ (the
-- __set__ snapshot computed at the last epoch boundary).
--
-- Needs @maxLovelaceSupply@ from genesis config.
queryStakePoolDistrByTotalSupply ::
  (EraStake era, EraCertState era) =>
  -- | maxLovelaceSupply from genesis config
  Word64 ->
  NewEpochState era ->
  PoolDistr
queryStakePoolDistrByTotalSupply maxLovelaceSupply nes =
  PoolDistr poolsByTotalStake totalActiveStake
  where
    snap = queryCurrentSnapshot nes
    Coin totalStake =
      let supply = Coin (fromIntegral maxLovelaceSupply)
       in circulation (nesEs nes) supply
    stakeRatio = unCoin (unNonZero totalActiveStake) %? totalStake
    PoolDistr poolsByActiveStake totalActiveStake = calculatePoolDistr snap
    poolsByTotalStake = Map.map toTotalStakeFrac poolsByActiveStake
    toTotalStakeFrac (IndividualPoolStake s c vrf) =
      IndividualPoolStake (s * stakeRatio) c vrf

-- | Query the pool distribution derived from the set-snapshot.
--
-- Returns the pre-computed 'PoolDistr' stored in 'NewEpochState'
-- (@nesPd@), optionally filtered to the given set of pools. Empty set
-- returns all pools.
--
-- ==== Equivalence with consensus
--
-- Consensus computes this on every query as:
--
-- @calculatePoolDistr' predicate (ssStakeSet (esSnapshots epochState))@
--
-- We instead read @nesPd@, which is set at the epoch boundary
-- to @ssStakeMarkPoolDistr (esSnapshots es)@ where @es@
-- is the /pre-transition/ epoch state. Since SNAP rotates
-- @ssStakeMark@ → @ssStakeSet@, and @ssStakeMarkPoolDistr =
-- calculatePoolDistr ssStakeMark@, the memoized @nesPd@ is identical to
-- @calculatePoolDistr (ssStakeSet ...)@ on the post-transition epoch
-- state.
--
-- Filtering is applied /after/ reading the memoized distribution
-- rather than /during/ @calculatePoolDistr'@. This is equivalent
-- because @calculatePoolDistr'@ sets @pdTotalActiveStake@ from
-- the snapshot total independently of the filter, and each pool's
-- @IndividualPoolStake@ values depend only on that pool's data.
--
-- This approach is more efficient: the expensive @calculatePoolDistr@
-- computation happens once at the epoch boundary (ADR-7), and the query
-- performs only an O(n+m) @Map.restrictKeys@.
queryStakePoolDistrFromSnapshot ::
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  PoolDistr
queryStakePoolDistrFromSnapshot nes poolIds
  | Set.null poolIds = nesPd nes
  | otherwise =
      let pd = nesPd nes
       in pd {unPoolDistr = Map.restrictKeys (unPoolDistr pd) poolIds}

-- | Query pool relay information with associated stake fractions.
-- Returns pools that have at least one registered relay, combining
-- relays from both current and pending (future) pool registrations.
--
-- This provides the ledger-side data needed by consensus for
-- peer discovery (GetLedgerPeerSnapshot). Consensus applies
-- networking-specific transformations (relay type conversion, big-peer
-- stake accumulation) on top of this result.
queryStakePoolRelays ::
  EraCertState era =>
  NewEpochState era ->
  Map (KeyHash StakePool) (Rational, StrictSeq StakePoolRelay)
queryStakePoolRelays nes =
  Map.mapMaybeWithKey getRelays (unPoolDistr (nesPd nes))
  where
    pstate = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL
    pools = psStakePools pstate
    futureParams = psFutureStakePoolParams pstate
    getRelays poolId ips =
      let curRelays = maybe mempty spsRelays (Map.lookup poolId pools)
          futRelays = maybe mempty sppRelays (Map.lookup poolId futureParams)
          allRelays = curRelays <> futRelays
       in if null allRelays
            then Nothing
            else Just (individualPoolStake ips, allRelays)
