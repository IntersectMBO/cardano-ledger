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
  querySPOStakeDistr,
) where

import Cardano.Ledger.Api.State.Query.Governance (finishedPulserState)
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
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
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  DefaultVote (..),
  defaultStakePoolVote,
  psPoolDistr,
 )
import Cardano.Ledger.Conway.State (ConwayEraAccounts)
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  PState,
  certDStateL,
  certPStateL,
  epochStateStakePoolsL,
  esLStateL,
  lsCertStateL,
  nesEsL,
  psStakePoolsL,
 )
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Ledger.State (
  EraCertState,
  StakePoolParams,
  accountsL,
  psFutureStakePoolParams,
  psRetiring,
  psStakePools,
  spsDeposit,
  stakePoolStateToStakePoolParams,
 )
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
