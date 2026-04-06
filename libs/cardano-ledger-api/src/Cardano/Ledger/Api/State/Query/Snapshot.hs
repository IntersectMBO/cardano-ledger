{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Api.State.Query.Snapshot (
  -- * Stable query result types
  QueryResultStakeSnapshot (..),
  QueryResultStakeSnapshots (..),

  -- * Deprecated aliases
  StakeSnapshot,
  StakeSnapshots,

  -- * Queries
  queryStakeSnapshots,
) where

import Cardano.Ledger.BaseTypes (NonZero, natVersion, pvMajor)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (ppProtocolVersionL)
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  curPParamsEpochStateL,
  esSnapshots,
  nesEs,
  nesEsL,
 )
import Cardano.Ledger.State (
  EraGov,
  SnapShots (..),
  spssNumDelegators,
  spssStake,
  ssStakePoolsSnapShot,
  ssTotalActiveStake,
 )
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Aeson (ToJSON)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

-- | Per-pool stake snapshot across mark, set, and go ledger snapshots.
data QueryResultStakeSnapshot = QueryResultStakeSnapshot
  { qrssMarkPool :: !Coin
  , qrssSetPool :: !Coin
  , qrssGoPool :: !Coin
  }
  deriving (Eq, Ord, Show, Generic)

deriving instance ToJSON QueryResultStakeSnapshot

instance NFData QueryResultStakeSnapshot

instance NoThunks QueryResultStakeSnapshot

instance EncCBOR QueryResultStakeSnapshot where
  encCBOR (QueryResultStakeSnapshot markPool setPool goPool) =
    encode $
      Rec QueryResultStakeSnapshot
        !> To markPool
        !> To setPool
        !> To goPool

instance DecCBOR QueryResultStakeSnapshot where
  decCBOR =
    decode $
      RecD QueryResultStakeSnapshot
        <! From
        <! From
        <! From

type StakeSnapshot = QueryResultStakeSnapshot

{-# DEPRECATED StakeSnapshot "Use QueryResultStakeSnapshot instead" #-}

-- | Stake snapshots for all pools plus total active stake per snapshot.
data QueryResultStakeSnapshots = QueryResultStakeSnapshots
  { qrssStakeSnapshots :: !(Map (KeyHash StakePool) QueryResultStakeSnapshot)
  , qrssMarkTotal :: !(NonZero Coin)
  , qrssSetTotal :: !(NonZero Coin)
  , qrssGoTotal :: !(NonZero Coin)
  }
  deriving (Eq, Ord, Show, Generic)

deriving instance ToJSON QueryResultStakeSnapshots

instance NFData QueryResultStakeSnapshots

instance NoThunks QueryResultStakeSnapshots

instance EncCBOR QueryResultStakeSnapshots where
  encCBOR (QueryResultStakeSnapshots stakeSnapshots markTotal setTotal goTotal) =
    encode $
      Rec QueryResultStakeSnapshots
        !> To stakeSnapshots
        !> To markTotal
        !> To setTotal
        !> To goTotal

instance DecCBOR QueryResultStakeSnapshots where
  decCBOR =
    decode $
      RecD QueryResultStakeSnapshots
        <! From
        <! From
        <! From
        <! From

type StakeSnapshots = QueryResultStakeSnapshots

{-# DEPRECATED StakeSnapshots "Use QueryResultStakeSnapshots instead" #-}

-- | Report stake per pool per snapshot as well as total active stake per snapshot.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:451
--   answerPureBlockQuery case for GetStakeSnapshots
--
-- Each snapshot is taken at an epoch boundary. The mark snapshot is the most recent, taken
-- immediately before the start of the current epoch. The set snapshot is one epoch older,
-- and the go snapshot is the oldest (two epochs back), used for reward calculation and
-- leader election.
--
-- /Note/ - The behavior of this function depends on the protocol version:
--
-- __Protocol Version < 11:__
--
-- * Empty 'Set': collects pools that have at least one delegator in any of the three
--   snapshots. Pools with delegators but zero stake are still included (with zero-valued
--   entries).
-- * Specific pool IDs: results are returned for exactly those pools, even if they have
--   no stake or are not registered (with zero-valued entries).
--
-- __Protocol Version >= 11:__
--
-- * Empty 'Set': collects pools with non-zero stake in any of the three snapshots.
-- * Specific pool IDs: pools with zero stake across all three snapshots are filtered out
--   and will not appear in the result, even if explicitly requested.
queryStakeSnapshots ::
  EraGov era =>
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  QueryResultStakeSnapshots
queryStakeSnapshots nes requestedPoolIds =
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
          QueryResultStakeSnapshot
            { qrssMarkPool = maybe mempty fromCompact markPoolStake
            , qrssSetPool = maybe mempty fromCompact setPoolStake
            , qrssGoPool = maybe mempty fromCompact goPoolStake
            }
      mkStakeSnapshot poolId =
        let
          lookupStake =
            maybe mempty (fromCompact . spssStake) . VMap.lookup poolId . ssStakePoolsSnapShot
         in
          QueryResultStakeSnapshot
            { qrssMarkPool = lookupStake ssStakeMark
            , qrssSetPool = lookupStake ssStakeSet
            , qrssGoPool = lookupStake ssStakeGo
            }
      version = pvMajor (nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL)
      poolIds =
        if Set.null requestedPoolIds
          then
            if version < natVersion @11
              then
                foldMap
                  (VMap.keysSet . VMap.filter (\_ -> (> 0) . spssNumDelegators) . ssStakePoolsSnapShot)
                  [ssStakeMark, ssStakeSet, ssStakeGo]
              else
                foldMap
                  (VMap.keysSet . VMap.filter (\_ -> (> mempty) . spssStake) . ssStakePoolsSnapShot)
                  [ssStakeMark, ssStakeSet, ssStakeGo]
          else requestedPoolIds
   in QueryResultStakeSnapshots
        { qrssStakeSnapshots =
            if version < natVersion @11
              then Map.fromSet mkStakeSnapshot poolIds
              else Map.mapMaybe id $ Map.fromSet mkStakeSnapshotMaybe poolIds
        , qrssMarkTotal = ssTotalActiveStake ssStakeMark
        , qrssSetTotal = ssTotalActiveStake ssStakeSet
        , qrssGoTotal = ssTotalActiveStake ssStakeGo
        }
