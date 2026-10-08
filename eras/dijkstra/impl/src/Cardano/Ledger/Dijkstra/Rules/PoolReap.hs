{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.PoolReap (
  POOLREAP,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, POOLREAP)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyPoolreapEvent (..),
  ShelleyPoolreapState (..),
  poolReapAssertions,
  renderPoolReapViolation,
 )
import Cardano.Ledger.State
import Cardano.Ledger.Val ((<+>), (<->))
import Control.State.Transition (
  Assertion (..),
  AssertionViolation (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  tellEvent,
 )
import Data.Default (Default)
import Data.Foldable (fold)
import Data.Foldable as F (foldl')
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Data.Word (Word64)
import Lens.Micro

-- The `POOLREAP` rule of the Dijkstra era mirrors the Shelley one, except for how it keeps
-- `psVRFKeyHashes` and `psBlsKeyHashes` in sync with the registered stake pools. Dropping
-- the VRF key hashes that a re-registration supersedes, the way Shelley does, loses the
-- references that other pools still hold to the same hash. Instead, every pool that
-- switches to a different VRF key hash releases a single reference to its active one,
-- just like every retired pool releases a single reference to the VRF key hash it uses.
-- The same goes for the BLS key hashes, except that a pool may have no BLS key, in which
-- case it holds no reference to release.
-- The `POOLREAP` rule type itself is declared in "Cardano.Ledger.Dijkstra.Era".
type instance EraRuleEvent "POOLREAP" DijkstraEra = ShelleyPoolreapEvent DijkstraEra

instance
  ( Default (ShelleyPoolreapState era)
  , EraPParams era
  , EraGov era
  , EraCertState era
  ) =>
  STS (POOLREAP era)
  where
  type State (POOLREAP era) = ShelleyPoolreapState era
  type Signal (POOLREAP era) = EpochNo
  type Environment (POOLREAP era) = ()
  type BaseM (POOLREAP era) = ShelleyBase
  type PredicateFailure (POOLREAP era) = Void
  type Event (POOLREAP era) = ShelleyPoolreapEvent era
  transitionRules = [poolReapTransition]

  renderAssertionViolation av =
    renderPoolReapViolation av
      <> foldMap renderVRFKeyHashCounts (avState av)
      <> foldMap renderBlsKeyHashCounts (avState av)
  assertions =
    poolReapAssertions
      <> [ PostCondition
             "VRF key hash counts must match those recomputed from the stake pools (PoolReap)"
             (\_trc -> uncurry (==) . vrfKeyHashCounts)
         , PostCondition
             "BLS key hash counts must match those recomputed from the stake pools (PoolReap)"
             (\_trc -> uncurry (==) . blsKeyHashCounts)
         ]

-- | The VRF key hash counts that @POOLREAP@ leaves behind, together with the ones recomputed
-- from the stake pools that remain registered. Its post-condition requires them to be equal.
vrfKeyHashCounts ::
  EraCertState era =>
  ShelleyPoolreapState era ->
  ( Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64)
  , Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64)
  )
vrfKeyHashCounts st = (psVRFKeyHashes ps, psVRFKeyHashes (populateVRFKeyHashes ps))
  where
    ps = prCertState st ^. certPStateL

renderVRFKeyHashCounts :: EraCertState era => ShelleyPoolreapState era -> String
renderVRFKeyHashCounts st =
  "\nVRF key hash counts (psVRFKeyHashes) = "
    <> show (unNonZero <$> counts)
    <> "\nVRF key hash counts recomputed from the stake pools (populateVRFKeyHashes) = "
    <> show (unNonZero <$> recomputed)
  where
    (counts, recomputed) = vrfKeyHashCounts st

-- | The BLS key hash counts that @POOLREAP@ leaves behind, together with the ones recomputed
-- from the stake pools that remain registered. Its post-condition requires them to be equal.
blsKeyHashCounts ::
  EraCertState era =>
  ShelleyPoolreapState era ->
  ( Map BlsVerKeyHash (NonZero Word64)
  , Map BlsVerKeyHash (NonZero Word64)
  )
blsKeyHashCounts st = (psBlsKeyHashes ps, psBlsKeyHashes (populateBlsKeyHashes ps))
  where
    ps = prCertState st ^. certPStateL

renderBlsKeyHashCounts :: EraCertState era => ShelleyPoolreapState era -> String
renderBlsKeyHashCounts st =
  "\nBLS key hash counts (psBlsKeyHashes) = "
    <> show (unNonZero <$> counts)
    <> "\nBLS key hash counts recomputed from the stake pools (populateBlsKeyHashes) = "
    <> show (unNonZero <$> recomputed)
  where
    (counts, recomputed) = blsKeyHashCounts st

poolReapTransition :: forall era. EraCertState era => TransitionRule (POOLREAP era)
poolReapTransition = do
  TRC (_, PoolreapState us a cs0, e) <- judgmentContext
  let
    ps0 = cs0 ^. certPStateL
    -- The active VRF key hash of every pool whose future parameters switch to a different
    -- one. A hash that several of these pools share is listed once for each of them.
    supersededVRFKeyHashes =
      Map.elems $
        Map.merge
          Map.dropMissing
          Map.dropMissing
          ( Map.zipWithMaybeMatched $ \_ sps sppF ->
              if sps ^. spsVrfL /= sppF ^. sppVrfL then Just (sps ^. spsVrfL) else Nothing
          )
          (ps0 ^. psStakePoolsL)
          (ps0 ^. psFutureStakePoolParamsL)
    activeBlsKeyHash :: StakePoolState -> Maybe BlsVerKeyHash
    activeBlsKeyHash sps = hashBlsKey . bksKey <$> strictMaybeToMaybe (sps ^. spsBlsKeyL)
    -- The active BLS key hash of every pool whose future parameters switch to a different
    -- one, which includes dropping the key. A hash that several of these pools share is
    -- listed once for each of them.
    supersededBlsKeyHashes =
      Map.elems $
        Map.merge
          Map.dropMissing
          Map.dropMissing
          ( Map.zipWithMaybeMatched $ \_ sps sppF ->
              let active = activeBlsKeyHash sps
               in if active /= (hashBlsKey <$> strictMaybeToMaybe (sppBlsKey sppF)) then active else Nothing
          )
          (ps0 ^. psStakePoolsL)
          (ps0 ^. psFutureStakePoolParamsL)

    -- activate future stakePools
    ps =
      ps0
        { psStakePools =
            Map.merge
              Map.dropMissing
              Map.preserveMissing
              ( Map.zipWithMatched $ \_ futureParams currentState ->
                  mkStakePoolState
                    e
                    (currentState ^. spsDepositL)
                    (currentState ^. spsDelegatorsL)
                    futureParams
              )
              (ps0 ^. psFutureStakePoolParamsL)
              (ps0 ^. psStakePoolsL)
        , psFutureStakePoolParams = Map.empty
        }
    cs = cs0 & certPStateL .~ ps

    ds = cs ^. certDStateL
    -- The set of pools retiring this epoch
    retired :: Set (KeyHash StakePool)
    retired = Set.fromDistinctAscList [k | (k, v) <- Map.toAscList (psRetiring ps), v == e]
    -- The Map of pools retiring this epoch
    retiringPools :: Map.Map (KeyHash StakePool) StakePoolState
    retiringPools = Map.restrictKeys (psStakePools ps) retired
    -- The VRF key hash of every pool retiring this epoch, once for each of them
    retiredVRFKeyHashes = spsVrf <$> Map.elems retiringPools
    -- The BLS key hash of every pool retiring this epoch that has a BLS key, once for each of them
    retiredBlsKeyHashes = mapMaybe activeBlsKeyHash (Map.elems retiringPools)

    -- Every pool releases a single reference to each VRF and BLS key hash it stops using,
    -- which keeps the invariant documented in "Cardano.Ledger.Dijkstra.Rules.Pool".
    vrfKeyHashes =
      F.foldl'
        (flip removeVRFKeyHashOccurrence)
        (psVRFKeyHashes ps0)
        (supersededVRFKeyHashes <> retiredVRFKeyHashes)
    blsKeyHashes =
      F.foldl'
        (flip removeBlsKeyHashOccurrence)
        (psBlsKeyHashes ps0)
        (supersededBlsKeyHashes <> retiredBlsKeyHashes)

    -- collect all of the potential refunds
    accountRefunds :: Map.Map (Credential Staking) (CompactForm Coin)
    accountRefunds =
      Map.fromListWith
        (<>)
        [(unAccountId $ spsAccountId sps, spsDeposit sps) | sps <- Map.elems retiringPools]
    accounts = ds ^. accountsL
    -- Deposits that can be refunded and those that are unclaimed (to be deposited into the treasury).
    refunds, unclaimedDeposits :: Map.Map (Credential Staking) (CompactForm Coin)
    (refunds, unclaimedDeposits) =
      Map.partitionWithKey
        (\stakeCred _ -> isAccountRegistered stakeCred accounts) -- (k ∈ dom (rewards ds))
        accountRefunds

    refunded = fold refunds
    unclaimed = fold unclaimedDeposits

  tellEvent $
    let rewardAccountsWithPool =
          Map.foldrWithKey'
            ( \poolId sps ->
                let cred = unAccountId $ spsAccountId sps
                 in Map.insertWith (Map.unionWith (<>)) cred (Map.singleton poolId (spsDeposit sps))
            )
            Map.empty
            retiringPools
        (refundPools', unclaimedPools') =
          Map.partitionWithKey
            (\cred _ -> isAccountRegistered cred accounts)
            rewardAccountsWithPool
     in RetiredPools
          { refundPools = refundPools'
          , unclaimedPools = unclaimedPools'
          , epochNo = e
          }
  pure $
    PoolreapState
      us {utxosDeposited = utxosDeposited us <-> fromCompact (unclaimed <> refunded)}
      a {casTreasury = casTreasury a <+> fromCompact unclaimed}
      ( cs
          & certDStateL . accountsL
            %~ removeStakePoolDelegations (delegsToClear cs retired)
              . addToBalanceAccounts refunds
          & certPStateL . psStakePoolsL %~ (`Map.withoutKeys` retired)
          & certPStateL . psRetiringL %~ (`Map.withoutKeys` retired)
          & certPStateL . psVRFKeyHashesL .~ vrfKeyHashes
          & certPStateL . psBlsKeyHashesL .~ blsKeyHashes
      )
  where
    delegsToClear cState pools =
      foldMap spsDelegators $
        Map.restrictKeys (cState ^. certPStateL . psStakePoolsL) pools
