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
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Lens.Micro

-- The `POOLREAP` rule of the Dijkstra era mirrors the Shelley one, except for how it keeps
-- `psVRFKeyHashes` and `psBlsKeyHashes` in sync with the registered stake pools. Dropping
-- the VRF key hashes that a re-registration supersedes, the way Shelley does, loses the
-- references that other pools still hold to the same hash. Instead, every pool that
-- switches to a different VRF key hash releases a single reference to its active one,
-- just like every retired pool releases a single reference to the VRF key hash it uses.
-- The BLS key hashes that pools stop using in the same way are removed from
-- `psBlsKeyHashes`.
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
    renderPoolReapViolation av <> foldMap renderKeyHashes (avState av)
  assertions =
    poolReapAssertions
      <> [ PostCondition
             "VRF and BLS key hashes must match those recomputed from the stake pools (PoolReap)"
             (\_trc -> uncurry (==) . recomputedKeyHashes)
         ]

-- | The pool state that @POOLREAP@ leaves behind, together with the same state with its VRF
-- and BLS key hashes recomputed from the stake pools that remain registered. Its
-- post-condition requires them to be equal.
recomputedKeyHashes :: EraCertState era => ShelleyPoolreapState era -> (PState era, PState era)
recomputedKeyHashes st = (ps, populateBlsKeyHashes (populateVRFKeyHashes ps))
  where
    ps = prCertState st ^. certPStateL

renderKeyHashes :: EraCertState era => ShelleyPoolreapState era -> String
renderKeyHashes st =
  "\nVRF key hash counts (psVRFKeyHashes) = "
    <> show (unNonZero <$> psVRFKeyHashes ps)
    <> "\nVRF key hash counts recomputed from the stake pools (populateVRFKeyHashes) = "
    <> show (unNonZero <$> psVRFKeyHashes recomputed)
    <> "\nBLS key hashes (psBlsKeyHashes) = "
    <> show (psBlsKeyHashes ps)
    <> "\nBLS key hashes recomputed from the stake pools (populateBlsKeyHashes) = "
    <> show (psBlsKeyHashes recomputed)
  where
    (ps, recomputed) = recomputedKeyHashes st

poolReapTransition :: forall era. EraCertState era => TransitionRule (POOLREAP era)
poolReapTransition = do
  TRC (_, PoolreapState us a cs0, e) <- judgmentContext
  let
    ps0 = cs0 ^. certPStateL
    -- The keys that pools stop using, as given by `activeKey` and `futureKey` for their
    -- active and future parameters: a pool whose future parameters change its key stops
    -- using the active one once they are adopted, and a retiring pool stops using the one
    -- it has by then. A key that several of these pools share is listed once for each of
    -- them.
    releasedKeys :: Eq k => (StakePoolState -> k) -> (StakePoolParams era -> k) -> [k]
    releasedKeys activeKey futureKey =
      Map.elems
        ( Map.merge
            Map.dropMissing
            Map.dropMissing
            ( Map.zipWithMaybeMatched $ \_ sps sppF ->
                let key = activeKey sps in if key /= futureKey sppF then Just key else Nothing
            )
            (ps0 ^. psStakePoolsL)
            (ps0 ^. psFutureStakePoolParamsL)
        )
        <> (activeKey <$> Map.elems retiringPools)

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
                    -- the pool keeps the epoch its voting key was registered in unless the re-registration actually rotates it
                    (currentState ^. spsBlsKeyL)
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

    -- Every pool releases a single reference to each VRF key hash it stops using, and the BLS
    -- key hashes that pools stop using are removed, which keeps the invariants documented in
    -- "Cardano.Ledger.Dijkstra.Rules.Pool".
    vrfKeyHashes =
      F.foldl'
        (flip removeVRFKeyHashOccurrence)
        (psVRFKeyHashes ps0)
        (releasedKeys spsVrf sppVrf)
    blsKeyHashes =
      psBlsKeyHashes ps0
        `Set.difference` Set.fromList
          [ blsKeyHash
          | SJust blsKeyHash <-
              releasedKeys (fmap (hashBlsKey . bksKey) . spsBlsKey) (fmap hashBlsKey . sppBlsKey)
          ]

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
