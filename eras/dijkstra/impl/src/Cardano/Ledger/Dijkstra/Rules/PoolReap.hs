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
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  tellEvent,
 )
import Data.Default (Default)
import Data.Foldable (fold)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Lens.Micro

-- The `POOLREAP` rule of the Dijkstra era mirrors the Shelley one, except for how it keeps
-- `psVRFKeyHashes` in sync with the registered stake pools. Dropping the VRF key hashes
-- that a re-registration supersedes, the way Shelley does, loses the references that
-- other pools still hold to the same hash. Instead, the reference counts are recomputed
-- once the future parameters have been adopted and the retired pools have been reaped.
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

  renderAssertionViolation = renderPoolReapViolation
  assertions = poolReapAssertions

poolReapTransition :: forall era. EraCertState era => TransitionRule (POOLREAP era)
poolReapTransition = do
  TRC (_, PoolreapState us a cs0, e) <- judgmentContext
  let
    ps0 = cs0 ^. certPStateL
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
          -- Adopting the future parameters and reaping the retired pools both change which
          -- VRF key hashes are in use. Rather than tracking those changes one by one,
          -- rebuild the reference counts from the pools that remain registered, per the
          -- invariant documented in "Cardano.Ledger.Dijkstra.Rules.Pool".
          & certPStateL %~ populateVRFKeyHashes
      )
  where
    delegsToClear cState pools =
      foldMap spsDelegators $
        Map.restrictKeys (cState ^. certPStateL . psStakePoolsL) pools
