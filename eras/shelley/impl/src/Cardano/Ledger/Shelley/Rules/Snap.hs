{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Snap
  ( SNAP,
    PredicateFailure,
    SnapPredicateFailure,
    SnapEvent (..),
  )
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    LedgerState (..),
    UTxOState (..),
    incrementalStakeDistr,
  )
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    tellEvent,
  )
import qualified Data.Compact.VMap as VMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- ======================================================

data SNAP era

data SnapPredicateFailure era -- No predicate failures
  deriving (Show, Generic, Eq)

instance NoThunks (SnapPredicateFailure era)

data SnapEvent era
  = StakeDistEvent !(Map (Credential 'Staking (Crypto era)) (Coin, (KeyHash 'StakePool (Crypto era))))

instance (UsesTxOut era, UsesValue era) => STS (SNAP era) where
  type State (SNAP era) = SnapShots (Crypto era)
  type Signal (SNAP era) = ()
  type Environment (SNAP era) = LedgerState era
  type BaseM (SNAP era) = ShelleyBase
  type PredicateFailure (SNAP era) = SnapPredicateFailure era
  type Event (SNAP era) = SnapEvent era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

-- | The stake distribution was previously computed as in the spec:
--
-- @
--  stakeDistr @era utxo dstate pstate
-- @
--
-- but is now computed incrementally. We leave the comment as a historical note about
-- where important changes were made to the source code.
snapTransition ::
  forall era.
  TransitionRule (SNAP era)
snapTransition = do
  TRC (lstate, s, _) <- judgmentContext

  let LedgerState (UTxOState _utxo _ fees _ incStake) (DPState dstate pstate) = lstate
      -- stakeSnap = stakeDistr @era utxo dstate pstate  -- HISTORICAL NOTE
      istakeSnap = incrementalStakeDistr @(Crypto era) incStake dstate pstate

  tellEvent $
    let stMap :: Map (Credential 'Staking (Crypto era)) (CompactForm Coin)
        stMap = VMap.toMap . unStake $ _stake istakeSnap

        stakeCoinMap :: Map (Credential 'Staking (Crypto era)) Coin
        stakeCoinMap = fmap fromCompact stMap

        stakePoolMap :: Map (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era))
        stakePoolMap = VMap.toMap $ _delegations istakeSnap

        stakeMap :: Map (Credential 'Staking (Crypto era)) (Coin, (KeyHash 'StakePool (Crypto era)))
        stakeMap = Map.intersectionWith (,) stakeCoinMap stakePoolMap
     in StakeDistEvent stakeMap

  pure $
    s
      { _pstakeMark = istakeSnap,
        _pstakeSet = _pstakeMark s,
        _pstakeGo = _pstakeSet s,
        _feeSS = fees
      }
