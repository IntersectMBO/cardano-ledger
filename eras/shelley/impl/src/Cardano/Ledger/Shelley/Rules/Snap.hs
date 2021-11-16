{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Snap
  ( SNAP,
    PredicateFailure,
    SnapPredicateFailure,
    SnapEvent (..),
    StakeDistEvent_ (..),
  )
where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Compactible (fromCompact)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    LedgerState (..),
    UTxOState (..),
    stakeDistr,
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
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))

data SNAP era

data SnapPredicateFailure era -- No predicate failures
  deriving (Show, Generic, Eq)

instance NoThunks (SnapPredicateFailure era)

data SnapEvent era
  = StakeDistEvent (StakeDistEvent_ era)

data StakeDistEvent_ era = StakeDistEvent_
  { sdistStakeMap :: !(Map (Credential 'Staking (Crypto era)) (Coin, (KeyHash 'StakePool (Crypto era))))
  }
  deriving (Eq)

instance (UsesTxOut era, UsesValue era) => STS (SNAP era) where
  type State (SNAP era) = SnapShots (Crypto era)
  type Signal (SNAP era) = ()
  type Environment (SNAP era) = LedgerState era
  type BaseM (SNAP era) = ShelleyBase
  type PredicateFailure (SNAP era) = SnapPredicateFailure era
  type Event (SNAP era) = SnapEvent era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

snapTransition ::
  forall era.
  ( UsesValue era,
    HasField "address" (Core.TxOut era) (Addr (Crypto era))
  ) =>
  TransitionRule (SNAP era)
snapTransition = do
  TRC (lstate, s, _) <- judgmentContext

  let LedgerState (UTxOState utxo _ fees _) (DPState dstate pstate) = lstate
      stake :: SnapShot (Crypto era)
      stake = stakeDistr utxo dstate pstate

      stMap :: Map (Credential 'Staking (Crypto era)) (CompactForm Coin)
      stMap = VMap.toMap . unStake $ _stake stake

      stakeCoinMap :: Map (Credential 'Staking (Crypto era)) Coin
      stakeCoinMap = fmap fromCompact stMap

      stakePoolMap :: Map (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era))
      stakePoolMap = VMap.toMap $ _delegations stake

      stakeMap :: Map (Credential 'Staking (Crypto era)) (Coin, (KeyHash 'StakePool (Crypto era)))
      stakeMap = Map.intersectionWith (,) stakeCoinMap stakePoolMap

  tellEvent $
    StakeDistEvent $
      StakeDistEvent_
        { sdistStakeMap = stakeMap
        }

  pure $
    s
      { _pstakeMark = stake,
        _pstakeSet = _pstakeMark s,
        _pstakeGo = _pstakeSet s,
        _feeSS = fees
      }
