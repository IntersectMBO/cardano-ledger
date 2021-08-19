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
    SnapEvent,
  )
where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
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
import GHC.Generics (Generic)
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))

data SNAP era

data SnapPredicateFailure era -- No predicate failures
  deriving (Show, Generic, Eq)

instance NoThunks (SnapPredicateFailure era)

data SnapEvent era
  = StakeDistEvent !(SnapShot (Crypto era))

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

  tellEvent $ StakeDistEvent stake

  pure $
    s
      { _pstakeMark = stake,
        _pstakeSet = _pstakeMark s,
        _pstakeGo = _pstakeSet s,
        _feeSS = fees
      }
