{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Snap
  ( SNAP,
    PredicateFailure,
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
  )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    LedgerState (..),
    UTxOState (..),
    stakeDistr,
  )
import Shelley.Spec.Ledger.Value

data SNAP crypto v

instance (CV crypto v) => STS (SNAP crypto v) where
  type State (SNAP crypto v) = SnapShots crypto
  type Signal (SNAP crypto v) = ()
  type Environment (SNAP crypto v) = LedgerState crypto v
  type BaseM (SNAP crypto v) = ShelleyBase
  data PredicateFailure (SNAP crypto v) -- No predicate failures
    deriving (Show, Generic, Eq)

  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

instance NoUnexpectedThunks (PredicateFailure (SNAP crypto v))

snapTransition :: CV crypto v => TransitionRule (SNAP crypto v)
snapTransition = do
  TRC (lstate, s, ()) <- judgmentContext

  let LedgerState (UTxOState utxo _ fees _) (DPState dstate pstate) = lstate
      stake = stakeDistr utxo dstate pstate
  pure $
    s
      { _pstakeMark = stake,
        _pstakeSet = _pstakeMark s,
        _pstakeGo = _pstakeSet s,
        _feeSS = fees
      }
