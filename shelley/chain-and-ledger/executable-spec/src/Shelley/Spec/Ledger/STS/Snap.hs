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
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    LedgerState (..),
    UTxOState (..),
    stakeDistr,
  )

data SNAP crypto

instance Typeable crypto => STS (SNAP crypto) where
  type State (SNAP crypto) = SnapShots crypto
  type Signal (SNAP crypto) = ()
  type Environment (SNAP crypto) = LedgerState crypto
  type BaseM (SNAP crypto) = ShelleyBase
  data PredicateFailure (SNAP crypto) -- No predicate failures
    deriving (Show, Generic, Eq)

  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

instance NoUnexpectedThunks (PredicateFailure (SNAP crypto))

snapTransition :: TransitionRule (SNAP crypto)
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
