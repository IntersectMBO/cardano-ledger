{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Snap
  ( SNAP,
    SnapState (..),
    SnapEnv (..),
    PredicateFailure,
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
  ( DState (..),
    PState (..),
    UTxOState (..),
    _deposited,
    _fees,
    stakeDistr,
  )
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.UTxO

data SNAP crypto

data SnapState crypto
  = SnapState
      (SnapShots crypto)
      (UTxOState crypto)

data SnapEnv crypto
  = SnapEnv
      PParams
      (DState crypto)
      (PState crypto)

instance STS (SNAP crypto) where
  type State (SNAP crypto) = SnapState crypto
  type Signal (SNAP crypto) = EpochNo
  type Environment (SNAP crypto) = SnapEnv crypto
  type BaseM (SNAP crypto) = ShelleyBase
  data PredicateFailure (SNAP crypto)
    deriving (Show, Generic, Eq)

  initialRules =
    [pure $ SnapState emptySnapShots (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPPUpdates)]
  transitionRules = [snapTransition]

instance NoUnexpectedThunks (PredicateFailure (SNAP crypto))

snapTransition :: TransitionRule (SNAP crypto)
snapTransition = do
  TRC (SnapEnv pp dstate pstate, SnapState s u, eNew) <- judgmentContext

  let UTxOState utxo deposits' fees _ = u
  let DState stkCreds _ _ _ _ _ _ = dstate
  let PState stpools _ _ _ = pstate
  let stake = stakeDistr utxo dstate pstate
  _slot <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoFirst ei eNew
  let oblg = obligation pp stkCreds stpools _slot
  let decayed = deposits' - oblg
  pure $
    SnapState
      s
        { _pstakeMark = stake,
          _pstakeSet = _pstakeMark s,
          _pstakeGo = _pstakeSet s,
          _feeSS = fees + decayed
        }
      u
        { _deposited = oblg,
          _fees = fees + decayed
        }
