{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Snap
  ( SNAP
  , SnapState (..)
  , SnapEnv (..)
  )
where

import           BaseTypes
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import qualified Data.Map.Strict as Map
import           EpochBoundary
import           GHC.Generics (Generic)
import           LedgerState (DState (..), PState (..), UTxOState (..), stakeDistr, _deposited,
                     _fees)
import           PParams hiding (d)
import           Slot
import           Updates
import           UTxO

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
    = FailureSNAP
    deriving (Show, Generic, Eq)

  initialRules =
    [pure $ SnapState emptySnapShots (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)]
  transitionRules = [snapTransition]

instance NoUnexpectedThunks (PredicateFailure (SNAP crypto))

snapTransition :: TransitionRule (SNAP crypto)
snapTransition = do
  TRC (SnapEnv pp dstate pstate, SnapState s u, eNew) <- judgmentContext

  let UTxOState utxo deposits' fees _ = u
  let DState stkCreds _ _ _ _ _ _ = dstate
  let PState stpools poolParams _ = pstate
  let stake = stakeDistr utxo dstate pstate
  _slot <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoFirst ei eNew
  let oblg = obligation pp stkCreds stpools _slot
  let decayed = deposits' - oblg
  pure $ SnapState
    s { _pstakeMark = stake
      , _pstakeSet = _pstakeMark s
      , _pstakeGo = _pstakeSet s
      , _poolsSS = poolParams
      , _feeSS = fees + decayed}
    u { _deposited = oblg
      , _fees = fees + decayed}
