{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Snap
  ( SNAP
  , SnapState (..)
  , SnapEnv (..)
  )
where

import qualified Data.Map.Strict as Map

import           Lens.Micro ((^.))

import           Coin
import           EpochBoundary
import           LedgerState
import           PParams hiding (d)
import           Slot
import           Updates
import           UTxO

import           Control.State.Transition

data SNAP hashAlgo dsignAlgo

data SnapState hashAlgo dsignAlgo
  = SnapState
      (SnapShots hashAlgo dsignAlgo)
      (UTxOState hashAlgo dsignAlgo)

data SnapEnv hashAlgo dsignAlgo
  = SnapEnv
      PParams
      (DState hashAlgo dsignAlgo)
      (PState hashAlgo dsignAlgo)

instance STS (SNAP hashAlgo dsignAlgo) where
  type State (SNAP hashAlgo dsignAlgo) = SnapState hashAlgo dsignAlgo
  type Signal (SNAP hashAlgo dsignAlgo) = Epoch
  type Environment (SNAP hashAlgo dsignAlgo) = SnapEnv hashAlgo dsignAlgo
  data PredicateFailure (SNAP hashAlgo dsignAlgo)
    = FailureSNAP
    deriving (Show, Eq)

  initialRules =
    [pure $ SnapState emptySnapShots (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)]
  transitionRules = [snapTransition]

snapTransition :: TransitionRule (SNAP hashAlgo dsignAlgo)
snapTransition = do
  TRC (SnapEnv pparams d p, SnapState s u, eNew) <- judgmentContext
  let pooledStake = stakeDistr (u ^. utxo) d p
  let _slot = firstSlot eNew
  let oblg = obligation pparams (d ^. stKeys) (p ^. stPools) _slot
  let decayed = (u ^. deposited) - oblg
  pure $ SnapState
    s { _pstakeMark = pooledStake
      , _pstakeSet = s ^. pstakeMark
      , _pstakeGo = s ^. pstakeSet
      , _poolsSS = p ^. pParams
      , _feeSS = (u ^. fees) + decayed}
    u { _deposited = oblg
      , _fees = (u ^. fees) + decayed}
