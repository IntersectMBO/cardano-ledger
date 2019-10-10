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

data SNAP hashAlgo dsignAlgo vrfAlgo

data SnapState hashAlgo dsignAlgo vrfAlgo
  = SnapState
      (SnapShots hashAlgo dsignAlgo vrfAlgo)
      (UTxOState hashAlgo dsignAlgo vrfAlgo)

data SnapEnv hashAlgo dsignAlgo vrfAlgo
  = SnapEnv
      PParams
      (DState hashAlgo dsignAlgo)
      (PState hashAlgo dsignAlgo vrfAlgo)

instance STS (SNAP hashAlgo dsignAlgo vrfAlgo) where
  type State (SNAP hashAlgo dsignAlgo vrfAlgo) = SnapState hashAlgo dsignAlgo vrfAlgo
  type Signal (SNAP hashAlgo dsignAlgo vrfAlgo) = Epoch
  type Environment (SNAP hashAlgo dsignAlgo vrfAlgo) = SnapEnv hashAlgo dsignAlgo vrfAlgo
  data PredicateFailure (SNAP hashAlgo dsignAlgo vrfAlgo)
    = FailureSNAP
    deriving (Show, Eq)

  initialRules =
    [pure $ SnapState emptySnapShots (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)]
  transitionRules = [snapTransition]

snapTransition :: TransitionRule (SNAP hashAlgo dsignAlgo vrfAlgo)
snapTransition = do
  TRC (SnapEnv pparams d p, SnapState s u, eNew) <- judgmentContext
  let pooledStake = stakeDistr (u ^. utxo) d p
  let _slot = firstSlot eNew
  let oblg = obligation pparams (d ^. stkCreds) (p ^. stPools) _slot
  let decayed = (u ^. deposited) - oblg
  pure $ SnapState
    s { _pstakeMark = pooledStake
      , _pstakeSet = s ^. pstakeMark
      , _pstakeGo = s ^. pstakeSet
      , _poolsSS = p ^. pParams
      , _feeSS = (u ^. fees) + decayed}
    u { _deposited = oblg
      , _fees = (u ^. fees) + decayed}
