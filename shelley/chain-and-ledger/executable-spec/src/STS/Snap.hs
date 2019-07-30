{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Snap
  ( SNAP
  )
where

import qualified Data.Map.Strict as Map

import           Lens.Micro ((%~), (&), (.~), (^.))

import           Coin
import           EpochBoundary
import           LedgerState
import           PParams hiding (d)
import           Slot
import           Updates
import           UTxO

import           Control.State.Transition

data SNAP hashAlgo dsignAlgo

instance STS (SNAP hashAlgo dsignAlgo) where
  type State (SNAP hashAlgo dsignAlgo)
    = (SnapShots hashAlgo dsignAlgo, UTxOState hashAlgo dsignAlgo)
  type Signal (SNAP hashAlgo dsignAlgo) = Epoch
  type Environment (SNAP hashAlgo dsignAlgo)
    = ( PParams
      , DState hashAlgo dsignAlgo
      , PState hashAlgo dsignAlgo
      )
  data PredicateFailure (SNAP hashAlgo dsignAlgo)
    = FailureSNAP
    deriving (Show, Eq)

  initialRules =
    [pure (emptySnapShots, UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)]
  transitionRules = [snapTransition]

snapTransition :: TransitionRule (SNAP hashAlgo dsignAlgo)
snapTransition = do
  TRC ((pparams, d, p), (s, u), eNew) <- judgmentContext
  let pooledStake = stakeDistr (u ^. utxo) d p
  let _slot       = firstSlot eNew
  let oblg = obligation pparams (d ^. stKeys) (p ^. stPools) _slot
  let decayed     = (u ^. deposited) - oblg
  pure
    ( s
    &  pstakeMark
    .~ pooledStake
    &  pstakeSet
    .~ (s ^. pstakeMark)
    &  pstakeGo
    .~ (s ^. pstakeSet)
    &  poolsSS
    .~ (p ^. pParams)
    &  feeSS
    .~ (u ^. fees)
    +  decayed
    , u & deposited .~ oblg & fees %~ (+) decayed
    )
