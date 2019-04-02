{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Snap
  ( SNAP
  ) where

import qualified Data.Map.Strict         as Map

import           Lens.Micro              ((^.), (&), (.~), (%~))

import           Coin
import           EpochBoundary
import           LedgerState
import           PParams
import           Slot
import           UTxO

import           Control.State.Transition

data SNAP

instance STS SNAP where
  type State SNAP = (SnapShots, UTxOState)
  type Signal SNAP = Epoch
  type Environment SNAP = (PParams, DState, PState, BlocksMade)
  data PredicateFailure SNAP = FailureSNAP
                               deriving (Show, Eq)
  initialRules =
    [pure (emptySnapShots, UTxOState (UTxO Map.empty) (Coin 0) (Coin 0))]
  transitionRules = [snapTransition]

snapTransition :: TransitionRule SNAP
snapTransition = do
  TRC ((pparams, d, p, blocks), (s, u), eNew) <- judgmentContext
  let pooledStake = poolDistr (u ^. utxo) d p
  let slot = firstSlot eNew
  let oblg = obligation pparams (d ^. stKeys) (p ^. stPools) slot
  let decayed = (u ^. deposited) - oblg
  pure
    ( s & pstakeMark .~ pooledStake & pstakeSet .~ (s ^. pstakeMark) &
      pstakeGo .~ (s ^. pstakeSet) &
      poolsSS .~ (p ^. pParams) &
      blocksSS .~ blocks &
      feeSS .~ (u ^. fees) + decayed
    , u & deposited .~ oblg & fees %~ (+) decayed)
