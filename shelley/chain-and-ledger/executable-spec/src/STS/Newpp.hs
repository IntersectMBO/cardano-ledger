{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Newpp
  ( NEWPP
  ) where

import qualified Data.Map.Strict as Map

import           Lens.Micro              ((^.), (&), (.~))

import           EpochBoundary
import           LedgerState
import           PParams
import           Slot
import           UTxO
import           Coin

import           Control.State.Transition

data NEWPP

instance STS NEWPP where
  type State NEWPP = (UTxOState, AccountState, PParams)
  type Signal NEWPP = Epoch
  type Environment NEWPP = (PParams, DState, PState)
  data PredicateFailure NEWPP = FailureNEWPP
                                deriving (Show, Eq)
  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

initialNewPp :: InitialRule NEWPP
initialNewPp =
  pure
    (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0), emptyAccount, emptyPParams)

newPpTransition :: TransitionRule NEWPP
newPpTransition = do
  TRC ((ppNew, ds, ps), (us, as, pp), eNew) <- judgmentContext
  let oblgCurr = obligation pp (ds ^. stKeys) (ps ^. stPools) (firstSlot eNew)
  let oblgNew =
        obligation ppNew (ds ^. stKeys) (ps ^. stPools) (slotFromEpoch eNew)
  let (oblg', reserves', pp') =
        if as ^. reserves + oblgCurr >= oblgNew -- reserves are sufficient
          then (oblgNew, (as ^. reserves + oblgCurr) - oblgNew, ppNew)
          else (us ^. deposited, as ^. reserves, pp)
  pure (us & deposited .~ oblg', as & reserves .~ reserves', pp')
