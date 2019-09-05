{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Newpp
  ( NEWPP
  , NewppState (..)
  , NewppEnv (..)
  )
where

import qualified Data.Map.Strict as Map

import           Lens.Micro ((^.))

import           Coin
import           EpochBoundary
import           LedgerState hiding (reserves)
import           PParams
import           Slot
import           Updates
import           UTxO

import           Control.State.Transition

data NEWPP hashAlgo dsignAlgo

data NewppState hashAlgo dsignAlgo
  = NewppState (UTxOState hashAlgo dsignAlgo) AccountState PParams

data NewppEnv hashAlgo dsignAlgo
  = NewppEnv (Maybe PParams) (DState hashAlgo dsignAlgo) (PState hashAlgo dsignAlgo)

instance STS (NEWPP hashAlgo dsignAlgo) where
  type State (NEWPP hashAlgo dsignAlgo) = NewppState hashAlgo dsignAlgo
  type Signal (NEWPP hashAlgo dsignAlgo) = Epoch
  type Environment (NEWPP hashAlgo dsignAlgo) = NewppEnv hashAlgo dsignAlgo
  data PredicateFailure (NEWPP hashAlgo dsignAlgo)
    = FailureNEWPP
    deriving (Show, Eq)

  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

initialNewPp :: InitialRule (NEWPP hashAlgo dsignAlgo)
initialNewPp = pure $ NewppState
  (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)
  emptyAccount
  emptyPParams

newPpTransition :: TransitionRule (NEWPP hashAlgo dsignAlgo)
newPpTransition = do
  TRC (NewppEnv ppNew ds ps, NewppState utxoSt acnt pp, e) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      let slot_ = firstSlot e
          Coin oblgCurr = obligation pp (ds ^. stKeys) (ps ^. stPools) slot_
          Coin oblgNew = obligation ppNew' (ds ^. stKeys) (ps ^. stPools) slot_
          diff = oblgCurr - oblgNew

      let Coin reserves = _reserves acnt
      if reserves + diff >= 0
         && (_maxTxSize ppNew' + _maxBHSize ppNew') <  _maxBBSize ppNew'
        then
          let utxoSt' = utxoSt { _deposited = Coin oblgNew }
          in  -- TODO: update mechanism
              let acnt' = acnt { _reserves = Coin $ reserves + diff }
              in pure $ NewppState (clearPpup utxoSt') acnt' ppNew'
        else
          pure $ NewppState (clearPpup utxoSt) acnt pp
    Nothing -> pure $ NewppState (clearPpup utxoSt) acnt pp
