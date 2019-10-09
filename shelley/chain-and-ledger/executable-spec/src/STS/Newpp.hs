{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
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
import           LedgerState (AccountState, DState, PState, UTxOState, pattern UTxOState, clearPpup,
                     emptyAccount, stKeys, stPools, _deposited, _irwd, _reserves)
import           PParams
import           Slot
import           Updates
import           UTxO

import           Control.State.Transition

data NEWPP hashAlgo dsignAlgo vrfAlgo

data NewppState hashAlgo dsignAlgo vrfAlgo
  = NewppState (UTxOState hashAlgo dsignAlgo vrfAlgo) AccountState PParams

data NewppEnv hashAlgo dsignAlgo vrfAlgo
  = NewppEnv (Maybe PParams) (DState hashAlgo dsignAlgo) (PState hashAlgo dsignAlgo vrfAlgo)

instance STS (NEWPP hashAlgo dsignAlgo vrfAlgo) where
  type State (NEWPP hashAlgo dsignAlgo vrfAlgo) = NewppState hashAlgo dsignAlgo vrfAlgo
  type Signal (NEWPP hashAlgo dsignAlgo vrfAlgo) = Epoch
  type Environment (NEWPP hashAlgo dsignAlgo vrfAlgo) = NewppEnv hashAlgo dsignAlgo vrfAlgo
  data PredicateFailure (NEWPP hashAlgo dsignAlgo vrfAlgo)
    = FailureNEWPP
    deriving (Show, Eq)

  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

initialNewPp :: InitialRule (NEWPP hashAlgo dsignAlgo vrfAlgo)
initialNewPp = pure $ NewppState
  (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)
  emptyAccount
  emptyPParams

newPpTransition :: TransitionRule (NEWPP hashAlgo dsignAlgo vrfAlgo)
newPpTransition = do
  TRC (NewppEnv ppNew ds ps, NewppState utxoSt acnt pp, e) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      let slot_ = firstSlot e
          Coin oblgCurr = obligation pp (ds ^. stKeys) (ps ^. stPools) slot_
          Coin oblgNew = obligation ppNew' (ds ^. stKeys) (ps ^. stPools) slot_
          diff = oblgCurr - oblgNew
          Coin reserves = _reserves acnt
          Coin requiredInstantaneousRewards = foldl (+) (Coin 0) $ _irwd ds

      if reserves + diff >= requiredInstantaneousRewards
         && (_maxTxSize ppNew' + _maxBHSize ppNew') <  _maxBBSize ppNew'
        then
          let utxoSt' = utxoSt { _deposited = Coin oblgNew }
          in  -- TODO: update mechanism
              let acnt' = acnt { _reserves = Coin $ reserves + diff }
              in pure $ NewppState (clearPpup utxoSt') acnt' ppNew'
        else
          pure $ NewppState (clearPpup utxoSt) acnt pp
    Nothing -> pure $ NewppState (clearPpup utxoSt) acnt pp
