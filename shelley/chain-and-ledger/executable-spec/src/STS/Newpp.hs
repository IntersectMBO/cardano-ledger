{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Newpp
  ( NEWPP
  ) where

import qualified Data.Map.Strict as Map

import           Lens.Micro              ((^.))

import           BaseTypes
import           EpochBoundary
import           LedgerState hiding (reserves)
import           PParams
import           Slot
import           UTxO
import           Coin

import           Control.State.Transition

data NEWPP

instance STS NEWPP where
  type State NEWPP = (UTxOState, AccountState, PParams)
  type Signal NEWPP = Epoch
  type Environment NEWPP = (Maybe PParams, DState, PState)
  data PredicateFailure NEWPP = FailureNEWPP
                                deriving (Show, Eq)
  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

initialNewPp :: InitialRule NEWPP
initialNewPp =
  pure
    (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) (EEnt Map.empty)
    , emptyAccount
    , emptyPParams)

newPpTransition :: TransitionRule NEWPP
newPpTransition = do
  TRC ((ppNew, ds, ps), (utxoSt, acnt, pp), e) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr = obligation pp (ds ^. stKeys) (ps ^. stPools) (firstSlot e)
      let Coin oblgNew =
            obligation ppNew' (ds ^. stKeys) (ps ^. stPools) (slotFromEpoch e)
      let diff = oblgCurr - oblgNew
      let Coin reserves = _reserves acnt
      if reserves + diff >= 0
         && (_maxTxSize ppNew' + _maxBHSize ppNew') < _maxBBSize ppNew'
       then
        let utxoSt' = utxoSt { _deposited = Coin oblgNew } in -- TODO: update mechanism
        let acnt' = acnt { _reserves = Coin $ reserves + diff } in
          pure $ (utxoSt', acnt', ppNew')
       else
        pure $
        ((if reserves + diff < 0
             || (_maxTxSize ppNew' + _maxBHSize ppNew') >= _maxBBSize ppNew'
           then
             utxoSt -- TODO update mechanism
           else
             utxoSt)
        , acnt
        , pp)
    Nothing -> pure (utxoSt, acnt, pp)
