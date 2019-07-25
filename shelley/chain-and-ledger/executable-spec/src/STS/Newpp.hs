{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Newpp
  ( NEWPP
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

instance STS (NEWPP hashAlgo dsignAlgo) where
  type State (NEWPP hashAlgo dsignAlgo)
    = (UTxOState hashAlgo dsignAlgo, AccountState, PParams)
  type Signal (NEWPP hashAlgo dsignAlgo) = Epoch
  type Environment (NEWPP hashAlgo dsignAlgo)
    = (Maybe PParams, DState hashAlgo dsignAlgo, PState hashAlgo dsignAlgo)
  data PredicateFailure (NEWPP hashAlgo dsignAlgo)
    = FailureNEWPP
    deriving (Show, Eq)

  initialRules = [initialNewPp]
  transitionRules = [newPpTransition]

initialNewPp :: InitialRule (NEWPP hashAlgo dsignAlgo)
initialNewPp = pure
  ( UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) (emptyUpdateState)
  , emptyAccount
  , emptyPParams
  )

newPpTransition :: TransitionRule (NEWPP hashAlgo dsignAlgo)
newPpTransition = do
  TRC ((ppNew, ds, ps), (utxoSt, acnt, pp), e) <- judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr =
            obligation pp (ds ^. stKeys) (ps ^. stPools) (firstSlot e)
      let Coin oblgNew =
            obligation ppNew' (ds ^. stKeys) (ps ^. stPools) (slotFromEpoch e)
      let diff          = oblgCurr - oblgNew
      let Coin reserves = _reserves acnt
      if reserves
           +  diff
           >= 0
           && (_maxTxSize ppNew' + _maxBHSize ppNew')
           <  _maxBBSize ppNew'
        then
          let utxoSt' = utxoSt { _deposited = Coin oblgNew }
          in  -- TODO: update mechanism
              let acnt' = acnt { _reserves = Coin $ reserves + diff }
                                       in  pure $ (utxoSt', acnt', ppNew')
        else
          pure
            $ ( (if reserves
                     +  diff
                     <  0
                     || (_maxTxSize ppNew' + _maxBHSize ppNew')
                     >= _maxBBSize ppNew'
                  then utxoSt -- TODO update mechanism
                  else utxoSt
                )
              , acnt
              , pp
              )
    Nothing -> pure (utxoSt, acnt, pp)
