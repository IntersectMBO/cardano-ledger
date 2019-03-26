{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Epoch
  ( EPOCH
  ) where

import qualified Data.Map.Strict as Map

import           LedgerState
import           PParams
import           EpochBoundary
import           Slot
import           UTxO
import           Coin

import           Control.State.Transition

import           STS.Newpp
import           STS.PoolReap
import           STS.Snap

data EPOCH

instance STS EPOCH where
    type State EPOCH = (AccountState, PParams, SnapShots, LedgerState)
    type Signal EPOCH = Epoch
    type Environment EPOCH = (PParams, BlocksMade)
    data PredicateFailure EPOCH = PoolReapFailure (PredicateFailure POOLREAP)
                                | SnapFailure (PredicateFailure SNAP)
                                | NewPpFailure (PredicateFailure NEWPP)
                   deriving (Show, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

initialEpoch :: InitialRule EPOCH
initialEpoch = pure ( emptyAccount
                    , emptyPParams
                    , emptySnapShots
                    , LedgerState
                       (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0))
                       emptyDelegation
                       emptyPParams
                       0
                       (Slot 0))

epochTransition :: TransitionRule EPOCH
epochTransition = do
    TRC((ppNew, blocks), (as, pp, ss, ls), eNew) <- judgmentContext
    let us = _utxoState ls
    let DWState ds ps = _delegationState ls
    (ss', us') <- trans @SNAP $ TRC((pp, ds, ps, blocks), (ss, us), eNew)
    (as', ds', ps') <- trans @POOLREAP $ TRC(pp, (as, ds, ps), eNew)
    (us'', as'', pp')
        <- trans @NEWPP $ TRC((ppNew, ds', ps'), (us', as', pp), eNew)
    pure (as'', pp', ss', ls { _utxoState = us'', _delegationState = DWState ds' ps'})

instance Embed SNAP EPOCH where
    wrapFailed = SnapFailure

instance Embed POOLREAP EPOCH where
    wrapFailed = PoolReapFailure

instance Embed NEWPP EPOCH where
    wrapFailed = NewPpFailure
