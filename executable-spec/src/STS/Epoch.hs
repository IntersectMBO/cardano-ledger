{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Epoch
  ( EPOCH
  ) where

import           LedgerState
import           PParams
import           EpochBoundary
import           Slot

import           Control.State.Transition

import           STS.Newpp
import           STS.PoolReap
import           STS.Snap

data EPOCH

instance STS EPOCH where
    type State EPOCH = EpochState
    type Signal EPOCH = Epoch
    type Environment EPOCH = (PParams, BlocksMade)
    data PredicateFailure EPOCH = PoolReapFailure (PredicateFailure POOLREAP)
                                | SnapFailure (PredicateFailure SNAP)
                                | NewPpFailure (PredicateFailure NEWPP)
                   deriving (Show, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

initialEpoch :: InitialRule EPOCH
initialEpoch = pure $ EpochState
                        emptyAccount
                        emptyPParams
                        emptySnapShots
                        emptyLedgerState

epochTransition :: TransitionRule EPOCH
epochTransition = do
    TRC((ppNew, blocks), EpochState as pp ss ls, eNew) <- judgmentContext
    let us = _utxoState ls
    let DWState ds ps = _delegationState ls
    (ss', us') <- trans @SNAP $ TRC((pp, ds, ps, blocks), (ss, us), eNew)
    (as', ds', ps') <- trans @POOLREAP $ TRC(pp, (as, ds, ps), eNew)
    (us'', as'', pp')
        <- trans @NEWPP $ TRC((ppNew, ds', ps'), (us', as', pp), eNew)
    pure $ EpochState as'' pp' ss' (ls { _utxoState = us'', _delegationState = DWState ds' ps'})

instance Embed SNAP EPOCH where
    wrapFailed = SnapFailure

instance Embed POOLREAP EPOCH where
    wrapFailed = PoolReapFailure

instance Embed NEWPP EPOCH where
    wrapFailed = NewPpFailure
