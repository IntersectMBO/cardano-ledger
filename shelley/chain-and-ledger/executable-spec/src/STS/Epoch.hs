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
    type Environment EPOCH = BlocksMade
    data PredicateFailure EPOCH = PoolReapFailure (PredicateFailure POOLREAP)
                                | SnapFailure (PredicateFailure SNAP)
                                | NewPpFailure (PredicateFailure NEWPP)
                   deriving (Show, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

initialEpoch :: InitialRule EPOCH
initialEpoch = pure $ EpochState
                        emptyAccount
                        emptySnapShots
                        emptyLedgerState
                        emptyPParams

epochTransition :: TransitionRule EPOCH
epochTransition = do
    TRC(blocks, EpochState as ss ls pp, e) <- judgmentContext
    let us = _utxoState ls
    let DPState ds ps = _delegationState ls
    (ss', us') <- trans @SNAP $ TRC((pp, ds, ps, blocks), (ss, us), e)
    (as', ds', ps') <- trans @POOLREAP $ TRC(pp, (as, ds, ps), e)
    let ppNew = undefined -- TODO: result from votedValuePParams
    (us'', as'', pp')
        <- trans @NEWPP $ TRC((ppNew, ds', ps'), (us', as', pp), e)
    pure $ EpochState as'' ss' (ls { _utxoState = us'', _delegationState = DPState ds' ps'}) pp'

instance Embed SNAP EPOCH where
    wrapFailed = SnapFailure

instance Embed POOLREAP EPOCH where
    wrapFailed = PoolReapFailure

instance Embed NEWPP EPOCH where
    wrapFailed = NewPpFailure
