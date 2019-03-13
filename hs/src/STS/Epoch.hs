{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Epoch
  ( EPOCH
  ) where

import qualified Data.Map as Map

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
    type State EPOCH = (UTxOState, AccountState, DState, PState, PParams, SnapShots)
    type Signal EPOCH = ()
    type Environment EPOCH = (Epoch, PParams, BlocksMade)
    data PredicateFailure EPOCH = PoolReapFailure (PredicateFailure POOLREAP)
                                | SnapFailure (PredicateFailure SNAP)
                                | NewPpFailure (PredicateFailure NEWPP)
                   deriving (Show, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

initialEpoch :: InitialRule EPOCH
initialEpoch = pure ( UTxOState (UTxO Map.empty) (Coin 0) (Coin 0)
                    , emptyAccount
                    , emptyDState
                    , emptyPState
                    , emptyPParams
                    , emptySnapShots)

epochTransition :: TransitionRule EPOCH
epochTransition = do
    TRC((eNew, ppNew, blocks), (us, as, ds, ps, pp, ss), _) <- judgmentContext
    (ss', us') <- trans @SNAP $ TRC((eNew, pp, ds, ps, blocks), (ss, us), ())
    (as', ds', ps') <- trans @POOLREAP $ TRC((eNew, pp), (as, ds, ps), ())
    (us'', as'', pp')
        <- trans @NEWPP $ TRC((eNew, ppNew, ds', ps'), (us', as', pp), ())
    pure (us'', as'', ds', ps', pp', ss')

instance Embed SNAP EPOCH where
    wrapFailed = SnapFailure

instance Embed POOLREAP EPOCH where
    wrapFailed = PoolReapFailure

instance Embed NEWPP EPOCH where
    wrapFailed = NewPpFailure
