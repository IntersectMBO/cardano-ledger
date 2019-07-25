{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Epoch
  ( EPOCH
  )
where

import           EpochBoundary
import           LedgerState
import           PParams
import           Slot

import           Control.State.Transition

import           STS.Newpp
import           STS.PoolReap
import           STS.Snap

data EPOCH hashAlgo dsignAlgo

instance STS (EPOCH hashAlgo dsignAlgo) where
    type State (EPOCH hashAlgo dsignAlgo) = EpochState hashAlgo dsignAlgo
    type Signal (EPOCH hashAlgo dsignAlgo) = Epoch
    type Environment (EPOCH hashAlgo dsignAlgo) = BlocksMade hashAlgo dsignAlgo
    data PredicateFailure (EPOCH hashAlgo dsignAlgo)
      = PoolReapFailure (PredicateFailure (POOLREAP hashAlgo dsignAlgo))
      | SnapFailure (PredicateFailure (SNAP hashAlgo dsignAlgo))
      | NewPpFailure (PredicateFailure (NEWPP hashAlgo dsignAlgo))
      deriving (Show, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

initialEpoch :: InitialRule (EPOCH hashAlgo dsignAlgo)
initialEpoch =
  pure $ EpochState emptyAccount emptySnapShots emptyLedgerState emptyPParams

epochTransition :: forall hashAlgo dsignAlgo . TransitionRule (EPOCH hashAlgo dsignAlgo)
epochTransition = do
  TRC (blocks, EpochState as ss ls pp, e) <- judgmentContext
  let us            = _utxoState ls
  let DPState ds ps = _delegationState ls
  (ss', us') <-
    trans @(SNAP hashAlgo dsignAlgo) $ TRC ((pp, ds, ps, blocks), (ss, us), e)
  (as', ds', ps') <-
    trans @(POOLREAP hashAlgo dsignAlgo) $ TRC (pp, (as, ds, ps), e)
  let ppNew = Just pp -- TODO: result from votedValuePParams
  (us'', as'', pp') <-
    trans @(NEWPP hashAlgo dsignAlgo)
      $ TRC ((ppNew, ds', ps'), (us', as', pp), e)
  pure $ EpochState
    as''
    ss'
    (ls { _utxoState = us'', _delegationState = DPState ds' ps' })
    pp'

instance Embed (SNAP hashAlgo dsignAlgo) (EPOCH hashAlgo dsignAlgo) where
    wrapFailed = SnapFailure

instance Embed (POOLREAP hashAlgo dsignAlgo) (EPOCH hashAlgo dsignAlgo) where
    wrapFailed = PoolReapFailure

instance Embed (NEWPP hashAlgo dsignAlgo) (EPOCH hashAlgo dsignAlgo) where
    wrapFailed = NewPpFailure
