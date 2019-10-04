{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Epoch
  ( EPOCH
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           EpochBoundary
import           LedgerState (pattern DPState, EpochState, pattern EpochState, emptyAccount,
                     emptyLedgerState, esAccountState, esLState, esPp, esSnapshots,
                     _delegationState, _ups, _utxoState)
import           PParams
import           Slot
import           Updates

import           Control.State.Transition

import           STS.Newpp
import           STS.PoolReap
import           STS.Snap

data EPOCH hashAlgo dsignAlgo vrfAlgo

instance STS (EPOCH hashAlgo dsignAlgo vrfAlgo) where
    type State (EPOCH hashAlgo dsignAlgo vrfAlgo) = EpochState hashAlgo dsignAlgo vrfAlgo
    type Signal (EPOCH hashAlgo dsignAlgo vrfAlgo) = Epoch
    type Environment (EPOCH hashAlgo dsignAlgo vrfAlgo) = ()
    data PredicateFailure (EPOCH hashAlgo dsignAlgo vrfAlgo)
      = PoolReapFailure (PredicateFailure (POOLREAP hashAlgo dsignAlgo vrfAlgo))
      | SnapFailure (PredicateFailure (SNAP hashAlgo dsignAlgo vrfAlgo))
      | NewPpFailure (PredicateFailure (NEWPP hashAlgo dsignAlgo vrfAlgo))
      deriving (Show, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

initialEpoch :: InitialRule (EPOCH hashAlgo dsignAlgo vrfAlgo)
initialEpoch =
  pure $ EpochState emptyAccount emptySnapShots emptyLedgerState emptyPParams

votedValuePParams
  :: PPUpdate hashAlgo dsignAlgo
  -> PParams
  -> Maybe PParams
votedValuePParams (PPUpdate ppup) pps =
  let
    incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
    votes = Map.foldr
              (\vote tally -> Map.insert vote (incrTally vote tally) tally)
              (Map.empty :: Map (Set Ppm) Int)
              ppup
    consensus = Map.filter (>= 5) votes
  in
    case length consensus of
      1 -> (Just . updatePParams pps . fst . head . Map.toList) consensus
      _ -> Nothing

epochTransition :: forall hashAlgo dsignAlgo vrfAlgo . TransitionRule (EPOCH hashAlgo dsignAlgo vrfAlgo)
epochTransition = do
  TRC (_, EpochState { esAccountState = as
                     , esSnapshots = ss
                     , esLState = ls
                     , esPp = pp}, e) <- judgmentContext
  let us = _utxoState ls
  let UpdateState ppup _ _ _ = _ups us
  let DPState ds ps = _delegationState ls
  SnapState ss' us' <-
    trans @(SNAP hashAlgo dsignAlgo vrfAlgo) $ TRC (SnapEnv pp ds ps, SnapState ss us, e)
  PoolreapState us'' as' ds' ps' <-
    trans @(POOLREAP hashAlgo dsignAlgo vrfAlgo) $ TRC (pp, PoolreapState us' as ds ps, e)
  let ppNew = votedValuePParams ppup pp
  NewppState us''' as'' pp' <-
    trans @(NEWPP hashAlgo dsignAlgo vrfAlgo)
      $ TRC (NewppEnv ppNew ds' ps', NewppState us'' as' pp, e)
  pure $ EpochState
    as''
    ss'
    (ls { _utxoState = us''', _delegationState = DPState ds' ps' })
    pp'

instance Embed (SNAP hashAlgo dsignAlgo vrfAlgo) (EPOCH hashAlgo dsignAlgo vrfAlgo) where
    wrapFailed = SnapFailure

instance Embed (POOLREAP hashAlgo dsignAlgo vrfAlgo) (EPOCH hashAlgo dsignAlgo vrfAlgo) where
    wrapFailed = PoolReapFailure

instance Embed (NEWPP hashAlgo dsignAlgo vrfAlgo) (EPOCH hashAlgo dsignAlgo vrfAlgo) where
    wrapFailed = NewPpFailure
