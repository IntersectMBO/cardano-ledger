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

import           BaseTypes
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

data EPOCH crypto

instance STS (EPOCH crypto) where
    type State (EPOCH crypto) = EpochState crypto
    type Signal (EPOCH crypto) = EpochNo
    type Environment (EPOCH crypto) = ()
    type BaseM (EPOCH crypto) = ShelleyBase
    data PredicateFailure (EPOCH crypto)
      = PoolReapFailure (PredicateFailure (POOLREAP crypto))
      | SnapFailure (PredicateFailure (SNAP crypto))
      | NewPpFailure (PredicateFailure (NEWPP crypto))
      deriving (Show, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

initialEpoch :: InitialRule (EPOCH crypto)
initialEpoch =
  pure $ EpochState emptyAccount emptySnapShots emptyLedgerState emptyPParams

votedValuePParams
  :: PPUpdate crypto
  -> PParams
  -> Maybe PParams
votedValuePParams (PPUpdate ppup) pps =
  let
    incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
    votes = Map.foldr
              (\vote tally -> Map.insert vote (incrTally vote tally) tally)
              (Map.empty :: Map PParamsUpdate Int)
              ppup
    consensus = Map.filter (>= 5) votes
  in
    case length consensus of
      1 -> (Just . updatePParams pps . fst . head . Map.toList) consensus
      _ -> Nothing

epochTransition :: forall crypto . TransitionRule (EPOCH crypto)
epochTransition = do
  TRC (_, EpochState { esAccountState = as
                     , esSnapshots = ss
                     , esLState = ls
                     , esPp = pp}, e) <- judgmentContext
  let us = _utxoState ls
  let UpdateState ppup _ _ _ = _ups us
  let DPState ds ps = _delegationState ls
  SnapState ss' us' <-
    trans @(SNAP crypto) $ TRC (SnapEnv pp ds ps, SnapState ss us, e)
  PoolreapState us'' as' ds' ps' <-
    trans @(POOLREAP crypto) $ TRC (pp, PoolreapState us' as ds ps, e)
  let ppNew = votedValuePParams ppup pp
  NewppState us''' as'' pp' <-
    trans @(NEWPP crypto)
      $ TRC (NewppEnv ppNew ds' ps', NewppState us'' as' pp, e)
  pure $ EpochState
    as''
    ss'
    (ls { _utxoState = us''', _delegationState = DPState ds' ps' })
    pp'

instance Embed (SNAP crypto) (EPOCH crypto) where
    wrapFailed = SnapFailure

instance Embed (POOLREAP crypto) (EPOCH crypto) where
    wrapFailed = PoolReapFailure

instance Embed (NEWPP crypto) (EPOCH crypto) where
    wrapFailed = NewPpFailure
