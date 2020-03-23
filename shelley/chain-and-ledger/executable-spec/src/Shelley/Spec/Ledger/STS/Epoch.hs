{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Epoch
  ( EPOCH
  )
where

import           Cardano.Prelude (NoUnexpectedThunks (..), asks)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.EpochBoundary
import           Shelley.Spec.Ledger.LedgerState (pattern DPState, EpochState, pattern EpochState,
                     emptyAccount, emptyLedgerState, esAccountState, esLState, esNonMyopic, esPp,
                     esSnapshots, _delegationState, _ppups, _utxoState)
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import           Shelley.Spec.Ledger.Slot

import           Control.State.Transition

import           Shelley.Spec.Ledger.STS.Newpp
import           Shelley.Spec.Ledger.STS.PoolReap
import           Shelley.Spec.Ledger.STS.Snap

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
      deriving (Show, Generic, Eq)

    initialRules = [ initialEpoch ]
    transitionRules = [ epochTransition ]

instance NoUnexpectedThunks (PredicateFailure (EPOCH crypto))

initialEpoch :: InitialRule (EPOCH crypto)
initialEpoch =
  pure $ EpochState
           emptyAccount
           emptySnapShots
           emptyLedgerState
           emptyPParams
           emptyNonMyopic

votedValuePParams
  :: ProposedPPUpdates crypto
  -> PParams
  -> Int
  -> Maybe PParams
votedValuePParams (ProposedPPUpdates ppup) pps quorumN =
  let
    incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
    votes = Map.foldr
              (\vote tally -> Map.insert vote (incrTally vote tally) tally)
              (Map.empty :: Map PParamsUpdate Int)
              ppup
    consensus = Map.filter (>= quorumN) votes
  in
    case length consensus of
      1 -> (Just . updatePParams pps . fst . head . Map.toList) consensus
      _ -> Nothing

epochTransition :: forall crypto . TransitionRule (EPOCH crypto)
epochTransition = do
  TRC (_, EpochState { esAccountState = acnt
                     , esSnapshots = ss
                     , esLState = ls
                     , esPp = pp
                     , esNonMyopic = nm}, e) <- judgmentContext
  let utxoSt = _utxoState ls
  let DPState dstate pstate = _delegationState ls
  SnapState ss' utxoSt' <-
    trans @(SNAP crypto) $ TRC (SnapEnv pp dstate pstate, SnapState ss utxoSt, e)
  PoolreapState utxoSt'' acnt' dstate' pstate' <-
    trans @(POOLREAP crypto) $ TRC (pp, PoolreapState utxoSt' acnt dstate pstate, e)

  coreNodeQuorum <- liftSTS $ asks quorum

  let ppup = _ppups utxoSt
  let ppNew = votedValuePParams ppup pp (fromIntegral coreNodeQuorum)
  NewppState utxoSt''' acnt'' pp' <-
    trans @(NEWPP crypto)
      $ TRC (NewppEnv ppNew dstate' pstate', NewppState utxoSt'' acnt' pp, e)
  pure $ EpochState
    acnt''
    ss'
    (ls { _utxoState = utxoSt''', _delegationState = DPState dstate' pstate' })
    pp'
    nm

instance Embed (SNAP crypto) (EPOCH crypto) where
    wrapFailed = SnapFailure

instance Embed (POOLREAP crypto) (EPOCH crypto) where
    wrapFailed = PoolReapFailure

instance Embed (NEWPP crypto) (EPOCH crypto) where
    wrapFailed = NewPpFailure
