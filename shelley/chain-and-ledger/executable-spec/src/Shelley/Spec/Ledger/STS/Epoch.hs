{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Epoch
  ( EPOCH,
    PredicateFailure (..),
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..), asks)
import Control.Iterate.SetAlgebra (eval, (⨃))
import Control.State.Transition (Embed (..), InitialRule, STS (..), TRC (..), TransitionRule, judgmentContext, liftSTS, trans)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Shelley.Spec.Ledger.EpochBoundary (emptySnapShots)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    PPUPState (..),
    PState (..),
    emptyAccount,
    emptyLedgerState,
    esAccountState,
    esLState,
    esNonMyopic,
    esPp,
    esPrevPp,
    esSnapshots,
    _delegationState,
    _ppups,
    _utxoState,
    pattern DPState,
    pattern EpochState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParamsUpdate, ProposedPPUpdates (..), emptyPParams, updatePParams)
import Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import Shelley.Spec.Ledger.STS.Newpp (NEWPP, NewppEnv (..), NewppState (..))
import Shelley.Spec.Ledger.STS.PoolReap (POOLREAP, PoolreapState (..))
import Shelley.Spec.Ledger.STS.Snap (SNAP)
import Shelley.Spec.Ledger.Slot (EpochNo)
import Shelley.Spec.Ledger.Value

data EPOCH crypto v

instance (CV crypto v) => STS (EPOCH crypto v) where
  type State (EPOCH crypto v) = EpochState crypto v
  type Signal (EPOCH crypto v) = EpochNo
  type Environment (EPOCH crypto v) = ()
  type BaseM (EPOCH crypto v) = ShelleyBase
  data PredicateFailure (EPOCH crypto v)
    = PoolReapFailure (PredicateFailure (POOLREAP crypto v)) -- Subtransition Failures
    | SnapFailure (PredicateFailure (SNAP crypto v)) -- Subtransition Failures
    | NewPpFailure (PredicateFailure (NEWPP crypto v)) -- Subtransition Failures
    deriving (Show, Generic, Eq)

  initialRules = [initialEpoch]
  transitionRules = [epochTransition]

instance NoUnexpectedThunks (PredicateFailure (EPOCH crypto v))

initialEpoch :: InitialRule (EPOCH crypto v)
initialEpoch =
  pure $
    EpochState
      emptyAccount
      emptySnapShots
      emptyLedgerState
      emptyPParams
      emptyPParams
      emptyNonMyopic

votedValue ::
  ProposedPPUpdates crypto ->
  PParams ->
  Int ->
  Maybe PParams
votedValue (ProposedPPUpdates pup) pps quorumN =
  let incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
      votes =
        Map.foldr
          (\vote tally -> Map.insert vote (incrTally vote tally) tally)
          (Map.empty :: Map PParamsUpdate Int)
          pup
      consensus = Map.filter (>= quorumN) votes
   in case length consensus of
        -- NOTE that `quorumN` is a global constant, and that we require
        -- it to be strictly greater than half the number of genesis nodes.
        -- The keys in the `pup` correspond to the genesis nodes,
        -- and therefore either:
        --   1) `consensus` is empty, or
        --   2) `consensus` has exactly one element.
        1 -> (Just . updatePParams pps . fst . head . Map.toList) consensus
        -- NOTE that `updatePParams` corresponds to the union override right
        -- operation in the formal spec.
        _ -> Nothing

epochTransition ::
  forall crypto v.
  CV crypto v =>
  TransitionRule (EPOCH crypto v)
epochTransition = do
  TRC
    ( _,
      EpochState
        { esAccountState = acnt,
          esSnapshots = ss,
          esLState = ls,
          esPrevPp = _pr,
          esPp = pp,
          esNonMyopic = nm
        },
      e
      ) <-
    judgmentContext
  let utxoSt = _utxoState ls
  let DPState dstate pstate = _delegationState ls
  ss' <-
    trans @(SNAP crypto v) $ TRC (ls, ss, ())

  let PState pParams fPParams _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { _pParams = ppp,
            _fPParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(POOLREAP crypto v) $ TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  coreNodeQuorum <- liftSTS $ asks quorum

  let pup = proposals . _ppups $ utxoSt'
  let ppNew = votedValue pup pp (fromIntegral coreNodeQuorum)
  NewppState utxoSt'' acnt'' pp' <-
    trans @(NEWPP crypto v) $
      TRC (NewppEnv dstate' pstate'', NewppState utxoSt' acnt' pp, ppNew)
  pure $
    EpochState
      acnt''
      ss'
      (ls {_utxoState = utxoSt'', _delegationState = DPState dstate' pstate''})
      pp
      pp'
      nm

instance (CV crypto v) => Embed (SNAP crypto v) (EPOCH crypto v) where
  wrapFailed = SnapFailure

instance (CV crypto v) => Embed (POOLREAP crypto v) (EPOCH crypto v) where
  wrapFailed = PoolReapFailure

instance (CV crypto v) => Embed (NEWPP crypto v) (EPOCH crypto v) where
  wrapFailed = NewPpFailure
