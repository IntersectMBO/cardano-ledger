{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Epoch
  ( EPOCH,
    EpochPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.Constraints (UsesValue)
import Cardano.Ledger.Era
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition (Embed (..), InitialRule, STS (..), TRC (..), TransitionRule, judgmentContext, liftSTS, trans)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
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

-- ================================================

data EPOCH era

data EpochPredicateFailure era
  = PoolReapFailure (PredicateFailure (POOLREAP era)) -- Subtransition Failures
  | SnapFailure (PredicateFailure (SNAP era)) -- Subtransition Failures
  | NewPpFailure (PredicateFailure (NEWPP era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  (Eq (PredicateFailure (SNAP era))) =>
  Eq (EpochPredicateFailure era)

deriving stock instance
  (Show (PredicateFailure (SNAP era))) =>
  Show (EpochPredicateFailure era)

instance UsesValue era => STS (EPOCH era) where
  type State (EPOCH era) = EpochState era
  type Signal (EPOCH era) = EpochNo
  type Environment (EPOCH era) = ()
  type BaseM (EPOCH era) = ShelleyBase
  type PredicateFailure (EPOCH era) = EpochPredicateFailure era
  initialRules = [initialEpoch]
  transitionRules = [epochTransition]

instance NoThunks (EpochPredicateFailure era)

initialEpoch :: InitialRule (EPOCH era)
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
  ProposedPPUpdates era ->
  PParams era ->
  Int ->
  Maybe (PParams era)
votedValue (ProposedPPUpdates pup) pps quorumN =
  let incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
      votes =
        Map.foldr
          (\vote tally -> Map.insert vote (incrTally vote tally) tally)
          (Map.empty :: Map (PParamsUpdate era) Int)
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
  forall era.
  UsesValue era =>
  TransitionRule (EPOCH era)
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
    trans @(SNAP era) $ TRC (ls, ss, ())

  let PState pParams fPParams _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { _pParams = ppp,
            _fPParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(POOLREAP era) $ TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  coreNodeQuorum <- liftSTS $ asks quorum

  let pup = proposals . _ppups $ utxoSt'
  let ppNew = votedValue pup pp (fromIntegral coreNodeQuorum)
  NewppState utxoSt'' acnt'' pp' <-
    trans @(NEWPP era) $
      TRC (NewppEnv dstate' pstate'', NewppState utxoSt' acnt' pp, ppNew)
  pure $
    EpochState
      acnt''
      ss'
      (ls {_utxoState = utxoSt'', _delegationState = DPState dstate' pstate''})
      pp
      pp'
      nm

instance UsesValue era => Embed (SNAP era) (EPOCH era) where
  wrapFailed = SnapFailure

instance Era era => Embed (POOLREAP era) (EPOCH era) where
  wrapFailed = PoolReapFailure

instance Era era => Embed (NEWPP era) (EPOCH era) where
  wrapFailed = NewPpFailure
