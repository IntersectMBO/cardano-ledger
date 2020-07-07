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
import Control.State.Transition (Embed (..), InitialRule, STS (..), TRC (..), TransitionRule, judgmentContext, liftSTS, trans)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Shelley.Spec.Ledger.Core ((⨃))
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

data EPOCH crypto

instance Typeable crypto => STS (EPOCH crypto) where
  type State (EPOCH crypto) = EpochState crypto
  type Signal (EPOCH crypto) = EpochNo
  type Environment (EPOCH crypto) = ()
  type BaseM (EPOCH crypto) = ShelleyBase
  data PredicateFailure (EPOCH crypto)
    = PoolReapFailure (PredicateFailure (POOLREAP crypto)) -- Subtransition Failures
    | SnapFailure (PredicateFailure (SNAP crypto)) -- Subtransition Failures
    | NewPpFailure (PredicateFailure (NEWPP crypto)) -- Subtransition Failures
    deriving (Show, Generic, Eq)

  initialRules = [initialEpoch]
  transitionRules = [epochTransition]

instance NoUnexpectedThunks (PredicateFailure (EPOCH crypto))

initialEpoch :: InitialRule (EPOCH crypto)
initialEpoch =
  pure $
    EpochState
      emptyAccount
      emptySnapShots
      emptyLedgerState
      emptyPParams
      emptyPParams
      emptyNonMyopic

votedValuePParams ::
  ProposedPPUpdates crypto ->
  PParams ->
  Int ->
  Maybe PParams
votedValuePParams (ProposedPPUpdates ppup) pps quorumN =
  let incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
      votes =
        Map.foldr
          (\vote tally -> Map.insert vote (incrTally vote tally) tally)
          (Map.empty :: Map PParamsUpdate Int)
          ppup
      consensus = Map.filter (>= quorumN) votes
   in case length consensus of
        1 -> (Just . updatePParams pps . fst . head . Map.toList) consensus
        _ -> Nothing

epochTransition :: forall crypto. Typeable crypto => TransitionRule (EPOCH crypto)
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
    trans @(SNAP crypto) $ TRC (ls, ss, ())

  let PState _ pParams fPParams _ = pstate
      ppp = pParams ⨃ fPParams
      pstate' =
        pstate
          { _pParams = ppp,
            _fPParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(POOLREAP crypto) $ TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  coreNodeQuorum <- liftSTS $ asks quorum

  let ppup = proposals . _ppups $ utxoSt
  let ppNew = votedValuePParams ppup pp (fromIntegral coreNodeQuorum)
  NewppState utxoSt'' acnt'' pp' <-
    trans @(NEWPP crypto) $
      TRC (NewppEnv dstate' pstate'', NewppState utxoSt' acnt' pp, ppNew)
  pure $
    EpochState
      acnt''
      ss'
      (ls {_utxoState = utxoSt'', _delegationState = DPState dstate' pstate''})
      pp
      pp'
      nm

instance Typeable crypto => Embed (SNAP crypto) (EPOCH crypto) where
  wrapFailed = SnapFailure

instance Typeable crypto => Embed (POOLREAP crypto) (EPOCH crypto) where
  wrapFailed = PoolReapFailure

instance Typeable crypto => Embed (NEWPP crypto) (EPOCH crypto) where
  wrapFailed = NewPpFailure
