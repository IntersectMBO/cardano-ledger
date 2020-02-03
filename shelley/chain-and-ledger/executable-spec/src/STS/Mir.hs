{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Mir
  ( MIR
  )
where

import           Address (mkRwdAcnt)
import           BaseTypes (ShelleyBase)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import qualified Data.Map.Strict as Map
import           Delegation.Certificates (StakeCreds (..))
import           EpochBoundary (emptySnapShots)
import           GHC.Generics (Generic)
import           Ledger.Core (dom, (∪+), (◁))
import           LedgerState (EpochState, pattern EpochState, emptyAccount, emptyLedgerState,
                     esAccountState, esLState, esPp, esSnapshots, _delegationState, _dstate, _irwd,
                     _reserves, _rewards, _stkCreds)
import           PParams (emptyPParams)

import           Control.State.Transition

data MIR crypto

instance STS (MIR crypto) where
    type State (MIR crypto) = EpochState crypto
    type Signal (MIR crypto) = ()
    type Environment (MIR crypto) = ()
    type BaseM (MIR crypto) = ShelleyBase
    data PredicateFailure (MIR crypto)
      = MirFailure (PredicateFailure (MIR crypto))
      deriving (Show, Generic, Eq)

    initialRules = [ initialMir ]
    transitionRules = [ mirTransition ]

instance NoUnexpectedThunks (PredicateFailure (MIR crypto))

initialMir :: InitialRule (MIR crypto)
initialMir = pure $ EpochState emptyAccount emptySnapShots emptyLedgerState emptyPParams

mirTransition :: forall crypto . TransitionRule (MIR crypto)
mirTransition = do
  TRC (_, EpochState { esAccountState = acnt
                     , esSnapshots = ss
                     , esLState = ls
                     , esPp = pp}, ()) <- judgmentContext
  let dpState = _delegationState ls
      dState = _dstate dpState
      StakeCreds stkcreds = _stkCreds dState
      irwd' = (dom stkcreds) ◁ (_irwd dState)
      tot = sum irwd'
      reserves = _reserves acnt
      rs = Map.mapKeys mkRwdAcnt irwd'
  if tot <= reserves
    then pure $ EpochState
                  acnt { _reserves = reserves - tot }
                  ss
                  ls { _delegationState =
                         dpState {  _dstate =
                           dState  { _rewards = (_rewards dState) ∪+ rs
                                   , _irwd = Map.empty } } }
                  pp
    else pure $ EpochState
                  acnt
                  ss
                  ls { _delegationState =
                         dpState {  _dstate =
                           dState  { _irwd = Map.empty } } }
                  pp
