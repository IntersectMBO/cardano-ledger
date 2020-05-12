{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Mir
  ( MIR,
    PredicateFailure,
  )
where

import Byron.Spec.Ledger.Core (dom, (∪+), (◁))
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address (mkRwdAcnt)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Delegation.Certificates (StakeCreds (..))
import Shelley.Spec.Ledger.EpochBoundary (emptySnapShots)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState,
    _delegationState,
    _dstate,
    _irwd,
    _reserves,
    _rewards,
    _stkCreds,
    emptyAccount,
    emptyLedgerState,
    esAccountState,
    esLState,
    esNonMyopic,
    esPp,
    esPrevPp,
    esSnapshots,
    pattern EpochState,
  )
import Shelley.Spec.Ledger.PParams (emptyPParams)
import Shelley.Spec.Ledger.Rewards (emptyNonMyopic)

data MIR crypto

instance STS (MIR crypto) where
  type State (MIR crypto) = EpochState crypto
  type Signal (MIR crypto) = ()
  type Environment (MIR crypto) = ()
  type BaseM (MIR crypto) = ShelleyBase
  data PredicateFailure (MIR crypto)
    deriving (Show, Generic, Eq)

  initialRules = [initialMir]
  transitionRules = [mirTransition]

instance NoUnexpectedThunks (PredicateFailure (MIR crypto))

initialMir :: InitialRule (MIR crypto)
initialMir =
  pure $
    EpochState
      emptyAccount
      emptySnapShots
      emptyLedgerState
      emptyPParams
      emptyPParams
      emptyNonMyopic

mirTransition :: forall crypto. TransitionRule (MIR crypto)
mirTransition = do
  TRC
    ( _,
      EpochState
        { esAccountState = acnt,
          esSnapshots = ss,
          esLState = ls,
          esPrevPp = pr,
          esPp = pp,
          esNonMyopic = nm
        },
      ()
      ) <-
    judgmentContext
  let dpState = _delegationState ls
      dState = _dstate dpState
      StakeCreds stkcreds = _stkCreds dState
      irwd' = (dom stkcreds) ◁ (_irwd dState)
      tot = sum irwd'
      reserves = _reserves acnt
      update = Map.mapKeys mkRwdAcnt irwd'
  if tot <= reserves
    then
      pure $
        EpochState
          acnt {_reserves = reserves - tot}
          ss
          ls
            { _delegationState =
                dpState
                  { _dstate =
                      dState
                        { _rewards = (_rewards dState) ∪+ update,
                          _irwd = Map.empty
                        }
                  }
            }
          pr
          pp
          nm
    else
      pure $
        EpochState
          acnt
          ss
          ls
            { _delegationState =
                dpState
                  { _dstate =
                      dState {_irwd = Map.empty}
                  }
            }
          pr
          pp
          nm
