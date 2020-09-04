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

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (dom, eval, (∪+), (◁))
import Control.State.Transition
  ( Assertion (..),
    InitialRule,
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
  )
import Data.Foldable (fold)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.EpochBoundary (emptySnapShots)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    EpochState,
    InstantaneousRewards (..),
    RewardAccounts,
    emptyAccount,
    emptyInstantaneousRewards,
    emptyLedgerState,
    esAccountState,
    esLState,
    esNonMyopic,
    esPp,
    esPrevPp,
    esSnapshots,
    _delegationState,
    _dstate,
    _irwd,
    _rewards,
    pattern EpochState,
  )
import Shelley.Spec.Ledger.PParams (emptyPParams)
import Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import qualified Cardano.Ledger.Val as Val

data MIR era

data MirPredicateFailure era
  deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (MirPredicateFailure era)

instance Typeable era => STS (MIR era) where
  type State (MIR era) = EpochState era
  type Signal (MIR era) = ()
  type Environment (MIR era) = ()
  type BaseM (MIR era) = ShelleyBase
  type PredicateFailure (MIR era) = MirPredicateFailure era

  initialRules = [initialMir]
  transitionRules = [mirTransition]

  assertions =
    [ PostCondition
        "MIR may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let r = _rewards . _dstate . _delegationState . esLState
             in length (r st) == length (r st')
        )
    ]

initialMir :: InitialRule (MIR era)
initialMir =
  pure $
    EpochState
      emptyAccount
      emptySnapShots
      emptyLedgerState
      emptyPParams
      emptyPParams
      emptyNonMyopic

mirTransition :: forall era. TransitionRule (MIR era)
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
      ds = _dstate dpState
      rewards = _rewards ds
      reserves = _reserves acnt
      treasury = _treasury acnt
      irwdR = eval $ (dom rewards) ◁ (iRReserves $ _irwd ds) :: RewardAccounts era
      irwdT = eval $ (dom rewards) ◁ (iRTreasury $ _irwd ds) :: RewardAccounts era
      totR = fold irwdR
      totT = fold irwdT
      update = (eval (irwdR ∪+ irwdT)) :: RewardAccounts era

  if totR <= reserves && totT <= treasury
    then
      pure $
        EpochState
          acnt
            { _reserves = reserves Val.~~ totR,
              _treasury = treasury Val.~~ totT
            }
          ss
          ls
            { _delegationState =
                dpState
                  { _dstate =
                      ds
                        { _rewards = eval ((_rewards ds) ∪+ update),
                          _irwd = emptyInstantaneousRewards
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
                      ds {_irwd = emptyInstantaneousRewards}
                  }
            }
          pr
          pp
          nm
