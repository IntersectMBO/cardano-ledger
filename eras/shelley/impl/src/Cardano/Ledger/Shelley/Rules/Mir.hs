{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Mir
  ( MIR,
    PredicateFailure,
    MirPredicateFailure,
    MirEvent (..),
    emptyInstantaneousRewards,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin, addDeltaCoin)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    EpochState,
    InstantaneousRewards (..),
    RewardAccounts,
    esAccountState,
    esLState,
    esNonMyopic,
    esPp,
    esPrevPp,
    esSnapshots,
    rewards,
    lsDPState,
    dpsDState,
    _irwd,
    _unified,
    pattern EpochState,
  )
import Cardano.Ledger.Val ((<->))
import Control.SetAlgebra (dom, eval, (∪+), (◁))
import Control.State.Transition
  ( Assertion (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    tellEvent,
  )
import Data.Default.Class (Default)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import qualified Data.UMap as UM
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data MIR era

data MirPredicateFailure era
  deriving (Show, Generic, Eq)

data MirEvent era
  = MirTransfer (InstantaneousRewards (Crypto era))
  | -- | We were not able to perform an MIR transfer due to insufficient funds.
    --   This event gives the rewards we wanted to pay, plus the available
    --   reserves and treasury.
    NoMirTransfer (InstantaneousRewards (Crypto era)) Coin Coin

instance NoThunks (MirPredicateFailure era)

instance (Typeable era, Default (EpochState era)) => STS (MIR era) where
  type State (MIR era) = EpochState era
  type Signal (MIR era) = ()
  type Environment (MIR era) = ()
  type BaseM (MIR era) = ShelleyBase
  type Event (MIR era) = MirEvent era
  type PredicateFailure (MIR era) = MirPredicateFailure era

  transitionRules = [mirTransition]

  assertions =
    [ PostCondition
        "MIR may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let r = rewards . dpsDState . lsDPState . esLState
             in length (r st) == length (r st')
        )
    ]

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
  let dpState = lsDPState ls
      ds = dpsDState dpState
      rewards' = rewards ds
      reserves = _reserves acnt
      treasury = _treasury acnt
      irwdR = eval $ dom rewards' ◁ iRReserves (_irwd ds) :: RewardAccounts (Crypto era)
      irwdT = eval $ dom rewards' ◁ iRTreasury (_irwd ds) :: RewardAccounts (Crypto era)
      totR = fold irwdR
      totT = fold irwdT
      availableReserves = reserves `addDeltaCoin` (deltaReserves . _irwd $ ds)
      availableTreasury = treasury `addDeltaCoin` (deltaTreasury . _irwd $ ds)
      update = eval (irwdR ∪+ irwdT) :: RewardAccounts (Crypto era)

  if totR <= availableReserves && totT <= availableTreasury
    then do
      tellEvent $ MirTransfer ((_irwd ds) {iRReserves = irwdR, iRTreasury = irwdT})
      pure $
        EpochState
          acnt
            { _reserves = availableReserves <-> totR,
              _treasury = availableTreasury <-> totT
            }
          ss
          ls
            { lsDPState =
                dpState
                  { dpsDState =
                      ds
                        { _unified = (rewards' UM.∪+ update),
                          _irwd = emptyInstantaneousRewards
                        }
                  }
            }
          pr
          pp
          nm
    else do
      tellEvent $
        NoMirTransfer
          ((_irwd ds) {iRReserves = irwdR, iRTreasury = irwdT})
          availableReserves
          availableTreasury
      pure $
        EpochState
          acnt
          ss
          ls
            { lsDPState =
                dpState
                  { dpsDState =
                      ds {_irwd = emptyInstantaneousRewards}
                  }
            }
          pr
          pp
          nm

emptyInstantaneousRewards :: InstantaneousRewards crypto
emptyInstantaneousRewards = InstantaneousRewards Map.empty Map.empty mempty mempty
