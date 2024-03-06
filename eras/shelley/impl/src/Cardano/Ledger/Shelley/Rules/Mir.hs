{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Mir (
  ShelleyMIR,
  PredicateFailure,
  ShelleyMirPredFailure,
  ShelleyMirEvent (..),
  emptyInstantaneousRewards,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin, addDeltaCoin)
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Shelley.Era (ShelleyMIR)
import Cardano.Ledger.Shelley.Governance (EraGov)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState,
  InstantaneousRewards (..),
  RewardAccounts,
  certDState,
  curPParamsEpochStateL,
  dsIRewards,
  dsUnified,
  esAccountState,
  esLState,
  esNonMyopic,
  esSnapshots,
  lsCertState,
  prevPParamsEpochStateL,
  rewards,
  pattern EpochState,
 )
import Cardano.Ledger.UMap (compactCoinOrError)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val ((<->))
import Control.DeepSeq (NFData)
import Control.SetAlgebra (eval, (∪+))
import Control.State.Transition (
  Assertion (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  tellEvent,
 )
import Data.Default.Class (Default)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

data ShelleyMirPredFailure era
  deriving (Show, Generic, Eq)

instance NFData (ShelleyMirPredFailure era)

data ShelleyMirEvent era
  = MirTransfer (InstantaneousRewards (EraCrypto era))
  | -- | We were not able to perform an MIR transfer due to insufficient funds.
    --   This event gives the rewards we wanted to pay, plus the available
    --   reserves and treasury.
    NoMirTransfer (InstantaneousRewards (EraCrypto era)) Coin Coin
  deriving (Generic)

deriving instance Eq (ShelleyMirEvent era)

instance NFData (ShelleyMirEvent era)

instance NoThunks (ShelleyMirPredFailure era)

instance
  ( Default (EpochState era)
  , EraGov era
  ) =>
  STS (ShelleyMIR era)
  where
  type State (ShelleyMIR era) = EpochState era
  type Signal (ShelleyMIR era) = ()
  type Environment (ShelleyMIR era) = ()
  type BaseM (ShelleyMIR era) = ShelleyBase
  type Event (ShelleyMIR era) = ShelleyMirEvent era
  type PredicateFailure (ShelleyMIR era) = ShelleyMirPredFailure era

  transitionRules = [mirTransition]

  assertions =
    [ PostCondition
        "MIR may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let r = rewards . certDState . lsCertState . esLState
             in length (r st) == length (r st')
        )
    ]

mirTransition ::
  forall era.
  EraGov era =>
  TransitionRule (ShelleyMIR era)
mirTransition = do
  TRC
    ( _
      , es@EpochState
          { esAccountState = acnt
          , esSnapshots = ss
          , esLState = ls
          , esNonMyopic = nm
          }
      , ()
      ) <-
    judgmentContext
  let pr = es ^. prevPParamsEpochStateL
      pp = es ^. curPParamsEpochStateL
      dpState = lsCertState ls
      ds = certDState dpState
      rewards' = rewards ds
      reserves = asReserves acnt
      treasury = asTreasury acnt
      irwdR = rewards' UM.◁ iRReserves (dsIRewards ds) :: RewardAccounts (EraCrypto era)
      irwdT = rewards' UM.◁ iRTreasury (dsIRewards ds) :: RewardAccounts (EraCrypto era)
      totR = fold irwdR
      totT = fold irwdT
      availableReserves = reserves `addDeltaCoin` deltaReserves (dsIRewards ds)
      availableTreasury = treasury `addDeltaCoin` deltaTreasury (dsIRewards ds)
      update = eval (irwdR ∪+ irwdT) :: RewardAccounts (EraCrypto era)

  if totR <= availableReserves && totT <= availableTreasury
    then do
      tellEvent $ MirTransfer ((dsIRewards ds) {iRReserves = irwdR, iRTreasury = irwdT})
      pure $
        EpochState
          acnt
            { asReserves = availableReserves <-> totR
            , asTreasury = availableTreasury <-> totT
            }
          ls
            { lsCertState =
                dpState
                  { certDState =
                      ds
                        { dsUnified = rewards' UM.∪+ Map.map compactCoinOrError update
                        , dsIRewards = emptyInstantaneousRewards
                        }
                  }
            }
          ss
          nm
          & prevPParamsEpochStateL .~ pr
          & curPParamsEpochStateL .~ pp
    else do
      tellEvent $
        NoMirTransfer
          ((dsIRewards ds) {iRReserves = irwdR, iRTreasury = irwdT})
          availableReserves
          availableTreasury
      pure $
        EpochState
          acnt
          ls
            { lsCertState =
                dpState
                  { certDState =
                      ds {dsIRewards = emptyInstantaneousRewards}
                  }
            }
          ss
          nm
          & prevPParamsEpochStateL .~ pr
          & curPParamsEpochStateL .~ pp

emptyInstantaneousRewards :: InstantaneousRewards c
emptyInstantaneousRewards = InstantaneousRewards Map.empty Map.empty mempty mempty
