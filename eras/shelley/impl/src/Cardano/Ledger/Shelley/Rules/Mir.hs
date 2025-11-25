{-# LANGUAGE DataKinds #-}
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
  ShelleyMirEvent (..),
  emptyInstantaneousRewards,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin, addDeltaCoin, compactCoinOrError)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyMIR)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  curPParamsEpochStateL,
  esChainAccountState,
  esLState,
  esLStateL,
  esNonMyopic,
  esSnapshots,
  lsCertStateL,
  prevPParamsEpochStateL,
  pattern EpochState,
 )
import Cardano.Ledger.State
import Cardano.Ledger.Val ((<->))
import Control.DeepSeq (NFData)
import Control.State.Transition (
  Assertion (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  tellEvent,
 )
import Data.Default (Default)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro

data ShelleyMirEvent era
  = MirTransfer InstantaneousRewards
  | -- | We were not able to perform an MIR transfer due to insufficient funds.
    --   This event gives the rewards we wanted to pay, plus the available
    --   reserves and treasury.
    NoMirTransfer InstantaneousRewards Coin Coin
  deriving (Generic)

deriving instance Eq (ShelleyMirEvent era)

instance NFData (ShelleyMirEvent era)

instance
  ( Default (EpochState era)
  , EraGov era
  , EraCertState era
  ) =>
  STS (ShelleyMIR era)
  where
  type State (ShelleyMIR era) = EpochState era
  type Signal (ShelleyMIR era) = ()
  type Environment (ShelleyMIR era) = ()
  type BaseM (ShelleyMIR era) = ShelleyBase
  type Event (ShelleyMIR era) = ShelleyMirEvent era
  type PredicateFailure (ShelleyMIR era) = Void

  transitionRules = [mirTransition]

  assertions =
    [ PostCondition
        "MIR may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let accountsCount esl =
                  Map.size (esl ^. esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL)
             in accountsCount st == accountsCount st'
        )
    ]

mirTransition ::
  forall era.
  (EraGov era, EraCertState era) =>
  TransitionRule (ShelleyMIR era)
mirTransition = do
  TRC
    ( _
      , es@EpochState
          { esChainAccountState = chainAccountState
          , esSnapshots = ss
          , esLState = ls
          , esNonMyopic = nm
          }
      , ()
      ) <-
    judgmentContext
  let pr = es ^. prevPParamsEpochStateL
      pp = es ^. curPParamsEpochStateL
      dpState = ls ^. lsCertStateL
      ds = dpState ^. certDStateL
      accountsMap = ds ^. accountsL . accountsMapL
      reserves = casReserves chainAccountState
      treasury = casTreasury chainAccountState
      irwdR = iRReserves (dsIRewards ds) `Map.intersection` accountsMap
      irwdT = iRTreasury (dsIRewards ds) `Map.intersection` accountsMap
      totR = fold irwdR
      totT = fold irwdT
      availableReserves = reserves `addDeltaCoin` deltaReserves (dsIRewards ds)
      availableTreasury = treasury `addDeltaCoin` deltaTreasury (dsIRewards ds)
      update = Map.unionWith (<>) irwdR irwdT :: Map.Map (Credential Staking) Coin

  if totR <= availableReserves && totT <= availableTreasury
    then do
      tellEvent $ MirTransfer ((dsIRewards ds) {iRReserves = irwdR, iRTreasury = irwdT})
      pure $
        EpochState
          ChainAccountState
            { casReserves = availableReserves <-> totR
            , casTreasury = availableTreasury <-> totT
            }
          ( ls
              & lsCertStateL . certDStateL . accountsL
                %~ addToBalanceAccounts (Map.map compactCoinOrError update)
              & lsCertStateL . certDStateL . dsIRewardsL .~ emptyInstantaneousRewards
          )
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
          chainAccountState
          ( ls
              & lsCertStateL . certDStateL . dsIRewardsL .~ emptyInstantaneousRewards
          )
          ss
          nm
          & prevPParamsEpochStateL .~ pr
          & curPParamsEpochStateL .~ pp

emptyInstantaneousRewards :: InstantaneousRewards
emptyInstantaneousRewards = InstantaneousRewards Map.empty Map.empty mempty mempty
