{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Enact (
  ConwayENACT,
  EnactSignal (..),
  EnactState (..),
  EnactPredFailure (..),
) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayENACT)
import Cardano.Ledger.Conway.Governance (
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  PrevGovActionId (..),
  ensCommitteeL,
  ensConstitutionL,
  ensCurPParamsL,
  ensPrevCommitteeL,
  ensPrevConstitutionL,
  ensPrevHardForkL,
  ensPrevPParamUpdateL,
  ensProtVerL,
 )
import Cardano.Ledger.Rules.ValidationMode (Inject (..), runTest)
import Cardano.Ledger.Val (Val (..))
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Validation (failureUnless)

data EnactPredFailure era
  = EnactTreasuryInsufficientFunds !(Map (RewardAcnt (EraCrypto era)) Coin) !Coin
  deriving (Show, Eq)

data EnactSignal era = EnactSignal
  { esGovActionId :: !(GovActionId (EraCrypto era))
  , esGovAction :: !(GovAction era)
  }

instance EraGov era => STS (ConwayENACT era) where
  type Environment (ConwayENACT era) = ()
  type PredicateFailure (ConwayENACT era) = EnactPredFailure era
  type Signal (ConwayENACT era) = EnactSignal era
  type State (ConwayENACT era) = EnactState era
  type BaseM (ConwayENACT era) = ShelleyBase

  initialRules = []
  transitionRules = [enactmentTransition]

enactmentTransition :: forall era. EraPParams era => TransitionRule (ConwayENACT era)
enactmentTransition = do
  TRC ((), st, EnactSignal govActionId act) <- judgmentContext

  case act of
    ParameterChange _ ppup ->
      pure $
        st
          & ensCurPParamsL %~ (`applyPPUpdates` ppup)
          & ensPrevPParamUpdateL .~ SJust (PrevGovActionId govActionId)
    HardForkInitiation _ pv ->
      pure $
        st
          & ensProtVerL .~ pv
          & ensPrevHardForkL .~ SJust (PrevGovActionId govActionId)
    TreasuryWithdrawals wdrls -> do
      let wdrlsAmount = fold wdrls
          wdrlsNoNetworkId = Map.mapKeys getRwdCred wdrls
      runTest
        . failureUnless (wdrlsAmount <= ensTreasury st)
        $ EnactTreasuryInsufficientFunds @era wdrls (ensTreasury st)
      pure
        st
          { ensWithdrawals = Map.unionWith (<>) wdrlsNoNetworkId $ ensWithdrawals st
          , ensTreasury = ensTreasury st <-> wdrlsAmount
          }
    NoConfidence _ ->
      pure $
        st
          & ensCommitteeL .~ SNothing
          & ensPrevCommitteeL .~ SJust (PrevGovActionId govActionId)
    NewCommittee _ _ committee ->
      pure $
        st
          & ensCommitteeL .~ SJust committee -- TODO: check old members
          & ensPrevCommitteeL .~ SJust (PrevGovActionId govActionId)
    NewConstitution _ c ->
      pure $
        st
          & ensConstitutionL .~ c
          & ensPrevConstitutionL .~ SJust (PrevGovActionId govActionId)
    InfoAction -> pure st

instance Inject (EnactPredFailure era) (EnactPredFailure era) where
  inject = id
