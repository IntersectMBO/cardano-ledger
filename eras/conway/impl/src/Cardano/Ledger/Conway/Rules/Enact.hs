{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Enact (
  ConwayENACT,
  EnactSignal (..),
  EnactState (..),
) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes
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
import Cardano.Ledger.Val (Val (..))
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Lens.Micro

data EnactSignal era = EnactSignal
  { esGovActionId :: !(GovActionId (EraCrypto era))
  , esGovAction :: !(GovAction era)
  }

instance EraGov era => STS (ConwayENACT era) where
  type Environment (ConwayENACT era) = ()
  type PredicateFailure (ConwayENACT era) = Void
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
