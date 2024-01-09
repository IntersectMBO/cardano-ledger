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
  Committee (..),
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  GovPurposeId (GovPurposeId),
  ensCommitteeL,
  ensConstitutionL,
  ensCurPParamsL,
  ensPrevCommitteeL,
  ensPrevConstitutionL,
  ensPrevHardForkL,
  ensPrevPParamUpdateL,
  ensProtVerL,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Val (Val (..))
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
          & ensPrevPParamUpdateL .~ SJust (GovPurposeId govActionId)
    HardForkInitiation _ pv ->
      pure $
        st
          & ensProtVerL .~ pv
          & ensPrevHardForkL .~ SJust (GovPurposeId govActionId)
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
          & ensPrevCommitteeL .~ SJust (GovPurposeId govActionId)
    UpdateCommittee _ membersToRemove membersToAdd newQuorum -> do
      pure $
        st
          & ensCommitteeL %~ SJust . updatedCommittee membersToRemove membersToAdd newQuorum
          & ensPrevCommitteeL .~ SJust (GovPurposeId govActionId)
    NewConstitution _ c ->
      pure $
        st
          & ensConstitutionL .~ c
          & ensPrevConstitutionL .~ SJust (GovPurposeId govActionId)
    InfoAction -> pure st

updatedCommittee ::
  Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo ->
  UnitInterval ->
  StrictMaybe (Committee era) ->
  Committee era
updatedCommittee membersToRemove membersToAdd newQuorum committee =
  case committee of
    SNothing -> Committee membersToAdd newQuorum
    SJust (Committee currentMembers _) ->
      let newCommitteeMembers =
            Map.union
              membersToAdd
              (currentMembers `Map.withoutKeys` membersToRemove)
       in Committee
            newCommitteeMembers
            newQuorum
