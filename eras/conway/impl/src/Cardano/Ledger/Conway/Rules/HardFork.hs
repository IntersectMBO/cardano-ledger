{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.HardFork (
  ConwayHARDFORK,
  ConwayHardForkEvent (..),
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, natVersion)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayHARDFORK)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Shelley.LedgerState
import Control.DeepSeq (NFData)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  tellEvent,
  transitionRules,
 )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro

newtype ConwayHardForkEvent era = ConwayHardForkEvent ProtVer
  deriving (Generic, Eq)
  deriving newtype (NFData)

type instance EraRuleEvent "HARDFORK" ConwayEra = ConwayHardForkEvent ConwayEra

instance
  (EraGov era, EraStake era, EraCertState era, ConwayEraCertState era) =>
  STS (ConwayHARDFORK era)
  where
  type State (ConwayHARDFORK era) = EpochState era
  type Signal (ConwayHARDFORK era) = ProtVer
  type Environment (ConwayHARDFORK era) = ()
  type BaseM (ConwayHARDFORK era) = ShelleyBase
  type PredicateFailure (ConwayHARDFORK era) = Void
  type Event (ConwayHARDFORK era) = ConwayHardForkEvent era

  transitionRules = [hardforkTransition @era]

hardforkTransition ::
  ConwayEraCertState era => TransitionRule (ConwayHARDFORK era)
hardforkTransition = do
  TRC (_, epochState, newPv) <-
    judgmentContext
  tellEvent $ ConwayHardForkEvent newPv
  if pvMajor newPv == natVersion @10
    then
      pure $
        epochState
          & esLStateL . lsCertStateL %~ \certState ->
            let accountsMap = certState ^. certDStateL . accountsL . accountsMapL
                dReps =
                  -- Reset all delegations in order to remove any inconsistencies
                  -- Delegations will be reset accordingly below.
                  Map.map (\dRepState -> dRepState {drepDelegs = Set.empty}) $
                    certState ^. certVStateL . vsDRepsL
                (dRepsWithDelegations, accountsWithoutUnknownDRepDelegations) =
                  Map.mapAccumWithKey adjustDelegations dReps accountsMap
                adjustDelegations ds stakeCred accountState =
                  case accountState ^. dRepDelegationAccountStateL of
                    Just (DRepCredential dRep) ->
                      let addDelegation _ dRepState =
                            Just $ dRepState {drepDelegs = Set.insert stakeCred (drepDelegs dRepState)}
                       in case Map.updateLookupWithKey addDelegation dRep ds of
                            (Nothing, _) -> (ds, accountState & dRepDelegationAccountStateL .~ Nothing)
                            (Just _, ds') -> (ds', accountState)
                    _ -> (ds, accountState)
             in certState
                  -- Remove dangling delegations to non-existent DReps:
                  & certDStateL . accountsL . accountsMapL .~ accountsWithoutUnknownDRepDelegations
                  -- Populate DRep delegations with delegatees
                  & certVStateL . vsDRepsL .~ dRepsWithDelegations
    else pure epochState
