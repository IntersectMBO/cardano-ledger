{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.HardFork (
  HARDFORK,
  ConwayHardForkEvent (..),
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, HARDFORK)
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
  STS (HARDFORK era)
  where
  type State (HARDFORK era) = EpochState era
  type Signal (HARDFORK era) = ProtVer
  type Environment (HARDFORK era) = ()
  type BaseM (HARDFORK era) = ShelleyBase
  type PredicateFailure (HARDFORK era) = Void
  type Event (HARDFORK era) = ConwayHardForkEvent era

  transitionRules = [hardforkTransition @era]

hardforkTransition ::
  ConwayEraCertState era => TransitionRule (HARDFORK era)
hardforkTransition = do
  TRC (_, epochState, newPv) <-
    judgmentContext
  tellEvent $ ConwayHardForkEvent newPv
  let update
        | pvMajor newPv == natVersion @10 =
            esLStateL . lsCertStateL %~ updateDRepDelegations
        | pvMajor newPv == natVersion @11 =
            esLStateL . lsCertStateL . certPStateL %~ populateVRFKeyHashes
        | otherwise = id
  pure $ update epochState

updateDRepDelegations :: ConwayEraCertState era => CertState era -> CertState era
updateDRepDelegations certState =
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
