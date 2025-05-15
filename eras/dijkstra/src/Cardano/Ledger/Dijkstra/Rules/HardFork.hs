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

module Cardano.Ledger.Dijkstra.Rules.HardFork (
  DijkstraHARDFORK,
  DijkstraHardForkEvent (..),
)
where

import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, StrictMaybe (..), natVersion)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraHARDFORK)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.UMap as UM
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

newtype DijkstraHardForkEvent era = DijkstraHardForkEvent ProtVer
  deriving (Generic, Eq)
  deriving newtype (NFData)

type instance EraRuleEvent "HARDFORK" DijkstraEra = DijkstraHardForkEvent DijkstraEra

instance
  (EraGov era, EraStake era, EraCertState era, DijkstraEraCertState era) =>
  STS (DijkstraHARDFORK era)
  where
  type State (DijkstraHARDFORK era) = EpochState era
  type Signal (DijkstraHARDFORK era) = ProtVer
  type Environment (DijkstraHARDFORK era) = ()
  type BaseM (DijkstraHARDFORK era) = ShelleyBase
  type PredicateFailure (DijkstraHARDFORK era) = Void
  type Event (DijkstraHARDFORK era) = DijkstraHardForkEvent era

  transitionRules = [hardforkTransition @era]

hardforkTransition ::
  DijkstraEraCertState era => TransitionRule (DijkstraHARDFORK era)
hardforkTransition = do
  TRC (_, epochState, newPv) <-
    judgmentContext
  tellEvent $ DijkstraHardForkEvent newPv
  if pvMajor newPv == natVersion @10
    then
      pure $
        epochState
          & esLStateL . lsCertStateL %~ \certState ->
            let umap = certState ^. certDStateL . dsUnifiedL
                dReps =
                  -- Reset all delegations in order to remove any inconsistencies
                  -- Delegations will be reset accordingly below.
                  Map.map (\dRepState -> dRepState {drepDelegs = Set.empty}) $
                    certState ^. certVStateL . vsDRepsL
                (dRepsWithDelegations, elemsWithoutUnknownDRepDelegations) =
                  Map.mapAccumWithKey adjustDelegations dReps (UM.umElems umap)
                adjustDelegations ds stakeCred umElem@(UM.UMElem rd ptr stakePool mDrep) =
                  case mDrep of
                    SJust (DRepCredential dRep) ->
                      let addDelegation _ dRepState =
                            Just $ dRepState {drepDelegs = Set.insert stakeCred (drepDelegs dRepState)}
                       in case Map.updateLookupWithKey addDelegation dRep ds of
                            (Nothing, _) -> (ds, UM.UMElem rd ptr stakePool SNothing)
                            (Just _, ds') -> (ds', umElem)
                    _ -> (ds, umElem)
             in certState
                  -- Remove dangling delegations to non-existent DReps:
                  & (certDStateL . dsUnifiedL .~ umap {UM.umElems = elemsWithoutUnknownDRepDelegations})
                  -- Populate DRep delegations with delegatees
                  & (certVStateL . vsDRepsL .~ dRepsWithDelegations)
    else pure epochState
