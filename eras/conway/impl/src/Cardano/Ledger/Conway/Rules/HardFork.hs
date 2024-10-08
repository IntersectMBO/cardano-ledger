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
)
where

import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, StrictMaybe (..), natVersion)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayHARDFORK)
import Cardano.Ledger.DRep
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

newtype ConwayHardForkEvent era = ConwayHardForkEvent ProtVer
  deriving (Generic, Eq)
  deriving newtype (NFData)

type instance EraRuleEvent "HARDFORK" (ConwayEra c) = ConwayHardForkEvent (ConwayEra c)

instance
  EraGov era =>
  STS (ConwayHARDFORK era)
  where
  type State (ConwayHARDFORK era) = EpochState era
  type Signal (ConwayHARDFORK era) = ProtVer
  type Environment (ConwayHARDFORK era) = ()
  type BaseM (ConwayHARDFORK era) = ShelleyBase
  type PredicateFailure (ConwayHARDFORK era) = Void
  type Event (ConwayHARDFORK era) = ConwayHardForkEvent era

  transitionRules = [hardforkTransition @era]

hardforkTransition :: TransitionRule (ConwayHARDFORK era)
hardforkTransition = do
  TRC (_, epochState, newPv) <-
    judgmentContext
  tellEvent $ ConwayHardForkEvent newPv
  if pvMajor newPv == natVersion @10
    then
      pure $
        epochState
          & esLStateL . lsCertStateL %~ \certState ->
            let umap = certState ^. certDStateL . dsUnifiedL
                dReps = certState ^. certVStateL . vsDRepsL
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
