{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Transition (
  TransitionConfig (..),
) where

import Cardano.Ledger.Alonzo.Core (AlonzoEraPParams, ppCostModelsL)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Transition (TransitionConfig (MaryTransitionConfig))
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Transition
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition AlonzoEra where
  data TransitionConfig AlonzoEra = AlonzoTransitionConfig
    { atcAlonzoGenesis :: !AlonzoGenesis
    , atcMaryTransitionConfig :: !(TransitionConfig MaryEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = AlonzoTransitionConfig

  injectIntoTestState cfg = 
    shelleyRegisterInitialFundsThenStaking cfg . alonzoInjectCostModels cfg

alonzoInjectCostModels cfg =
  case agExtraConfig $ cfg ^. tcTranslationContextL of
    Nothing -> id
    Just aec -> overrideCostModels (aecCostModels aec)

  tcPreviousEraConfigL =
    lens atcMaryTransitionConfig (\atc pc -> atc {atcMaryTransitionConfig = pc})

  tcTranslationContextL =
    lens atcAlonzoGenesis (\atc ag -> atc {atcAlonzoGenesis = ag})

instance NoThunks (TransitionConfig AlonzoEra)

overrideCostModels ::
  (EraTransition era, AlonzoEraPParams era) =>
  Maybe CostModels ->
  NewEpochState era ->
  NewEpochState era
overrideCostModels Nothing nes = nes
overrideCostModels = \case 
  Nothing -> id 
  Just cms -> nesEsL . curPParamsEpochStateL . ppCostModelsL %~ updateCostModels cms
