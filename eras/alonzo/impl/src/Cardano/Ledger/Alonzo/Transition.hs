{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Transition (
  TransitionConfig (..),
  alonzoInjectCostModels,
) where

import Cardano.Ledger.Alonzo.Core (AlonzoEraPParams, ppCostModelsL)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Transition (TransitionConfig (MaryTransitionConfig))
import Cardano.Ledger.Plutus.CostModels (CostModels, updateCostModels)
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

  tcPreviousEraConfigL =
    lens atcMaryTransitionConfig (\atc pc -> atc {atcMaryTransitionConfig = pc})

  tcTranslationContextL =
    lens atcAlonzoGenesis (\atc ag -> atc {atcAlonzoGenesis = ag})

instance NoThunks (TransitionConfig AlonzoEra)

alonzoInjectCostModels ::
  (EraTransition era, AlonzoEraPParams era) =>
  TransitionConfig AlonzoEra -> NewEpochState era -> NewEpochState era
alonzoInjectCostModels cfg =
  case agExtraConfig $ cfg ^. tcTranslationContextL of
    Nothing -> id
    Just aec -> overrideCostModels (aecCostModels aec)

overrideCostModels ::
  (EraTransition era, AlonzoEraPParams era) =>
  Maybe CostModels ->
  NewEpochState era ->
  NewEpochState era
overrideCostModels = \case
  Nothing -> id
  -- The injected cost models must OVERRIDE the era-translated ones (including the
  -- count-locked PlutusV1/PlutusV3 genesis fields), so a testnet/benchmark can carry
  -- full cost models without an on-chain parameter update. `updateCostModels` lets its
  -- SECOND argument win, so the injected `cms` must be passed second (via `flip`).
  -- See IntersectMBO/cardano-ledger#5342.
  Just cms -> nesEsL . curPParamsEpochStateL . ppCostModelsL %~ flip updateCostModels cms
