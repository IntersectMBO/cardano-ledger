{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Transition (
  AlonzoEraTransition (..),
  TransitionConfig (..),
  alonzoInjectCostModels,
) where

import Cardano.Ledger.Alonzo.Core (AlonzoEraPParams, PreviousEra, ppCostModelsL)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Transition (TransitionConfig (MaryTransitionConfig))
import Cardano.Ledger.Plutus.CostModels (CostModels, CostModelsUpdate (..), updateCostModels)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Transition
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class (EraTransition era, AlonzoEraPParams era) => AlonzoEraTransition era where
  -- | Lens for the `AlonzoGenesis` from the `TransitionConfig`. Default implementation
  -- looks in the previous era's config
  tcAlonzoGenesisL :: Lens' (TransitionConfig era) AlonzoGenesis
  default tcAlonzoGenesisL ::
    AlonzoEraTransition (PreviousEra era) =>
    Lens' (TransitionConfig era) AlonzoGenesis
  tcAlonzoGenesisL = tcPreviousEraConfigL . tcAlonzoGenesisL

instance EraTransition AlonzoEra where
  data TransitionConfig AlonzoEra = AlonzoTransitionConfig
    { atcAlonzoGenesis :: !AlonzoGenesis
    , atcMaryTransitionConfig :: !(TransitionConfig MaryEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = AlonzoTransitionConfig

  injectIntoTestState hasFS cfg newEpochState =
    shelleyRegisterInitialFundsThenStaking hasFS cfg (alonzoInjectCostModels cfg newEpochState)

  tcPreviousEraConfigL =
    lens atcMaryTransitionConfig (\atc pc -> atc {atcMaryTransitionConfig = pc})

  tcTranslationContextL =
    lens atcAlonzoGenesis (\atc ag -> atc {atcAlonzoGenesis = ag})

instance AlonzoEraTransition AlonzoEra where
  tcAlonzoGenesisL = tcTranslationContextL

instance NoThunks (TransitionConfig AlonzoEra)

alonzoInjectCostModels ::
  AlonzoEraTransition era =>
  TransitionConfig era -> NewEpochState era -> NewEpochState era
alonzoInjectCostModels cfg =
  case agExtraConfig $ cfg ^. tcAlonzoGenesisL of
    SNothing -> id
    SJust aec -> overrideCostModels (aecCostModels aec)

overrideCostModels ::
  (EraTransition era, AlonzoEraPParams era) =>
  Maybe CostModels ->
  NewEpochState era ->
  NewEpochState era
overrideCostModels = \case
  Nothing -> id
  -- Injected cost models override the era-translated ones (the fixed-length
  -- PlutusV1/PlutusV3 genesis fields), so a testnet can carry full cost models
  -- without an on-chain parameter update.
  Just cms ->
    nesEsL . curPParamsEpochStateL . ppCostModelsL %~ flip updateCostModels (CostModelsUpdate cms)
