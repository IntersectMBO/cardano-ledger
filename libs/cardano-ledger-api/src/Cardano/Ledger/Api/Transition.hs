-- | This module is used for defining initial configuration for all. It is also used in
-- testing and benchmarking to initilize a chain in a particular era without going through
-- the trouble of generating all the history for preceeding eras.
module Cardano.Ledger.Api.Transition (
  EraTransition,
  TransitionConfig,
  mkLatestTransitionConfig,
  mkTransitionConfig,
  mkShelleyTransitionConfig,
  tcShelleyGenesisL,
  tcPreviousEraConfigL,
  tcTranslationContextL,

  -- * Genesis
  EraGenesis (..),
  ShelleyGenesis (..),
  AlonzoGenesis (..),
  ConwayGenesis (..),
  NoGenesis (..),

  -- * Functions for Testing
  tcInitialPParamsG,
  tcInitialFundsL,
  tcInitialStakingL,
  createInitialState,
  injectIntoTestState,
) where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Api.Era (LatestKnownEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Genesis (EraGenesis (..), NoGenesis (..))
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import Cardano.Ledger.Shelley.Transition (
  EraTransition (..),
  TransitionConfig,
  createInitialState,
  mkShelleyTransitionConfig,
  tcInitialFundsL,
  tcInitialStakingL,
 )
import Data.Function ((&))

mkLatestTransitionConfig ::
  ShelleyGenesis ->
  AlonzoGenesis ->
  ConwayGenesis ->
  TransitionConfig (LatestKnownEra)
mkLatestTransitionConfig shelleyGenesis alonzoGenesis conwayGenesis =
  mkShelleyTransitionConfig shelleyGenesis
    & mkTransitionConfig NoGenesis
    & mkTransitionConfig NoGenesis
    & mkTransitionConfig alonzoGenesis
    & mkTransitionConfig NoGenesis
    & mkTransitionConfig conwayGenesis
