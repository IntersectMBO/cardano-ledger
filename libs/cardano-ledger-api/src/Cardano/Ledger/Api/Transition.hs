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
  ShelleyGenesis (..),
  AlonzoGenesis (..),
  ConwayGenesis (..),

  -- * Functions for Testing
  tcInitialPParamsG,
  tcInitialFundsL,
  tcInitialStakingL,
  createInitialState,
  registerInitialFunds,
  registerInitialStaking,
  registerInitialDReps,
  registerDelegs,
) where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Api.Era (LatestKnownEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Transition (
  registerDelegs,
  registerInitialDReps,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import Cardano.Ledger.Shelley.Transition (
  EraTransition (..),
  TransitionConfig,
  createInitialState,
  mkShelleyTransitionConfig,
  registerInitialFunds,
  registerInitialStaking,
  tcInitialFundsL,
  tcInitialStakingL,
 )
import Data.Function ((&))

mkLatestTransitionConfig ::
  Crypto c =>
  ShelleyGenesis c ->
  AlonzoGenesis ->
  ConwayGenesis c ->
  TransitionConfig (LatestKnownEra c)
mkLatestTransitionConfig shelleyGenesis alonzoGenesis conwayGenesis =
  mkShelleyTransitionConfig shelleyGenesis
    & mkTransitionConfig ()
    & mkTransitionConfig ()
    & mkTransitionConfig alonzoGenesis
    & mkTransitionConfig ()
    & mkTransitionConfig conwayGenesis
