-- | This module is used for defining initial configuration for all. It is also used in
-- testing and benchmarking to initilize a chain in a particular era without going through
-- the trouble of generating all the history for preceeding eras.
module Cardano.Ledger.Api.Transition (
  EraTransition,
  TransitionConfig,
  mkTransitionConfig,
  mkShelleyTransitionConfig,
  tcShelleyGenesisL,
  tcPreviousEraConfigL,
  tcTranslationContextL,

  -- * Functions for Testing
  tcInitialPParamsG,
  tcInitialFundsL,
  tcInitialStakingL,
  createInitialState,
  registerInitialFunds,
  registerInitialStaking,
) where

import Cardano.Ledger.Api.Era ()
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
