{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Mostly for backward compatibility as many modules import
--   this one to get 'pparamsSpec'
module Test.Cardano.Ledger.Constrained.Conway.Experiment.PParams where

import Cardano.Ledger.Core (PParams (..))
import Constrained.Experiment.API
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Instances.PParams (
  EraSpecPParams (..),
  simplePParamsSpec,
 )

pparamsSpec ::
  forall era. EraSpecPParams era => Specification (PParams era)
pparamsSpec = constrained' $ \simplepp -> satisfies simplepp (simplePParamsSpec @era)
