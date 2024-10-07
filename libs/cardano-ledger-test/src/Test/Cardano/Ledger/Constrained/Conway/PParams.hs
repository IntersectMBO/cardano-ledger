{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Mostly for backward compatibility as many modules import
--   this one to get 'pparamsSpec'
module Test.Cardano.Ledger.Constrained.Conway.PParams where

import Cardano.Ledger.Core (PParams (..))
import Constrained
import Test.Cardano.Ledger.Constrained.Conway.SimplePParams (
  EraSpecPParams (..),
  simplePParamsSpec,
 )

pparamsSpec ::
  forall fn era. (EraSpecPParams era, BaseUniverse fn) => Specification fn (PParams era)
pparamsSpec = constrained' $ \simplepp -> satisfies simplepp (simplePParamsSpec @fn @era)
