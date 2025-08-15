{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Era.Spec (
  conwayEraSpec,
) where

import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Babbage.Era.Spec (babbageEraSpec)

-- | This spec is applicable to all eras and will be executed for every era starting with Conway.
conwayEraSpec :: forall era. ConwayEraImp era => Spec
conwayEraSpec = do
  babbageEraSpec @era
