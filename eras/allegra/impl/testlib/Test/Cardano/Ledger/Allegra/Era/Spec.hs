{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Era.Spec (
  allegraEraSpec,
) where

import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.Era.Spec (shelleyEraSpec)

-- | This spec is applicable to all eras and will be executed for every era starting with Allegra.
allegraEraSpec :: forall era. ShelleyEraImp era => Spec
allegraEraSpec = do
  shelleyEraSpec @era
