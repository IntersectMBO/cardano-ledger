{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Era.Spec (
  shelleyEraSpec,
) where

import Test.Cardano.Ledger.Era.Spec
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

-- | This spec is applicable to all eras and will be executed for every era starting with Shelley.
shelleyEraSpec :: forall era. ShelleyEraImp era => Spec
shelleyEraSpec = do
  everyEraSpec @era
