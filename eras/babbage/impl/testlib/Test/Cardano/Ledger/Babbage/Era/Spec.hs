{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Era.Spec (
  babbageEraSpec,
) where

import Test.Cardano.Ledger.Alonzo.Era.Spec (alonzoEraSpec)
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Imp.Common

-- | This spec is applicable to all eras and will be executed for every era starting with Babbage.
babbageEraSpec :: forall era. AlonzoEraImp era => Spec
babbageEraSpec = do
  alonzoEraSpec @era
