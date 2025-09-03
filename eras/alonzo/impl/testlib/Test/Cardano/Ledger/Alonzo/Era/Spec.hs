{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Era.Spec (
  alonzoEraSpec,
) where

import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.Era.Spec (maryEraSpec)

-- | This spec is applicable to all eras and will be executed for every era starting with Alonzo.
alonzoEraSpec :: forall era. AlonzoEraImp era => Spec
alonzoEraSpec = do
  maryEraSpec @era
