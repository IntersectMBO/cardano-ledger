{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Era.Spec (
  maryEraSpec,
) where

import Test.Cardano.Ledger.Mary.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Allegra.Era.Spec (allegraEraSpec)

-- | This spec is applicable to all eras and will be executed for every era starting with Mary.
maryEraSpec :: forall era. MaryEraImp era => Spec
maryEraSpec = do
  allegraEraSpec @era
