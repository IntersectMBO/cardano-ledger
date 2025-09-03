{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Era.Spec (
  dijkstraEraSpec,
) where

import Test.Cardano.Ledger.Conway.Era.Spec (conwayEraSpec)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

-- | This spec is applicable to all eras and will be executed for every era starting with Dijkstra.
dijkstraEraSpec :: forall era. ConwayEraImp era => Spec
dijkstraEraSpec = do
  conwayEraSpec @era
