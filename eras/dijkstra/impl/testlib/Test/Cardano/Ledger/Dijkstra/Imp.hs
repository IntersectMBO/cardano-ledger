{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Imp where

import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Shelley.Rules
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as ConwayImp
import Test.Cardano.Ledger.Dijkstra.ImpTest

spec ::
  forall era.
  ( DijkstraEraImp era
  , EraSpecificSpec era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  ) =>
  Spec
spec = ConwayImp.spec @era

instance EraSpecificSpec DijkstraEra
