{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Dijkstra.NewEpoch () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), SpecTRC (..), unComputationResult_)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra ()
import Test.Cardano.Ledger.Dijkstra.ImpTest ()

instance ExecSpecRule "NEWEPOCH" DijkstraEra where
  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.newEpochStep env st sig
