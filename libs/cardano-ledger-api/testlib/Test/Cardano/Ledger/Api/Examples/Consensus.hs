{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Api.Examples.Consensus (
  Shelley.LedgerExamples (..),
  LedgerExamplesTPraos (..),
  ledgerExamplesShelley,
  ledgerExamplesAllegra,
  ledgerExamplesMary,
  ledgerExamplesAlonzo,
  ledgerExamplesBabbage,
  ledgerExamplesConway,
  ledgerExamplesDijkstra,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Test.Cardano.Ledger.Allegra.Examples as Allegra (ledgerExamples)
import qualified Test.Cardano.Ledger.Alonzo.Examples as Alonzo (ledgerExamples)
import qualified Test.Cardano.Ledger.Babbage.Examples as Babbage (ledgerExamples)
import qualified Test.Cardano.Ledger.Conway.Examples as Conway (ledgerExamples)
import qualified Test.Cardano.Ledger.Dijkstra.Examples as Dijkstra (ledgerExamples)
import qualified Test.Cardano.Ledger.Mary.Examples as Mary (ledgerExamples)
import qualified Test.Cardano.Ledger.Shelley.Examples as Shelley (
  LedgerExamples (..),
  ledgerExamples,
 )
import Test.Cardano.Protocol.TPraos.Examples (
  LedgerExamplesTPraos (..),
  ledgerExamplesTPraos,
 )

ledgerExamplesShelley :: LedgerExamplesTPraos ShelleyEra
ledgerExamplesShelley = ledgerExamplesTPraos Shelley.ledgerExamples

ledgerExamplesAllegra :: LedgerExamplesTPraos AllegraEra
ledgerExamplesAllegra = ledgerExamplesTPraos Allegra.ledgerExamples

ledgerExamplesMary :: LedgerExamplesTPraos MaryEra
ledgerExamplesMary = ledgerExamplesTPraos Mary.ledgerExamples

ledgerExamplesAlonzo :: LedgerExamplesTPraos AlonzoEra
ledgerExamplesAlonzo = ledgerExamplesTPraos Alonzo.ledgerExamples

ledgerExamplesBabbage :: LedgerExamplesTPraos BabbageEra
ledgerExamplesBabbage = ledgerExamplesTPraos Babbage.ledgerExamples

ledgerExamplesConway :: LedgerExamplesTPraos ConwayEra
ledgerExamplesConway = ledgerExamplesTPraos Conway.ledgerExamples

ledgerExamplesDijkstra :: LedgerExamplesTPraos DijkstraEra
ledgerExamplesDijkstra = ledgerExamplesTPraos Dijkstra.ledgerExamples
