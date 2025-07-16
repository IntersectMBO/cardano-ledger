{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Dijkstra (
  ledgerExamplesDijkstra,
  exampleTxBodyDijkstra,
  exampleTxDijkstra,
  exampleDijkstraGenesis,
) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.API (ChainDepState)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import Test.Cardano.Ledger.Dijkstra.Examples
import Test.Cardano.Ledger.Shelley.Examples (LedgerExamples)
import Test.Cardano.Protocol.TPraos.Examples (
  exampleBlockHeader,
  exampleHashHeader,
  exampleLedgerChainDepState,
 )

ledgerExamplesDijkstra ::
  LedgerExamples (BHeader StandardCrypto) HashHeader ChainDepState DijkstraEra
ledgerExamplesDijkstra =
  ledgerExamples
    (exampleBlockHeader [exampleTxDijkstra])
    exampleHashHeader
    (exampleLedgerChainDepState 1)
