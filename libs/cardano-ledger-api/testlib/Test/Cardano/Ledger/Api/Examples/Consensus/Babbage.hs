{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Babbage (
  ledgerExamplesBabbage,
  exampleTxBabbage,
  exampleTxBodyBabbage,
  exampleBabbageNewEpochState,
) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.API (ChainDepState)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import Test.Cardano.Ledger.Babbage.Examples
import Test.Cardano.Ledger.Shelley.Examples (LedgerExamples)
import Test.Cardano.Protocol.TPraos.Examples (
  exampleBlockHeader,
  exampleHashHeader,
  exampleLedgerChainDepState,
 )

ledgerExamplesBabbage ::
  LedgerExamples (BHeader StandardCrypto) HashHeader ChainDepState BabbageEra
ledgerExamplesBabbage =
  ledgerExamples
    (exampleBlockHeader [exampleTxBabbage])
    exampleHashHeader
    (exampleLedgerChainDepState 1)
