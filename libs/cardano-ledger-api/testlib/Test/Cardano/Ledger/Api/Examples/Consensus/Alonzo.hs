{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Alonzo (
  ledgerExamplesAlonzo,
  exampleTxAlonzo,
  exampleTxBodyAlonzo,
  exampleAlonzoNewEpochState,
  exampleDatum,
  exampleRedeemer,
  exampleAlonzoGenesis,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.API (ChainDepState)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import Test.Cardano.Ledger.Alonzo.Examples
import Test.Cardano.Ledger.Shelley.Examples (LedgerExamples)
import Test.Cardano.Protocol.TPraos.Examples (
  exampleBlockHeader,
  exampleHashHeader,
  exampleLedgerChainDepState,
 )

ledgerExamplesAlonzo ::
  LedgerExamples (BHeader StandardCrypto) HashHeader ChainDepState AlonzoEra
ledgerExamplesAlonzo =
  ledgerExamples
    (exampleBlockHeader [exampleTxAlonzo])
    exampleHashHeader
    (exampleLedgerChainDepState 1)
