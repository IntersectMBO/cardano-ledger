{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Conway (
  ledgerExamplesConway,
  exampleTxConway,
  exampleTxBodyConway,
  exampleConwayCerts,
  exampleConwayGenesis,
) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.API (ChainDepState)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import Test.Cardano.Ledger.Conway.Examples
import Test.Cardano.Ledger.Shelley.Examples (LedgerExamples)
import Test.Cardano.Protocol.TPraos.Examples (
  exampleBlockHeader,
  exampleHashHeader,
  exampleLedgerChainDepState,
 )

ledgerExamplesConway ::
  LedgerExamples (BHeader StandardCrypto) HashHeader ChainDepState ConwayEra
ledgerExamplesConway =
  ledgerExamples
    (exampleBlockHeader [exampleTxConway])
    exampleHashHeader
    (exampleLedgerChainDepState 1)
