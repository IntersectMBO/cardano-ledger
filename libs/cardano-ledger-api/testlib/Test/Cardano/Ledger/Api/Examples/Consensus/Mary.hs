{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Mary (
  ledgerExamplesMary,
  exampleMultiAssetValue,
  exampleMultiAsset,
) where

import Cardano.Ledger.Mary (MaryEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.API (ChainDepState)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import Test.Cardano.Ledger.Mary.Examples
import Test.Cardano.Ledger.Shelley.Examples (LedgerExamples)
import Test.Cardano.Protocol.TPraos.Examples (
  exampleBlockHeader,
  exampleHashHeader,
  exampleLedgerChainDepState,
 )

ledgerExamplesMary ::
  LedgerExamples (BHeader StandardCrypto) HashHeader ChainDepState MaryEra
ledgerExamplesMary =
  ledgerExamples
    (exampleBlockHeader [exampleTxMary])
    exampleHashHeader
    (exampleLedgerChainDepState 1)
