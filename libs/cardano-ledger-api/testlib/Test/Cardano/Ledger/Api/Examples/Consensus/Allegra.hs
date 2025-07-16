{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Allegra (
  ledgerExamplesAllegra,
  exampleAllegraTxBody,
  exampleTimelock,
  exampleAllegraTxAuxData,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.API (ChainDepState)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import Test.Cardano.Ledger.Allegra.Examples
import Test.Cardano.Ledger.Shelley.Examples (LedgerExamples)
import Test.Cardano.Protocol.TPraos.Examples (
  exampleBlockHeader,
  exampleHashHeader,
  exampleLedgerChainDepState,
 )

ledgerExamplesAllegra ::
  LedgerExamples (BHeader StandardCrypto) HashHeader ChainDepState AllegraEra
ledgerExamplesAllegra =
  ledgerExamples
    (exampleBlockHeader [exampleTxAllegra])
    exampleHashHeader
    (exampleLedgerChainDepState 1)
