{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Alonzo (
  ledgerExamplesAlonzo,
  exampleTxBodyAlonzo,
  datumExample,
  redeemerExample,
  exampleTx,
  exampleTransactionInBlock,
  exampleAlonzoNewEpochState,
  exampleAlonzoGenesis,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Shelley.API (
  NewEpochState (..),
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import qualified PlutusLedgerApi.Common as P
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Shelley as SLE

-- | ShelleyLedgerExamples for Alonzo era
ledgerExamplesAlonzo :: SLE.ShelleyLedgerExamples AlonzoEra
ledgerExamplesAlonzo = undefined

exampleTxBodyAlonzo :: TxBody AlonzoEra
exampleTxBodyAlonzo = undefined

datumExample :: Data AlonzoEra
datumExample = Data (P.I 191)

redeemerExample :: Data AlonzoEra
redeemerExample = Data (P.I 919)

exampleTx :: ShelleyTx AlonzoEra
exampleTx = undefined

exampleTransactionInBlock :: AlonzoTx AlonzoEra
exampleTransactionInBlock = undefined

exampleAlonzoNewEpochState :: NewEpochState AlonzoEra
exampleAlonzoNewEpochState = undefined

exampleAlonzoGenesis :: AlonzoGenesis
exampleAlonzoGenesis = undefined
