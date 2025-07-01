{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Babbage (
  ledgerExamplesBabbage,
  collateralOutput,
  exampleTxBodyBabbage,
  datumExample,
  redeemerExample,
  exampleTx,
  exampleTransactionInBlock,
  exampleBabbageNewEpochState,
) where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
 )
import Cardano.Ledger.Shelley.API (
  NewEpochState (..),
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Shelley as SLE

-- | ShelleyLedgerExamples for Babbage era
ledgerExamplesBabbage :: SLE.ShelleyLedgerExamples BabbageEra
ledgerExamplesBabbage = undefined

collateralOutput :: BabbageTxOut BabbageEra
collateralOutput = undefined

exampleTxBodyBabbage :: TxBody BabbageEra
exampleTxBodyBabbage = undefined

datumExample :: Data BabbageEra
datumExample = undefined

redeemerExample :: Data BabbageEra
redeemerExample = undefined

exampleTx :: ShelleyTx BabbageEra
exampleTx = undefined

exampleTransactionInBlock :: AlonzoTx BabbageEra
exampleTransactionInBlock = undefined

exampleBabbageNewEpochState :: NewEpochState BabbageEra
exampleBabbageNewEpochState = undefined
