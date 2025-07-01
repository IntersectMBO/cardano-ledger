{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Conway (
  ledgerExamplesConway,
  collateralOutput,
  exampleConwayCerts,
  exampleTxBodyConway,
  datumExample,
  redeemerExample,
  exampleTx,
  exampleTransactionInBlock,
  exampleConwayNewEpochState,
  exampleConwayGenesis,
) where

import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (TxBody (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Plutus.Data (
  Data (..),
 )
import Cardano.Ledger.Shelley.API (
  NewEpochState (..),
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import qualified Data.OSet.Strict as OSet
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Shelley as SLE

-- | ShelleyLedgerExamples for Conway era
ledgerExamplesConway ::
  SLE.ShelleyLedgerExamples ConwayEra
ledgerExamplesConway = undefined

collateralOutput :: BabbageTxOut ConwayEra
collateralOutput = undefined

exampleConwayCerts :: OSet.OSet (ConwayTxCert era)
exampleConwayCerts = undefined

exampleTxBodyConway :: TxBody ConwayEra
exampleTxBodyConway = undefined

datumExample :: Data ConwayEra
datumExample = undefined

redeemerExample :: Data ConwayEra
redeemerExample = undefined

exampleTx :: ShelleyTx ConwayEra
exampleTx = undefined

exampleTransactionInBlock :: AlonzoTx ConwayEra
exampleTransactionInBlock = undefined

exampleConwayNewEpochState :: NewEpochState ConwayEra
exampleConwayNewEpochState = undefined

exampleConwayGenesis :: ConwayGenesis
exampleConwayGenesis = undefined
