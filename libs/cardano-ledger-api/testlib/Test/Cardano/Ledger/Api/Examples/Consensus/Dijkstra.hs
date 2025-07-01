{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Dijkstra (
  ledgerExamplesDijkstra,
  collateralOutput,
  exampleTxBodyDijkstra,
  datumExample,
  redeemerExample,
  exampleTx,
  exampleTransactionInBlock,
  exampleDijkstraNewEpochState,
  exampleDijkstraGenesis,
) where

import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (TxBody (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
 )
import Cardano.Ledger.Shelley.API (
  NewEpochState (..),
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Shelley as SLE

ledgerExamplesDijkstra ::
  SLE.ShelleyLedgerExamples DijkstraEra
ledgerExamplesDijkstra = undefined

collateralOutput :: BabbageTxOut DijkstraEra
collateralOutput = undefined

exampleTxBodyDijkstra :: TxBody DijkstraEra
exampleTxBodyDijkstra = undefined

datumExample :: Data DijkstraEra
datumExample = undefined

redeemerExample :: Data DijkstraEra
redeemerExample = undefined

exampleTx :: ShelleyTx DijkstraEra
exampleTx = undefined

exampleTransactionInBlock :: AlonzoTx DijkstraEra
exampleTransactionInBlock = undefined

exampleDijkstraNewEpochState :: NewEpochState DijkstraEra
exampleDijkstraNewEpochState = undefined

exampleDijkstraGenesis :: DijkstraGenesis
exampleDijkstraGenesis = undefined
