{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Era (
  module Test.Cardano.Ledger.Conway.Era,
  DijkstraEraTest,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..), EraPlutusTxInfo)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Binary (DecCBOR)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraEraScript)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody)
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError)
import Cardano.Ledger.Dijkstra.UTxO (DijkstraEraUTxO)
import Cardano.Ledger.Plutus (Language (..))
import Data.Coerce
import Paths_cardano_ledger_dijkstra (getDataFileName)
import Test.Cardano.Ledger.Common (Arbitrary)
import Test.Cardano.Ledger.Conway.Era
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.Examples (
  exampleDijkstraOnwardsEraPParams,
  exampleDijkstraOnwardsEraPParamsUpdate,
  exampleDijkstraTx,
 )
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

instance EraTest DijkstraEra where
  type
    EraRulesWithFailures DijkstraEra =
      '[ "BBODY"
       , "CERT"
       , "CERTS"
       , "DELEG"
       , "EB"
       , "GOVCERT"
       , "GOV"
       , "LEDGER"
       , "LEDGERS"
       , "LEIOSCERT"
       , "MEMPOOL"
       , "POOL"
       , "UTXO"
       , "UTXOS"
       , "UTXOW"
       , "SUBCERT"
       , "SUBCERTS"
       , "SUBDELEG"
       , "SUBGOVCERT"
       , "SUBGOV"
       , "SUBLEDGER"
       , "SUBLEDGERS"
       , "SUBPOOL"
       , "SUBUTXO"
       , "SUBUTXOW"
       ]

  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV4]

  mkTestAccountState _ptr = mkConwayTestAccountState

  accountsFromAccountsMap = coerce

  mkEraFullPath = getDataFileName

  exampleTx = exampleDijkstraTx

  examplePParams = exampleDijkstraOnwardsEraPParams

  examplePParamsUpdate = exampleDijkstraOnwardsEraPParamsUpdate

class
  ( ConwayEraTest era
  , DijkstraEraPParams era
  , DijkstraEraTxBody era
  , DijkstraEraScript era
  , DijkstraEraUTxO era
  , EraEndorserBlock era
  , EraPlutusTxInfo PlutusV4 era
  , Inject (DijkstraContextError era) (ContextError era)
  , Arbitrary (EbReferences era)
  , DecCBOR (EbReferences era)
  ) =>
  DijkstraEraTest era

instance ShelleyEraTest DijkstraEra

instance AllegraEraTest DijkstraEra

instance MaryEraTest DijkstraEra

instance AlonzoEraTest DijkstraEra

instance BabbageEraTest DijkstraEra

instance ConwayEraTest DijkstraEra

instance DijkstraEraTest DijkstraEra
