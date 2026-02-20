{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Era (
  module Test.Cardano.Ledger.Conway.Era,
  DijkstraEraTest,
) where

import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Era (DijkstraEraBlockHeader (..))
import Cardano.Ledger.Dijkstra.Scripts (DijkstraEraScript)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody)
import Cardano.Ledger.Plutus (Language (..))
import Data.Coerce
import Data.Maybe (fromJust)
import Lens.Micro (lens)
import Test.Cardano.Ledger.BlockHeader (TestBlockHeader (..))
import Test.Cardano.Ledger.Conway.Era
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

instance EraTest DijkstraEra where
  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV4]

  mkTestAccountState _ptr = mkConwayTestAccountState

  accountsFromAccountsMap = coerce

class
  ( ConwayEraTest era
  , DijkstraEraTxBody era
  , DijkstraEraScript era
  ) =>
  DijkstraEraTest era

instance ShelleyEraTest DijkstraEra

instance AllegraEraTest DijkstraEra

instance MaryEraTest DijkstraEra

instance AlonzoEraTest DijkstraEra

instance BabbageEraTest DijkstraEra

instance ConwayEraTest DijkstraEra

instance DijkstraEraTest DijkstraEra

instance DijkstraEraBlockHeader TestBlockHeader DijkstraEra where
  blockHeaderPrevNonceL =
    lens
      (fromJust . tbhPrevNonce . blockHeader)
      $ \b@Block {blockHeader} pNonce -> b {blockHeader = blockHeader {tbhPrevNonce = Just pNonce}}
