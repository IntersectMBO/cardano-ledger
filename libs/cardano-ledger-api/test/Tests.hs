{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Mary (MaryEra)
import Data.Default (def)
import Test.Cardano.Ledger.Allegra.Binary.Annotator ()
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import qualified Test.Cardano.Ledger.Api.State.Imp.QuerySpec as ImpQuery (spec)
import qualified Test.Cardano.Ledger.Api.State.QuerySpec as StateQuery (spec)
import qualified Test.Cardano.Ledger.Api.Tx as Tx (spec)
import qualified Test.Cardano.Ledger.Api.Tx.Body as TxBody (spec)
import qualified Test.Cardano.Ledger.Api.Tx.Out as TxOut (spec)
import Test.Cardano.Ledger.Api.Upgrade (BinaryUpgradeOpts (..))
import qualified Test.Cardano.Ledger.Api.Upgrade as Upgrade
import Test.Cardano.Ledger.Babbage.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.Binary.Annotator ()
import Test.Cardano.Ledger.Shelley.ImpTest (LedgerSpec, modifyImpInitProtVer)

-- ====================================================================================

apiSpec :: Spec
apiSpec =
  describe "cardano-ledger-api" $ do
    describe "Tx" $ do
      Tx.spec
      TxOut.spec
      TxBody.spec
    describe "State" $ do
      StateQuery.spec
    describe "Imp" $
      withImpInit @(LedgerSpec ConwayEra) $
        forM_ (eraProtVersions @ConwayEra) $ \v ->
          modifyImpInitProtVer v $ do
            ImpQuery.spec @ConwayEra
    describe "Upgrade" $ do
      Upgrade.spec @AllegraEra def
      Upgrade.spec @MaryEra def
      -- Scripts are not upgradeable from Mary through their CBOR instances, since
      -- Mary had no concept of a prefix.
      -- Transactions are also not upgradeable through deserialisation, though we
      -- check them via the translateEra method
      Upgrade.spec @AlonzoEra (BinaryUpgradeOpts False False)
      Upgrade.spec @BabbageEra def
      Upgrade.spec @ConwayEra def
      Upgrade.spec @DijkstraEra def

main :: IO ()
main = ledgerTestMain apiSpec
