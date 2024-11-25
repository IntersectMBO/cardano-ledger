{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import qualified Test.Cardano.Ledger.Api.State.Imp.QuerySpec as ImpQuery (spec)
import qualified Test.Cardano.Ledger.Api.State.QuerySpec as StateQuery (spec)
import qualified Test.Cardano.Ledger.Api.Tx as Tx (spec)
import qualified Test.Cardano.Ledger.Api.Tx.Body as TxBody (spec)
import qualified Test.Cardano.Ledger.Api.Tx.Out as TxOut (spec)
import Test.Cardano.Ledger.Imp.Common
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

main :: IO ()
main = ledgerTestMain apiSpec
