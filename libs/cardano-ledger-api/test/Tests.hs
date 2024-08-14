{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Conway (Conway)
import qualified Test.Cardano.Ledger.Api.State.Imp.QuerySpec as ImpQuery (spec)
import qualified Test.Cardano.Ledger.Api.State.QuerySpec as StateQuery (spec)
import qualified Test.Cardano.Ledger.Api.Tx as Tx (spec)
import qualified Test.Cardano.Ledger.Api.Tx.Body as TxBody (spec)
import qualified Test.Cardano.Ledger.Api.Tx.Out as TxOut (spec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.ImpTest (withImpStateWithProtVer)

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
      forM_ [natVersion @9, natVersion @10] $ \v ->
        withImpStateWithProtVer @Conway v $ do
          ImpQuery.spec @Conway

main :: IO ()
main = ledgerTestMain apiSpec
