module Main where

import qualified Test.Cardano.Ledger.Api.Tx.Body as TxBody (spec)
import qualified Test.Cardano.Ledger.Api.Tx.Out as TxOut (spec)
import Test.Cardano.Ledger.Common

-- ====================================================================================

apiSpec :: Spec
apiSpec =
  describe "cardano-ledger-api" $ do
    TxOut.spec
    TxBody.spec

main :: IO ()
main = ledgerTestMain apiSpec
