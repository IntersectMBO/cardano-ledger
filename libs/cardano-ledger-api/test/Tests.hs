module Main where

import Test.Cardano.Ledger.Api.Tx.Out (txOutSpec)
import Test.Cardano.Ledger.Common

-- ====================================================================================

apiSpec :: Spec
apiSpec =
  describe "cardano-ledger-api" txOutSpec

main :: IO ()
main = ledgerTestMain apiSpec
