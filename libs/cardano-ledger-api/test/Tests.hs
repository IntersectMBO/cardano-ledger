module Main where

import System.IO (hSetEncoding, stdout, utf8)
import Test.Cardano.Ledger.Api.Tx.Out (txOutTests)
import Test.Tasty

-- ====================================================================================

apiTests :: TestTree
apiTests =
  testGroup
    "cardano-ledger-api"
    [ txOutTests
    ]

-- main entry point
main :: IO ()
main = do
  hSetEncoding stdout utf8
  defaultMain apiTests
