module Main where

import Test.Cardano.Ledger.Mary.Examples.MultiAssets (multiAssetsExample)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks (timelockTests)
import Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders (codersTest)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL (cddlTests)
import Test.Tasty
import Test.Tasty.HUnit ()

maryChainExamples :: TestTree
maryChainExamples =
  testGroup
    "Mary LEDGER examples"
    [ multiAssetsExample
    ]

tests :: TestTree
tests =
  testGroup
    "Cardano-Legder-Tests"
    [ codersTest,
      txBodyTest,
      timelockTests,
      cddlTests 10,
      maryChainExamples
    ]

-- main entry point
main :: IO ()
main = defaultMain tests
