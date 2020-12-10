module Main where

import Test.Cardano.Ledger.Allegra.Translation (allegraTranslationTests)
import Test.Cardano.Ledger.Mary.Examples.MultiAssets (multiAssetsExample)
import Test.Cardano.Ledger.Mary.Translation (maryTranslationTests)
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation as Serialisation
import Test.Cardano.Ledger.Mary.Value (valTests)
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
    [ Serialisation.tests,
      allegraTranslationTests,
      maryTranslationTests,
      maryChainExamples,
      valTests
    ]

-- main entry point
main :: IO ()
main = defaultMain tests
