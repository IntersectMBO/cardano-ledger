{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Cardano.Ledger.Allegra ()
import Test.Cardano.Ledger.Allegra.ScriptTranslation (testScriptPostTranslation)
import Test.Cardano.Ledger.Allegra.Translation (allegraTranslationTests)
import Test.Cardano.Ledger.EraBuffet (AllegraEra, MaryEra, TestCrypto)
import Test.Cardano.Ledger.Mary ()
import Test.Cardano.Ledger.Mary.Examples.MultiAssets (multiAssetsExample)
import Test.Cardano.Ledger.Mary.Translation (maryTranslationTests)
import Test.Cardano.Ledger.Mary.Value (valTests)
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation as Serialisation
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Tasty
import Test.Tasty.HUnit ()
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> mainTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "ShelleyMA Ledger Tests"
    [ allegraTests,
      maryTests,
      testGroup
        "Mixed MA Ledger Tests"
        [ Serialisation.tests
        ]
    ]

allegraTests :: TestTree
allegraTests =
  testGroup
    "Allegra Ledger Tests"
    [ allegraTranslationTests,
      minimalPropertyTests @(AllegraEra TestCrypto),
      testScriptPostTranslation
    ]

maryTests :: TestTree
maryTests =
  testGroup
    "Mary Ledger Tests"
    [ maryTranslationTests,
      valTests,
      multiAssetsExample
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "ShelleyMA Ledger - nightly"
    [ testGroup
        "Allegra Ledger - nightly"
        [ propertyTests @(AllegraEra TestCrypto)
        ],
      testGroup
        "Mary Ledger - nightly"
        [ propertyTests @(MaryEra TestCrypto)
        ]
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
