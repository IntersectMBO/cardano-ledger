{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Ledger (LEDGER)
import Test.Cardano.Ledger.Allegra.ScriptTranslation (testScriptPostTranslation)
import Test.Cardano.Ledger.Allegra.Translation (allegraTranslationTests)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.EraBuffet (AllegraEra, MaryEra, TestCrypto)
import Test.Cardano.Ledger.Mary.Examples.MultiAssets (multiAssetsExample)
import Test.Cardano.Ledger.Mary.Golden (goldenScaledMinDeposit)
import Test.Cardano.Ledger.Mary.Translation (maryTranslationTests)
import Test.Cardano.Ledger.Mary.Value (valTests)
import Test.Cardano.Ledger.MaryEraGen ()
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation as Serialisation
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Tasty
import Test.Tasty.HUnit ()
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

type A = AllegraEra TestCrypto

type AL = LEDGER (AllegraEra TestCrypto)

type M = MaryEra TestCrypto

type ML = LEDGER (MaryEra TestCrypto)

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
      minimalPropertyTests @A @AL,
      testScriptPostTranslation
    ]

maryTests :: TestTree
maryTests =
  testGroup
    "Mary Ledger Tests"
    [ maryTranslationTests,
      valTests,
      multiAssetsExample,
      goldenScaledMinDeposit
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "ShelleyMA Ledger - nightly"
    [ testGroup
        "Allegra Ledger - nightly"
        [ propertyTests @A @AL
        ],
      testGroup
        "Mary Ledger - nightly"
        [ propertyTests @M @ML
        ]
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
