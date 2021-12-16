{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LEDGER)
import Cardano.Ledger.ShelleyMA (ShelleyMAEra)
import qualified Cardano.Protocol.TPraos.Rules.Tickn as TPraos
import Test.Cardano.Ledger.Allegra.ScriptTranslation (testScriptPostTranslation)
import Test.Cardano.Ledger.Allegra.Translation (allegraTranslationTests)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.EraBuffet (AllegraEra, MaryEra, TestCrypto)
import Test.Cardano.Ledger.Mary.Examples.MultiAssets (multiAssetsExample)
import Test.Cardano.Ledger.Mary.Golden (goldenScaledMinDeposit)
import Test.Cardano.Ledger.Mary.Translation (maryTranslationTests)
import Test.Cardano.Ledger.Mary.Value (valTests)
import Test.Cardano.Ledger.MaryEraGen ()
import Test.Cardano.Ledger.Shelley.PropertyTests (minimalPropertyTests, propertyTests)
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation as Serialisation
import Test.Tasty
import Test.Tasty.HUnit ()
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

type A = AllegraEra TestCrypto

type AL = LEDGER (AllegraEra TestCrypto)

type M = MaryEra TestCrypto

type ML = LEDGER (MaryEra TestCrypto)

type instance Core.EraRule "TICKN" (ShelleyMAEra _ma _c) = TPraos.TICKN

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
