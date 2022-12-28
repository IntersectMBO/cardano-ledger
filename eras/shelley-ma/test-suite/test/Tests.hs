{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Cardano.Ledger.Allegra (Allegra, AllegraEra)
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary (Mary, MaryEra)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Rules (ShelleyLEDGER)
import qualified Cardano.Protocol.TPraos.Rules.Tickn as TPraos
import Test.Cardano.Ledger.Allegra.ScriptTranslation (testScriptPostTranslation)
import Test.Cardano.Ledger.Allegra.Translation (allegraTranslationTests)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Mary.Examples.MultiAssets (multiAssetsExample)
import Test.Cardano.Ledger.Mary.Golden (goldenScaledMinDeposit)
import Test.Cardano.Ledger.Mary.Translation (maryTranslationTests)
import Test.Cardano.Ledger.Mary.Value (valTests)
import Test.Cardano.Ledger.MaryEraGen ()
import Test.Cardano.Ledger.Shelley.PropertyTests (minimalPropertyTests, propertyTests)
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation as Serialisation
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

type instance Core.EraRule "TICKN" (MaryEra _c) = TPraos.TICKN

type instance Core.EraRule "TICKN" (AllegraEra _c) = TPraos.TICKN

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> mainTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "ShelleyMA Ledger Tests"
    [ allegraTests
    , maryTests
    , testGroup
        "Mixed MA Ledger Tests"
        [ Serialisation.tests
        ]
    ]

allegraTests :: TestTree
allegraTests =
  testGroup
    "Allegra Ledger Tests"
    [ allegraTranslationTests
    , minimalPropertyTests @Allegra @(ShelleyLEDGER Allegra)
    , testScriptPostTranslation
    ]

maryTests :: TestTree
maryTests =
  testGroup
    "Mary Ledger Tests"
    [ maryTranslationTests
    , valTests
    , multiAssetsExample
    , goldenScaledMinDeposit
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "ShelleyMA Ledger - nightly"
    [ testGroup
        "Allegra Ledger - nightly"
        [ propertyTests @Allegra @(ShelleyLEDGER Allegra)
        ]
    , testGroup
        "Mary Ledger - nightly"
        [ propertyTests @Mary @(ShelleyLEDGER Mary)
        ]
    ]

main :: IO ()
main = mainWithTestScenario tests
