{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (Conway)
import qualified Test.Cardano.Ledger.Conway.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Conway.Serialisation.Roundtrip as Roundtrip
import Test.Tasty (TestTree, askOption, testGroup)
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

tests :: TestTree
tests = askOption @TestScenario $ \case
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Conway tests"
    [ Roundtrip.allprops @Conway
    , CDDL.tests 5
    ]

main :: IO ()
main = mainWithTestScenario tests
