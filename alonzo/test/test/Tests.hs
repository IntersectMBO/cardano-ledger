module Main where

import Test.Cardano.Ledger.Alonzo.Examples.Utxow (plutusScriptExamples, utxowExamples)
import Test.Cardano.Ledger.Alonzo.Golden as Golden
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import qualified Test.Cardano.Ledger.Alonzo.Translation as Translation
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Alonzo tests"
    [ Tripping.tests,
      Translation.tests,
      CDDL.tests 5,
      Golden.goldenUTxOEntryMinAda,
      plutusScriptExamples,
      utxowExamples
    ]

main :: IO ()
main = defaultMain tests
