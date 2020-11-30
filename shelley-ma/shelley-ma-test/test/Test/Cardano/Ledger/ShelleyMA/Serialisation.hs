module Test.Cardano.Ledger.ShelleyMA.Serialisation where

import Test.Cardano.Ledger.Allegra.Translation (allegraEncodeDecodeTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders (codersTest)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL (cddlTests)
import Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks (timelockTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Golden.Encoding (goldenEncodingTests)
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Serialisation tests"
    [ codersTest,
      allegraEncodeDecodeTests,
      txBodyTest,
      timelockTests,
      cddlTests 10,
      goldenEncodingTests
    ]
