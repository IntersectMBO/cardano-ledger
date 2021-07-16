module Test.Cardano.Ledger.ShelleyMA.Serialisation where

import Test.Cardano.Ledger.Allegra.Translation (allegraEncodeDecodeTests)
import Test.Cardano.Ledger.Mary.Translation (maryEncodeDecodeTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL (cddlTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders (codersTest)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Golden.Encoding (goldenEncodingTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip (allEraRoundtripTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks (timelockTests)
import Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest)
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Serialisation tests"
    [ codersTest,
      allegraEncodeDecodeTests,
      maryEncodeDecodeTests,
      txBodyTest,
      timelockTests,
      cddlTests 10,
      goldenEncodingTests,
      allEraRoundtripTests
    ]
