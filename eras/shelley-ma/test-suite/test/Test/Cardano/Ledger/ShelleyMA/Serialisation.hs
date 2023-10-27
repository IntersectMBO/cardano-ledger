module Test.Cardano.Ledger.ShelleyMA.Serialisation where

import Test.Cardano.Ledger.Allegra.Translation (allegraEncodeDecodeTests)
import Test.Cardano.Ledger.Mary.Translation (maryEncodeDecodeTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Golden.Encoding (goldenEncodingTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip (allEraRoundtripTests)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks (timelockTests)
import Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest)
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Serialisation tests"
    [ allegraEncodeDecodeTests
    , maryEncodeDecodeTests
    , txBodyTest
    , timelockTests
    , goldenEncodingTests
    , allEraRoundtripTests
    ]
