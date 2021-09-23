module Test.Cardano.Ledger.Shelley.Serialisation where

import qualified Test.Cardano.Ledger.Shelley.Serialisation.CDDL
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Golden.Address
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Golden.Genesis
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON
import Test.Tasty

tests :: Int -> TestTree
tests cnt =
  testGroup
    "Serialisation tests"
    [ Test.Cardano.Ledger.Shelley.Serialisation.Golden.Address.tests,
      Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding.tests,
      Test.Cardano.Ledger.Shelley.Serialisation.Golden.Genesis.tests,
      Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR.tests,
      Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON.tests,
      Test.Cardano.Ledger.Shelley.Serialisation.CDDL.tests cnt
    ]
