module Test.Cardano.Ledger.Shelley.Serialisation where

import qualified Test.Cardano.Ledger.Shelley.Serialisation.Golden.Address
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Golden.Genesis
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR
import qualified Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Serialisation tests"
    [ Test.Cardano.Ledger.Shelley.Serialisation.Golden.Address.tests
    , Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding.tests
    , Test.Cardano.Ledger.Shelley.Serialisation.Golden.Genesis.tests
    , Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR.tests
    , Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON.tests
    ]
