module Test.Shelley.Spec.Ledger.Serialisation where

import qualified Test.Shelley.Spec.Ledger.Serialisation.CDDL
import qualified Test.Shelley.Spec.Ledger.Serialisation.Golden.Address
import qualified Test.Shelley.Spec.Ledger.Serialisation.Golden.Encoding
import qualified Test.Shelley.Spec.Ledger.Serialisation.Golden.Genesis
import qualified Test.Shelley.Spec.Ledger.Serialisation.Tripping.CBOR
import qualified Test.Shelley.Spec.Ledger.Serialisation.Tripping.JSON
import Test.Tasty

tests :: Int -> TestTree
tests cnt =
  testGroup
    "Serialisation tests"
    [ Test.Shelley.Spec.Ledger.Serialisation.Golden.Address.tests,
      Test.Shelley.Spec.Ledger.Serialisation.Golden.Encoding.tests,
      Test.Shelley.Spec.Ledger.Serialisation.Golden.Genesis.tests,
      Test.Shelley.Spec.Ledger.Serialisation.Tripping.CBOR.tests,
      Test.Shelley.Spec.Ledger.Serialisation.Tripping.JSON.tests,
      Test.Shelley.Spec.Ledger.Serialisation.CDDL.tests cnt
    ]
