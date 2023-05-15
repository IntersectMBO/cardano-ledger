{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Serialisation.Failure (
  tests,
)
where

import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut)
import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.Binary (
  DecoderError (..),
  DeserialiseFailure (..),
  decodeFull,
  natVersion,
  serialize,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.QuickCheck (Property, counterexample, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- Test that the legacy serialization format for txouts has been deprecated starting
-- at protocol version 9.
-- The legacy serialization format can be produced by generation an AlonzoTxOut.
propConwayTxOutDeprecation :: AlonzoTxOut (ConwayEra StandardCrypto) -> Property
propConwayTxOutDeprecation a =
  let encoded = serialize (natVersion @9) a
      decoded :: Either DecoderError (BabbageTxOut (ConwayEra StandardCrypto))
      decoded = decodeFull (natVersion @9) encoded
   in case decoded of
        Left
          ( DecoderErrorDeserialiseFailure
              x
              (DeserialiseFailure 0 y)
            ) ->
            x === "BabbageTxOut (ConwayEra StandardCrypto)"
              .&&. y === "legacy TxOut serialization was deprecated in version 9"
        Left _ -> counterexample "wrong error" False
        Right _ -> counterexample "should not have serialized" False

tests :: TestTree
tests =
  testGroup
    "Expected serialization failures"
    [ testProperty "TxOut deprecation" propConwayTxOutDeprecation
    ]
