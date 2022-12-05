{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Serialisation.Roundtrip (allprops) where

import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Core (Era (..), EraTxOut, Script, TxOut, Value, addrTxOutL, eraProtVerHigh)
import Data.Data (Proxy (..), typeRep)
import Data.List (isInfixOf)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Conway.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators (genNonPtrAddress, genPtrAddress)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary, Property, counterexample, forAll)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

allprops ::
  forall e.
  ( EraTxOut e,
    TxOut e ~ BabbageTxOut e,
    Mock (EraCrypto e),
    Arbitrary (Value e),
    Arbitrary (Script e)
  ) =>
  TestTree
allprops =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "ConwayGenesis" $
        roundTripCborExpectation @(ConwayGenesis (EraCrypto e)) (eraProtVerHigh @e),
      testProperty "conway/TxOut with pointer addresses" (ptrAddrTxOutNotDeserialized @e),
      testProperty "conway/TxOut without pointer addresses" (nonPtrAddrTxOutDeserialized @e)
    ]

ptrAddrTxOutNotDeserialized ::
  forall e.
  EraTxOut e =>
  TxOut e ->
  Property
ptrAddrTxOutNotDeserialized txOut =
  forAll (genPtrAddress @(EraCrypto e)) $ \ptrAddr ->
    do
      let ptrAddrTxOut = txOut & addrTxOutL .~ ptrAddr
      let messageMatch s =
            counterexample ("Unexpected decoder failure message: " ++ s) $
              "Pointer addresses not supported starting with version 9" `isInfixOf` s
      case roundTrip (natVersion @9) (cborTrip @(TxOut e)) ptrAddrTxOut of
        Left (RoundTripFailure _ _ _ _ _ _ (Just e)) -> messageMatch (show e)
        _ -> counterexample "Pointer address deserialization results in RoundTripFailure" False

nonPtrAddrTxOutDeserialized ::
  forall e.
  EraTxOut e =>
  TxOut e ->
  Property
nonPtrAddrTxOutDeserialized txOut =
  forAll (genNonPtrAddress @(EraCrypto e)) $ \nonPtrAddr ->
    do
      let nonPtrAddrTxOut = txOut & addrTxOutL .~ nonPtrAddr
      case roundTrip (natVersion @9) (cborTrip @(TxOut e)) nonPtrAddrTxOut of
        Left err -> counterexample ("Unexpected decoder failure: " ++ show err) False
        Right _ -> counterexample "Non-pointer address deserialize successfuly" True
