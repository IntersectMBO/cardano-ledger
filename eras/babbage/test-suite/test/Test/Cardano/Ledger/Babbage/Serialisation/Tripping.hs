{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -w #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Tripping where

import Cardano.Binary
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.TxBody (TxBody)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Shelley.Metadata (Metadata)
import qualified Cardano.Ledger.Shelley.Tx as LTX
import Cardano.Protocol.TPraos.BHeader (BHeader)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders (roundTrip, roundTrip', roundTripAnn)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

trippingF ::
  (Eq src, Show src, Show target, ToCBOR src) =>
  (src -> Either target (BSL.ByteString, src)) ->
  src ->
  Property
trippingF f x =
  case f x of
    Right (remaining, y)
      | BSL.null remaining ->
        x === y
    Right (remaining, _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
    Left stuff ->
      counterexample
        ( concat
            [ "Failed to decode: ",
              show stuff,
              "\nbytes: ",
              show (Base16.encode (serialize x))
            ]
        )
        False

trippingAnn ::
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR (Annotator t)
  ) =>
  t ->
  Property
trippingAnn x = trippingF roundTripAnn x

tripping :: (Eq src, Show src, ToCBOR src, FromCBOR src) => src -> Property
tripping x = trippingF roundTrip x

tests :: TestTree
tests =
  testGroup
    "Babbage CBOR round-trip"
    [ testProperty "alonzo/TxBody" $
        trippingAnn @(TxBody (BabbageEra C_Crypto))
    ]
