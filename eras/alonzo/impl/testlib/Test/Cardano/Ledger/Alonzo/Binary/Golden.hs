{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.Golden (
  spec,
  witsEmptyField,
  module Test.Cardano.Ledger.Shelley.Binary.Golden,
) where

import Cardano.Ledger.Alonzo.Core (EraTxWits (..))
import Cardano.Ledger.Binary (
  Annotator,
  DecoderError (..),
  DeserialiseFailure (..),
  Tokens (..),
  Version,
 )
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Data.Data (Proxy (..))
import Data.Void (Void)
import Test.Cardano.Ledger.Alonzo.Era (AlonzoEraTest)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (
  Spec,
  describe,
  diffExprString,
  expectationFailure,
  it,
 )
import Test.Cardano.Ledger.Shelley.Binary.Golden

witsEmptyField :: Int -> Enc
witsEmptyField k =
  mconcat
    [ E $ TkMapLen 1
    , E k
    , E @[Void] []
    ]

spec ::
  forall era.
  AlonzoEraTest era =>
  Version -> Spec
spec version =
  describe "TxWits" $ do
    describe "Empty fields allowed" $ do
      let
        expectSuccessOnEmptyFieldRaw k =
          case decodeEnc version (witsEmptyField k) of
            Right x
              | x `eqRaw` expected -> pure ()
            decResult -> expectationFailure $ diffExprString decResult (Right expected)
          where
            expected = mkBasicTxWits @era
        expectFailureOnEmptyField k =
          expectDecoderFailureAnn @(TxWits era) version (witsEmptyField k)
      it "addrTxWits" $ expectSuccessOnEmptyFieldRaw 0
      it "nativeScripts" $ expectSuccessOnEmptyFieldRaw 1
      it "bootstrapWitness" $ expectSuccessOnEmptyFieldRaw 2
      it "plutusV1Script" $ expectSuccessOnEmptyFieldRaw 3
      it "plutusData" $ expectSuccessOnEmptyFieldRaw 4
      it "redeemers" $ expectSuccessOnEmptyFieldRaw 5
      -- Fields 6 and 7 should not deserialize, but they do due to a bug in the Alonzo decoder
      -- This should not be a problem starting with PV9, because we won't allow empty lists
      -- from there onwards
      it "plutusV2Script" $ expectSuccessOnEmptyFieldRaw 6
      it "plutusV3Script" $ expectSuccessOnEmptyFieldRaw 7
      it "8th field" . expectFailureOnEmptyField 8 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 2 "An error occured while decoding (Int,Void) not a valid key:.\nError: 8")
