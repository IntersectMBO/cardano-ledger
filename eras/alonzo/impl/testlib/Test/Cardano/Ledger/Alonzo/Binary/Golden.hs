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
  module Test.Cardano.Ledger.Allegra.Binary.Golden,
) where

import Cardano.Ledger.Alonzo.Core (EraTxWits (..), ShelleyEraTxCert)
import Cardano.Ledger.Binary (
  Annotator,
  DecoderError (..),
  DeserialiseFailure (..),
  Tokens (..),
 )
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Data.Data (Proxy (..))
import Data.Void (Void)
import Test.Cardano.Ledger.Allegra.Binary.Golden hiding (spec)
import Test.Cardano.Ledger.Alonzo.Era (AlonzoEraTest)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (
  Spec,
  describe,
  it,
 )
import Test.Cardano.Ledger.Imp.Common (forEachEraVersion)

witsEmptyField :: Int -> Enc
witsEmptyField k =
  mconcat
    [ E $ TkMapLen 1
    , E k
    , E @[Void] []
    ]

spec ::
  forall era.
  (AlonzoEraTest era, ShelleyEraTxCert era) =>
  Spec
spec = do
  describe "TxWits" $ do
    forEachEraVersion @era $ \version ->
      describe "Empty fields allowed" $ do
        let
          expectSuccessOnEmptyFieldRaw k =
            expectDecoderSuccessAnnWith eqRaw version (witsEmptyField k) (mkBasicTxWits @era)
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
  describe "TxCerts" $ do
    forEachEraVersion @era $ allegraDecodeDuplicateDelegCertSucceeds @era
