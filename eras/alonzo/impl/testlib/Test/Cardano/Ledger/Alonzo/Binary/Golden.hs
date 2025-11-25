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
  expectFailureOnTxWitsEmptyField,
  txWitsDecodingFailsOnInvalidField,
  module Test.Cardano.Ledger.Allegra.Binary.Golden,
) where

import Cardano.Ledger.Alonzo.Core (EraTxWits (..), ShelleyEraTxCert)
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
import Test.Cardano.Ledger.Allegra.Binary.Golden hiding (spec)
import Test.Cardano.Ledger.Alonzo.Era (AlonzoEraTest)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (
  Expectation,
  Spec,
  describe,
  it,
  prop,
  (==>),
 )
import Test.Cardano.Ledger.Imp.Common (forEachEraVersion)

witsEmptyField :: Int -> Enc
witsEmptyField k =
  mconcat
    [ E $ TkMapLen 1
    , E k
    , E @[Void] []
    ]

expectFailureOnTxWitsEmptyField ::
  forall era.
  AlonzoEraTest era =>
  Version ->
  Int ->
  DecoderError ->
  Expectation
expectFailureOnTxWitsEmptyField version k =
  expectDecoderFailureAnn @(TxWits era) version (witsEmptyField k)

expectSuccessOnEmptyFieldRaw ::
  forall era.
  AlonzoEraTest era =>
  Version ->
  Int ->
  Expectation
expectSuccessOnEmptyFieldRaw version k =
  expectDecoderSuccessAnnWith eqRaw version (witsEmptyField k) (mkBasicTxWits @era)

txWitsDecodingFailsOnInvalidField :: forall era. AlonzoEraTest era => Version -> [Int] -> Spec
txWitsDecodingFailsOnInvalidField version validFields =
  prop "Invalid field" $ \n ->
    n `notElem` validFields ==> expectFailureOnTxWitsEmptyField @era version n $
      DecoderErrorDeserialiseFailure
        (Binary.label $ Proxy @(Annotator (TxWits era)))
        ( DeserialiseFailure 2 $
            "An error occurred while decoding (Int,Void) not a valid key:.\nError: " <> show n
        )

spec ::
  forall era.
  (AlonzoEraTest era, ShelleyEraTxCert era) =>
  Spec
spec = do
  describe "TxWits" $ do
    forEachEraVersion @era $ \version ->
      describe "Empty fields allowed" $ do
        it "addrTxWits" $ expectSuccessOnEmptyFieldRaw @era version 0
        it "nativeScripts" $ expectSuccessOnEmptyFieldRaw @era version 1
        it "bootstrapWitness" $ expectSuccessOnEmptyFieldRaw @era version 2
        it "plutusV1Script" $ expectSuccessOnEmptyFieldRaw @era version 3
        it "plutusData" $ expectSuccessOnEmptyFieldRaw @era version 4
        it "redeemers" $ expectSuccessOnEmptyFieldRaw @era version 5
        -- Fields 6 and 7 should not deserialize, but they do due to a bug in the Alonzo decoder
        -- This should not be a problem starting with PV9, because we won't allow empty lists
        -- from there onwards
        it "plutusV2Script" $ expectSuccessOnEmptyFieldRaw @era version 6
        it "plutusV3Script" $ expectSuccessOnEmptyFieldRaw @era version 7
  describe "TxCerts" $ do
    forEachEraVersion @era $ allegraDecodeDuplicateDelegCertSucceeds @era
