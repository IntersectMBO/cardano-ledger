{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Test.Cardano.Ledger.Conway.Binary.Golden (
  spec,
  listRedeemersEnc,
  goldenListRedeemers,
  witsEmptyFieldWithSetTag,
  conwayDecodeDuplicateDelegCertFails,
  expectFailureOnTxWitsEmptyFieldWithTag,
  module Test.Cardano.Ledger.Alonzo.Binary.Golden,
) where

import Cardano.Ledger.Alonzo.Core (
  AsIx (..),
  EraTxWits (..),
  TxLevel (..),
  eraProtVerLow,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), unRedeemers)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR,
  DecoderError (..),
  DeserialiseFailure (..),
  Version,
 )
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.Binary.Plain (Tokens (..))
import Cardano.Ledger.Conway.Core (
  EraTxBody (..),
 )
import Cardano.Ledger.Plutus (Data (..))
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import PlutusLedgerApi.Common (Data (..))
import Test.Cardano.Ledger.Alonzo.Binary.Golden hiding (spec)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (
  Expectation,
  Spec,
  ToExpr,
  describe,
  it,
 )
import Test.Cardano.Ledger.Conway.Era (ConwayEraTest)
import Test.Cardano.Ledger.Imp.Common (forEachEraVersion)

-- | A simple redeemer encoded as a list
listRedeemersEnc :: Enc
listRedeemersEnc =
  mconcat
    [ E (TkListLen 1)
    , mconcat
        [ E (TkListLen 4)
        , E (0 :: Int)
        , E (10 :: Int)
        , E (20 :: Int)
        , mconcat
            [ E (TkListLen 2)
            , E (30 :: Int)
            , E (40 :: Int)
            ]
        ]
    ]

goldenListRedeemers :: forall era. ConwayEraTest era => Spec
goldenListRedeemers =
  it "Decoding Redeemers encoded as a list succeeds" $
    expectDecoderResultOn @(Redeemers era)
      (eraProtVerLow @era)
      listRedeemersEnc
      (Redeemers $ Map.singleton (SpendingPurpose $ AsIx 10) (Data $ I 20, ExUnits 30 40))
      unRedeemers

witsEmptyFieldWithSetTag :: Int -> Enc
witsEmptyFieldWithSetTag k =
  mconcat
    [ E $ TkMapLen 1
    , E k
    , E $ TkTag 258
    , E @[Void] []
    ]

expectFailureOnTxWitsEmptyFieldWithTag ::
  forall era.
  ( ToExpr (TxWits era)
  , DecCBOR (Annotator (TxWits era))
  ) =>
  Version -> Int -> DecoderError -> Expectation
expectFailureOnTxWitsEmptyFieldWithTag version k =
  expectDecoderFailureAnn @(TxWits era) version (witsEmptyFieldWithSetTag k)

conwayDecodeDuplicateDelegCertFails ::
  forall era. ConwayEraTest era => Version -> Spec
conwayDecodeDuplicateDelegCertFails version =
  it "Decoding duplicate delegation certs fails" $ do
    expectDecoderFailureAnn @(TxBody TopTx era) version (duplicateDelegCertsTxBody @era version) $
      DecoderErrorDeserialiseFailure
        (Binary.label $ Proxy @(Annotator (TxBody TopTx era)))
        ( DeserialiseFailure
            144
            "Final number of elements: 1 does not match the total count that was decoded: 2"
        )

spec :: forall era. ConwayEraTest era => Spec
spec = do
  describe "TxWits" $
    describe "Empty fields not allowed" $ do
      forEachEraVersion @era $ \version -> do
        describe "Untagged" $ do
          it "addrTxWits" . expectFailureOnTxWitsEmptyField @era version 0 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list found, expected non-empty")
          it "nativeScripts" . expectFailureOnTxWitsEmptyField @era version 1 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list found, expected non-empty")
          it "bootstrapWitness" . expectFailureOnTxWitsEmptyField @era version 2 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list found, expected non-empty")
          it "plutusV1Script" . expectFailureOnTxWitsEmptyField @era version 3 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list of scripts is not allowed")
          it "plutusData" . expectFailureOnTxWitsEmptyField @era version 4 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list found, expected non-empty")
          it "redeemers" . expectFailureOnTxWitsEmptyField @era version 5 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list found, expected non-empty")
          it "plutusV2Script" . expectFailureOnTxWitsEmptyField @era version 6 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list of scripts is not allowed")
          it "plutusV3Script" . expectFailureOnTxWitsEmptyField @era version 7 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 4 "Empty list of scripts is not allowed")
          txWitsDecodingFailsOnInvalidField @era version [0 .. 7]
        describe "Tagged" $ do
          it "addrTxWits" . expectFailureOnTxWitsEmptyField @era version 0 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 7 "Empty list found, expected non-empty")
          it "nativeScripts" . expectFailureOnTxWitsEmptyFieldWithTag @era version 1 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 7 "Empty list found, expected non-empty")
          it "bootstrapWitness" . expectFailureOnTxWitsEmptyFieldWithTag @era version 2 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 7 "Empty list found, expected non-empty")
          it "plutusV1Script" . expectFailureOnTxWitsEmptyFieldWithTag @era version 3 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 7 "Empty list of scripts is not allowed")
          it "plutusData" . expectFailureOnTxWitsEmptyFieldWithTag @era version 4 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 7 "Empty list found, expected non-empty")
          it "plutusV2Script" . expectFailureOnTxWitsEmptyFieldWithTag @era version 6 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 7 "Empty list of scripts is not allowed")
          it "plutusV3Script" . expectFailureOnTxWitsEmptyFieldWithTag @era version 7 $
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure 7 "Empty list of scripts is not allowed")
  describe "TxCerts" . forEachEraVersion @era $ conwayDecodeDuplicateDelegCertFails @era
