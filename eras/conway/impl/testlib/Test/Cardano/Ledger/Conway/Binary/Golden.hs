{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
  goldenMapRedeemers,
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
import qualified Cardano.Ledger.Binary.Plain as Plain
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
  expectLeft,
  expectRight,
  expectationFailure,
  it,
  shouldBe,
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

goldenMapRedeemers :: forall era. ConwayEraTest era => Version -> Spec
goldenMapRedeemers version =
  describe "Redeemer maps" $
    mapM_
      ( \(decoderName, decodeRedeemers) ->
          describe decoderName $
            mapM_ (checkEncoding decodeRedeemers) encodings
      )
      decoders
  where
    decoders :: [(String, Enc -> Either DecoderError (Redeemers era))]
    decoders =
      [ ("Annotator", decodeEnc @(Redeemers era) version)
      , ("Plain", Binary.decodeFull @(Redeemers era) version . Plain.serialize)
      ]
    encodings =
      [ ("Definite length", definiteMap, 21, 1)
      , ("Indefinite length", indefiniteMap, 22, 2)
      ]
    definiteMap entries =
      Em [E $ TkMapLen $ fromIntegral $ length entries, Em entries]
    indefiniteMap entries =
      Em [E TkMapBegin, Em entries, E TkBreak]

    entry :: Int -> Int -> Int -> Enc
    entry dat mem steps =
      Em
        [ Em [E $ TkListLen 2, E @Int 0, E @Int 10]
        , Em [E $ TkListLen 2, E dat, Em [E $ TkListLen 2, E mem, E steps]]
        ]
    firstEntry = entry 20 30 40
    lastEntry = entry 21 31 41
    firstExpected = Map.singleton (SpendingPurpose $ AsIx 10) (Data $ I 20, ExUnits 30 40)
    lastExpected = Map.singleton (SpendingPurpose $ AsIx 10) (Data $ I 21, ExUnits 31 41)

    checkEncoding decodeRedeemers (encodingName, encodeMap, duplicateOffset, emptyOffset) =
      describe encodingName $ do
        it "Decoding a single redeemer succeeds" $
          expectSuccess (encodeMap [firstEntry]) firstExpected
        it "Duplicate keys with identical values follow the protocol version" $
          checkDuplicate [firstEntry, firstEntry] firstExpected
        it "Duplicate keys with different values follow the protocol version" $
          checkDuplicate [firstEntry, lastEntry] lastExpected
        it "Decoding an empty redeemers map fails" $
          expectFailure
            (decodeRedeemers $ encodeMap [])
            (DeserialiseFailure emptyOffset "Expected redeemers map to be non-empty")
      where
        expectSuccess enc expected = do
          redeemers <- expectRight $ decodeRedeemers enc
          shouldBe (unRedeemers redeemers) expected
        checkDuplicate entries expected
          | version >= Binary.natVersion @12 =
              expectFailure
                (decodeRedeemers $ encodeMap entries)
                ( DeserialiseFailure
                    duplicateOffset
                    "Final number of elements: 1 does not match the total count that was decoded: 2"
                )
          | otherwise = expectSuccess (encodeMap entries) expected

    expectFailure result expected = do
      err <- expectLeft result
      case err of
        DecoderErrorDeserialiseFailure _ failure -> shouldBe failure expected
        _ -> expectationFailure $ "Expected a deserialisation failure, got: " <> show err

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
  forEachEraVersion @era $ goldenMapRedeemers @era
  describe "TxWits" $
    describe "Empty fields not allowed" $ do
      forEachEraVersion @era $ \version -> do
        let
          decoderFailure n msg =
            DecoderErrorDeserialiseFailure
              (Binary.label $ Proxy @(Annotator (TxWits era)))
              (DeserialiseFailure n msg)
        describe "Untagged" $ do
          it "addrTxWits" . expectFailureOnTxWitsEmptyField @era version 0 $
            decoderFailure 4 "Empty list found, expected non-empty"
          it "nativeScripts" . expectFailureOnTxWitsEmptyField @era version 1 $
            decoderFailure 4 "Empty list found, expected non-empty"
          it "bootstrapWitness" . expectFailureOnTxWitsEmptyField @era version 2 $
            decoderFailure 4 "Empty list found, expected non-empty"
          it "plutusV1Script" . expectFailureOnTxWitsEmptyField @era version 3 $
            decoderFailure 4 "Empty list of scripts is not allowed"
          it "plutusData" . expectFailureOnTxWitsEmptyField @era version 4 $
            decoderFailure 4 "Empty list found, expected non-empty"
          it "redeemers" . expectFailureOnTxWitsEmptyField @era version 5 $
            decoderFailure 4 "Empty list found, expected non-empty"
          it "plutusV2Script" . expectFailureOnTxWitsEmptyField @era version 6 $
            decoderFailure 4 "Empty list of scripts is not allowed"
          it "plutusV3Script" . expectFailureOnTxWitsEmptyField @era version 7 $
            decoderFailure 4 "Empty list of scripts is not allowed"
        describe "Tagged" $ do
          it "addrTxWits" . expectFailureOnTxWitsEmptyFieldWithTag @era version 0 $
            decoderFailure 7 "Empty list found, expected non-empty"
          it "nativeScripts" . expectFailureOnTxWitsEmptyFieldWithTag @era version 1 $
            decoderFailure 7 "Empty list found, expected non-empty"
          it "bootstrapWitness" . expectFailureOnTxWitsEmptyFieldWithTag @era version 2 $
            decoderFailure 7 "Empty list found, expected non-empty"
          it "plutusV1Script" . expectFailureOnTxWitsEmptyFieldWithTag @era version 3 $
            decoderFailure 7 "Empty list of scripts is not allowed"
          it "plutusData" . expectFailureOnTxWitsEmptyFieldWithTag @era version 4 $
            decoderFailure 7 "Empty list found, expected non-empty"
          it "plutusV2Script" . expectFailureOnTxWitsEmptyFieldWithTag @era version 6 $
            decoderFailure 7 "Empty list of scripts is not allowed"
          it "plutusV3Script" . expectFailureOnTxWitsEmptyFieldWithTag @era version 7 $
            decoderFailure 7 "Empty list of scripts is not allowed"
        txWitsDecodingFailsOnInvalidField @era version [0 .. 7]
  describe "TxCerts" . forEachEraVersion @era $ conwayDecodeDuplicateDelegCertFails @era
