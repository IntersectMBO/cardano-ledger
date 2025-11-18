{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  witsEmptyFieldWithTag,
  module Test.Cardano.Ledger.Alonzo.Binary.Golden,
) where

import Cardano.Ledger.Alonzo.Core (
  AsIx (..),
  EraTxWits (..),
  eraProtVerLow,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), unRedeemers)
import Cardano.Ledger.Binary (Annotator, DecoderError (..), DeserialiseFailure (..), Version)
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.Binary.Plain (Tokens (..))
import Cardano.Ledger.Plutus (Data (..))
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import PlutusLedgerApi.Common (Data (..))
import Test.Cardano.Ledger.Alonzo.Binary.Golden hiding (spec)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (
  Spec,
  describe,
  it,
 )
import Test.Cardano.Ledger.Conway.Era (ConwayEraTest)

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

witsEmptyFieldWithTag :: Int -> Enc
witsEmptyFieldWithTag k =
  mconcat
    [ E $ TkMapLen 1
    , E k
    , E $ TkTag 258
    , E @[Void] []
    ]

spec :: forall era. ConwayEraTest era => Version -> Spec
spec version =
  describe "TxWits" $
    describe "Empty fields not allowed" $ do
      describe "Untagged" $ do
        let
          expectFailureOnEmptyField k =
            expectDecoderFailureAnn @(TxWits era) version (witsEmptyField k)
        it "addrTxWits" . expectFailureOnEmptyField 0 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list found, expected non-empty")
        it "nativeScripts" . expectFailureOnEmptyField 1 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list found, expected non-empty")
        it "bootstrapWitness" . expectFailureOnEmptyField 2 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list found, expected non-empty")
        it "plutusV1Script" . expectFailureOnEmptyField 3 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list of scripts is not allowed")
        it "plutusData" . expectFailureOnEmptyField 4 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list found, expected non-empty")
        it "redeemers" . expectFailureOnEmptyField 5 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list found, expected non-empty")
        it "plutusV2Script" . expectFailureOnEmptyField 6 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list of scripts is not allowed")
        it "plutusV3Script" . expectFailureOnEmptyField 7 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 4 "Empty list of scripts is not allowed")
        it "8th field" . expectFailureOnEmptyField 8 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 2 "An error occured while decoding (Int,Void) not a valid key:.\nError: 8")
      describe "Tagged" $ do
        let
          expectFailureOnEmptyField k =
            expectDecoderFailureAnn @(TxWits era) version (witsEmptyFieldWithTag k)
        it "addrTxWits" . expectFailureOnEmptyField 0 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 7 "Empty list found, expected non-empty")
        it "nativeScripts" . expectFailureOnEmptyField 1 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 7 "Empty list found, expected non-empty")
        it "bootstrapWitness" . expectFailureOnEmptyField 2 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 7 "Empty list found, expected non-empty")
        it "plutusV1Script" . expectFailureOnEmptyField 3 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 7 "Empty list of scripts is not allowed")
        it "plutusData" . expectFailureOnEmptyField 4 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 7 "Empty list found, expected non-empty")
        it "plutusV2Script" . expectFailureOnEmptyField 6 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 7 "Empty list of scripts is not allowed")
        it "plutusV3Script" . expectFailureOnEmptyField 7 $
          DecoderErrorDeserialiseFailure
            (Binary.label $ Proxy @(Annotator (TxWits era)))
            (DeserialiseFailure 7 "Empty list of scripts is not allowed")
