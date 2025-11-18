{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Golden (
  spec,
  module Test.Cardano.Ledger.Conway.Binary.Golden,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, SupportedLanguage (..))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptBinary)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (Annotator, DecoderError (..), DeserialiseFailure (..), Tokens (..))
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core (
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  TxLevel (..),
  eraProtVerLow,
  eraProtVersions,
 )
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Data (Proxy (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceedsLang)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (Spec, describe, forM_, it)
import Test.Cardano.Ledger.Conway.Binary.Golden hiding (spec)
import Test.Cardano.Ledger.Core.KeyPair (mkKeyPair, mkWitnessVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Dijkstra.Era (DijkstraEraTest)

spec :: forall era. DijkstraEraTest era => Spec
spec = describe "Golden" $ do
  goldenListRedeemersDisallowed @era
  goldenDuplicateCertsDisallowed @era
  describe "TxWits" $ do
    goldenDuplicateVKeyWitsDisallowed @era
    goldenDuplicateNativeScriptsDisallowed @era
    goldenDuplicatePlutusScriptsDisallowed @era SPlutusV1
    goldenDuplicatePlutusScriptsDisallowed @era SPlutusV2
    goldenDuplicatePlutusScriptsDisallowed @era SPlutusV3
    goldenDuplicatePlutusDataDisallowed @era
    goldenSubTransactions @era
    forM_ (eraProtVersions @era) $ goldenEmptyFields @era

goldenEmptyFields :: forall era. DijkstraEraTest era => Version -> Spec
goldenEmptyFields version =
  describe "Empty fields not allowed" $ do
    describe "Untagged" $ do
      let
        expectFailureOnEmptyField k =
          expectDecoderFailureAnn @(TxWits era) version (witsEmptyField k)
      it "addrTxWits" . expectFailureOnEmptyField 0 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 4 "Set cannot be empty")
      it "nativeScripts" . expectFailureOnEmptyField 1 $
        DecoderErrorCustom "Annotator" "Empty script Set is not allowed"
      it "bootstrapWitness" . expectFailureOnEmptyField 2 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 4 "Set cannot be empty")
      it "plutusV1Script" . expectFailureOnEmptyField 3 $
        DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (AlonzoTxWitsRaw DijkstraEra))"
          (DeserialiseFailure 4 "Empty list of scripts is not allowed")
      it "plutusData" . expectFailureOnEmptyField 4 $
        DecoderErrorCustom "Annotator" "Empty script Set is not allowed"
      it "redeemers" . expectFailureOnEmptyField 5 $
        DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (AlonzoTxWitsRaw DijkstraEra))"
          (DeserialiseFailure 2 "List encoding of redeemers not supported starting with PV 12")
      it "plutusV2Script" . expectFailureOnEmptyField 6 $
        DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (AlonzoTxWitsRaw DijkstraEra))"
          (DeserialiseFailure 4 "Empty list of scripts is not allowed")
      it "plutusV3Script" . expectFailureOnEmptyField 7 $
        DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (AlonzoTxWitsRaw DijkstraEra))"
          (DeserialiseFailure 4 "Empty list of scripts is not allowed")
      -- TODO replace this with `plutusV4Script` once that is added
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
          (DeserialiseFailure 7 "Set cannot be empty")
      it "nativeScripts" . expectFailureOnEmptyField 1 $
        DecoderErrorCustom "Annotator" "Empty script Set is not allowed"
      it "bootstrapWitness" . expectFailureOnEmptyField 2 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 7 "Set cannot be empty")
      it "plutusV1Script" . expectFailureOnEmptyField 3 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 7 "Empty list of scripts is not allowed")
      it "plutusData" . expectFailureOnEmptyField 4 $
        DecoderErrorCustom "Annotator" "Empty script Set is not allowed"
      it "plutusV2Script" . expectFailureOnEmptyField 6 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 7 "Empty list of scripts is not allowed")
      it "plutusV3Script" . expectFailureOnEmptyField 7 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 7 "Empty list of scripts is not allowed")
      -- TODO replace this with `plutusV4Script` once that is added
      it "8th field" . expectFailureOnEmptyField 8 $
        DecoderErrorDeserialiseFailure
          (Binary.label $ Proxy @(Annotator (TxWits era)))
          (DeserialiseFailure 2 "An error occured while decoding (Int,Void) not a valid key:.\nError: 8")

witsDuplicateVKeyWits :: Enc
witsDuplicateVKeyWits =
  mconcat
    [ E $ TkMapLen 1
    , E @Int 0
    , Em
        [ E $ TkTag 258
        , E $ TkListLen 2
        , E vkeywit
        , E vkeywit
        ]
    ]
  where
    vkeywit = mkWitnessVKey (mkDummySafeHash 0) (mkKeyPair 0)

witsDuplicateNativeScripts :: Enc
witsDuplicateNativeScripts =
  mconcat
    [ E $ TkMapLen 1
    , E @Int 1
    , Em
        [ E $ TkTag 258
        , E $ TkListLen 2
        , nativeScript
        , nativeScript
        ]
    ]
  where
    nativeScript = Em [E $ TkListLen 2, E @Int 1, E $ TkListLen 0]

witsDuplicatePlutus ::
  forall era l.
  EraPlutusTxInfo l era =>
  SLanguage l -> Enc
witsDuplicatePlutus slang =
  mconcat
    [ E $ TkMapLen 1
    , E @Int $ case slang of
        SPlutusV1 -> 3
        SPlutusV2 -> 6
        SPlutusV3 -> 7
        -- TODO add PlutusV4 support once the CDDL for TxWits is updated to include V4 scripts
        l -> error $ "Unsupported plutus version: " <> show l
    , Em
        [ E $ TkTag 258
        , E $ TkListLen 2
        , plutus
        , plutus
        ]
    ]
  where
    plutus = E . plutusScriptBinary $ alwaysSucceedsLang @era (SupportedLanguage slang) 0

witsDuplicatePlutusData :: Enc
witsDuplicatePlutusData =
  mconcat
    [ E $ TkMapLen 1
    , E @Int 4
    , Em
        [ E $ TkTag 258
        , E $ TkListLen 2
        , dat
        , dat
        ]
    ]
  where
    dat = E @Int 0

goldenListRedeemersDisallowed :: forall era. DijkstraEraTest era => Spec
goldenListRedeemersDisallowed =
  it "Decoding Redeemers encoded as a list fails" $
    expectDecoderFailureAnn @(Redeemers era)
      (eraProtVerLow @era)
      listRedeemersEnc
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (RedeemersRaw DijkstraEra))"
          (DeserialiseFailure 0 "List encoding of redeemers not supported starting with PV 12")
      )

goldenDuplicateCertsDisallowed :: forall era. DijkstraEraTest era => Spec
goldenDuplicateCertsDisallowed =
  it "Decoding a transaction body with duplicate certificates fails" $
    expectDecoderFailureAnn @(TxBody TopTx era)
      version
      (duplicateCertsTx @era version)
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (DijkstraTxBodyRaw TopTx DijkstraEra))"
          ( DeserialiseFailure
              143
              "Final number of elements: 1 does not match the total count that was decoded: 2"
          )
      )
  where
    version = eraProtVerLow @era

goldenDuplicateVKeyWitsDisallowed :: forall era. DijkstraEraTest era => Spec
goldenDuplicateVKeyWitsDisallowed =
  it "Decoding a TxWits with duplicate VKeyWits fails" $
    expectDecoderFailureAnn @(TxWits era)
      (eraProtVerLow @era)
      witsDuplicateVKeyWits
      (DecoderErrorCustom "Annotator" "Duplicates found, expected no duplicates")

goldenDuplicateNativeScriptsDisallowed :: forall era. DijkstraEraTest era => Spec
goldenDuplicateNativeScriptsDisallowed =
  it "Decoding a TxWits with duplicate native scripts fails" $
    expectDecoderFailureAnn @(TxWits era)
      version
      witsDuplicateNativeScripts
      ( DecoderErrorCustom
          "Annotator"
          "Duplicates found, expected no duplicates"
      )
  where
    version = eraProtVerLow @era

goldenDuplicatePlutusScriptsDisallowed ::
  forall era l.
  ( DijkstraEraTest era
  , EraPlutusTxInfo l era
  ) =>
  SLanguage l -> Spec
goldenDuplicatePlutusScriptsDisallowed slang =
  it ("Decoding a TxWits with duplicate " <> show slang <> " scripts fails") $
    expectDecoderFailureAnn @(TxWits era)
      (eraProtVerLow @era)
      (witsDuplicatePlutus @era slang)
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (AlonzoTxWitsRaw DijkstraEra))"
          ( DeserialiseFailure
              22
              "Final number of elements: 1 does not match the total count that was decoded: 2"
          )
      )

goldenDuplicatePlutusDataDisallowed :: forall era. DijkstraEraTest era => Spec
goldenDuplicatePlutusDataDisallowed =
  it "Decoding a TxWits with duplicate plutus data fails" $
    expectDecoderFailureAnn @(TxWits era)
      (eraProtVerLow @era)
      witsDuplicatePlutusData
      ( DecoderErrorCustom
          "Annotator"
          "Duplicates found, expected no duplicates"
      )

goldenSubTransactions :: forall era. DijkstraEraTest era => Spec
goldenSubTransactions = do
  it "TxBody with subtransactions decoded as expected" $
    expectDecoderResultOn @(TxBody TopTx era)
      (eraProtVerLow @era)
      txBodySubTransactionsEnc
      ( mkBasicTxBody @era @TopTx
          & subTransactionsTxBodyL
            .~ OMap.singleton
              (mkBasicTx @era @SubTx (mkBasicTxBody @era @SubTx))
      )
      id
  it "Subtransactions have to be non-empty if the field is present" $
    expectDecoderFailureAnn @(TxBody TopTx era)
      version
      txBodyEmptySubTransactionsEnc
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (DijkstraTxBodyRaw TopTx DijkstraEra))"
          (DeserialiseFailure 12 "Empty list found, expected non-empty")
      )
  it "Subtransactions have to be distinct" $
    expectDecoderFailureAnn @(TxBody TopTx era)
      version
      txBodyDuplicateSubTransactionsEnc
      (DecoderErrorCustom "Annotator" "Duplicates found, expected no duplicates")
  where
    version = eraProtVerLow @era
    txBodyEnc =
      mconcat
        [ E $ TkMapLen 4
        , Em [E @Int 0, Ev version $ Set.empty @TxIn]
        , Em [E @Int 1, Ev version $ [] @(TxOut era)]
        , Em [E @Int 2, E $ Coin 0]
        ]
    txBodySubTransactionsEnc =
      txBodyEnc <> Em [E @Int 23, E (TkListLen 1), subTxEnc]
    txBodyEmptySubTransactionsEnc =
      txBodyEnc <> Em [E @Int 23, E (TkListLen 0)]
    txBodyDuplicateSubTransactionsEnc =
      txBodyEnc <> Em [E @Int 23, E (TkListLen 2), subTxEnc, subTxEnc]
    subTxEnc =
      mconcat
        [ E $ TkListLen 3
        , mconcat
            [ E $ TkMapLen 2
            , Em [E @Int 0, Ev version $ Set.empty @TxIn]
            , Em [E @Int 1, Ev version $ [] @(TxOut era)]
            ]
        , E (TkMapLen 0)
        , E TkNull
        ]
