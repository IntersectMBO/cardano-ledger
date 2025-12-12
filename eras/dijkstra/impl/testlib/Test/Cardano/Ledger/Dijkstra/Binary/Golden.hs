{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Data (Proxy (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceedsLang)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (Spec, describe, it)
import Test.Cardano.Ledger.Conway.Binary.Golden hiding (spec)
import Test.Cardano.Ledger.Core.KeyPair (mkKeyPair, mkWitnessVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Dijkstra.Era (DijkstraEraTest)
import Test.Cardano.Ledger.Imp.Common (forEachEraVersion)

spec :: forall era. DijkstraEraTest era => Spec
spec = describe "Golden" . forEachEraVersion @era $ \version -> do
  describe "Redeemers" $ do
    goldenListRedeemersDisallowed @era version
  describe "TxCert" $ do
    conwayDecodeDuplicateDelegCertFails @era version
  describe "TxWits" $ do
    goldenDuplicateVKeyWitsDisallowed @era version
    goldenDuplicateNativeScriptsDisallowed @era version
    goldenDuplicatePlutusScriptsDisallowed @era version SPlutusV1
    goldenDuplicatePlutusScriptsDisallowed @era version SPlutusV2
    goldenDuplicatePlutusScriptsDisallowed @era version SPlutusV3
    goldenDuplicatePlutusDataDisallowed @era version
    goldenEmptyFields @era version
  describe "Subtransactions" $ do
    goldenSubTransactions @era
  describe "IsValid flag" $ do
    goldenIsValidFlag @era

goldenEmptyFields :: forall era. DijkstraEraTest era => Version -> Spec
goldenEmptyFields version =
  describe "Empty fields not allowed" $ do
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
        decoderFailure 2 "List encoding of redeemers not supported starting with PV 12"
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

goldenListRedeemersDisallowed :: forall era. DijkstraEraTest era => Version -> Spec
goldenListRedeemersDisallowed version =
  it "Decoding Redeemers encoded as a list fails" $
    expectDecoderFailureAnn @(Redeemers era)
      version
      listRedeemersEnc
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (RedeemersRaw DijkstraEra))"
          (DeserialiseFailure 0 "List encoding of redeemers not supported starting with PV 12")
      )

goldenDuplicateVKeyWitsDisallowed :: forall era. DijkstraEraTest era => Version -> Spec
goldenDuplicateVKeyWitsDisallowed version =
  it "Decoding a TxWits with duplicate VKeyWits fails" $
    expectDecoderFailureAnn @(TxWits era)
      version
      witsDuplicateVKeyWits
      (DecoderErrorCustom "Annotator" "Duplicates found, expected no duplicates")

goldenDuplicateNativeScriptsDisallowed :: forall era. DijkstraEraTest era => Version -> Spec
goldenDuplicateNativeScriptsDisallowed version =
  it "Decoding a TxWits with duplicate native scripts fails" $
    expectDecoderFailureAnn @(TxWits era)
      version
      witsDuplicateNativeScripts
      ( DecoderErrorCustom
          "Annotator"
          "Duplicates found, expected no duplicates"
      )

goldenDuplicatePlutusScriptsDisallowed ::
  forall era l.
  ( DijkstraEraTest era
  , EraPlutusTxInfo l era
  ) =>
  Version -> SLanguage l -> Spec
goldenDuplicatePlutusScriptsDisallowed version slang =
  it ("Decoding a TxWits with duplicate " <> show slang <> " scripts fails") $
    expectDecoderFailureAnn @(TxWits era)
      version
      (witsDuplicatePlutus @era slang)
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (AlonzoTxWitsRaw DijkstraEra))"
          ( DeserialiseFailure
              22
              "Final number of elements: 1 does not match the total count that was decoded: 2"
          )
      )

goldenDuplicatePlutusDataDisallowed :: forall era. DijkstraEraTest era => Version -> Spec
goldenDuplicatePlutusDataDisallowed version =
  it "Decoding a TxWits with duplicate plutus data fails" $
    expectDecoderFailureAnn @(TxWits era)
      version
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

goldenIsValidFlag :: forall era. DijkstraEraTest era => Spec
goldenIsValidFlag = do
  it "Deserialize transactions with missing `isValid` flag" $
    expectDecoderResultOn @(Tx TopTx era)
      version
      txWithoutFlagEnc
      basicValidTx
      id
  it "Deserialize transactions with `isValid` flag set to true" $
    expectDecoderResultOn @(Tx TopTx era)
      version
      txWithFlagTrueEnc
      basicValidTx
      id
  it "Fail to deserialize transactions with `isValid` flag set to false" $
    expectDecoderFailureAnn @(Tx TopTx era)
      version
      txWithFlagFalseEnc
      ( DecoderErrorDeserialiseFailure
          "Annotator (Tx TopTx DijkstraEra)"
          (DeserialiseFailure 13 "value `false` not allowed for `isValid`")
      )
  where
    version = eraProtVerLow @era
    basicValidTx = mkBasicTx @era @TopTx (mkBasicTxBody @era @TopTx) & isValidTxL .~ IsValid True
    txWithoutFlagEnc =
      mconcat
        [ E $ TkListLen 3
        , txBodyEnc
        , E (TkMapLen 0)
        , E TkNull
        ]
    txWithFlagTrueEnc =
      mconcat
        [ E $ TkListLen 4
        , txBodyEnc
        , E (TkMapLen 0)
        , E (TkBool True)
        , E TkNull
        ]
    txWithFlagFalseEnc =
      mconcat
        [ E $ TkListLen 4
        , txBodyEnc
        , E (TkMapLen 0)
        , E (TkBool False)
        , E TkNull
        ]
    txBodyEnc =
      mconcat
        [ E $ TkMapLen 3
        , Em [E @Int 0, Ev version $ Set.empty @TxIn]
        , Em [E @Int 1, Ev version $ [] @(TxOut era)]
        , Em [E @Int 2, E $ Coin 0]
        ]
