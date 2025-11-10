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
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, SupportedLanguage (..))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptBinary)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (DecoderError (..), DeserialiseFailure (..), Tokens (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core (
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  TxLevel (..),
  eraProtVerLow,
  pattern DelegTxCert,
 )
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceedsLang)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (Spec, describe, it)
import Test.Cardano.Ledger.Conway.Binary.Golden (expectDecoderFailureAnn, listRedeemersEnc)
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash, mkKeyPair, mkWitnessVKey)
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

duplicateCertsTx :: forall era. DijkstraEraTest era => Version -> Enc
duplicateCertsTx v =
  mconcat
    [ E $ TkMapLen 4
    , Em [E @Int 0, Ev v $ Set.empty @TxIn]
    , Em [E @Int 1, Ev v $ [] @(TxOut era)]
    , Em [E @Int 2, E $ Coin 0]
    , Em
        [ E @Int 4
        , Em
            [ E $ TkTag 258
            , E $ TkListLen 2
            , Ev v cert
            , Ev v cert
            ]
        ]
    ]
  where
    cert = DelegTxCert @era (KeyHashObj (mkKeyHash 0)) (DelegStake (mkKeyHash 1))

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
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (AlonzoTxWitsRaw DijkstraEra))"
          ( DeserialiseFailure
              208
              "Final number of elements: 1 does not match the total count that was decoded: 2"
          )
      )

goldenDuplicateNativeScriptsDisallowed :: forall era. DijkstraEraTest era => Spec
goldenDuplicateNativeScriptsDisallowed =
  it "Decoding a TxWits with duplicate native scripts fails" $
    expectDecoderFailureAnn @(TxWits era)
      version
      witsDuplicateNativeScripts
      ( DecoderErrorCustom
          "Annotator"
          "Duplicate elements in the scripts Set were encountered"
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
          "Duplicate elements in the scripts Set were encountered"
      )
