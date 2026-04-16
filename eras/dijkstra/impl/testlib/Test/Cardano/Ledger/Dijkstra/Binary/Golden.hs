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

import Cardano.Crypto.Hash.Class (Hash (UnsafeHash))
import Cardano.Ledger.Allegra.Scripts (mkTimeExpire, mkTimeStart)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, SupportedLanguage (..))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptBinary)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  Exclusive (..),
  Inclusive (..),
  Network (..),
  StrictMaybe (..),
  Version,
  boundRational,
  knownNonZeroBounded,
  textToUrl,
 )
import Cardano.Ledger.Binary (Annotator, DecoderError (..), DeserialiseFailure (..), Tokens (..))
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  ProposalProcedure (..),
  Vote (..),
  VotingProcedure (..),
 )
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..), Delegatee (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  pattern RequireGuard,
 )
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.Dijkstra.TxCert (DijkstraDelegCert (..), DijkstraTxCert (..))
import qualified Cardano.Ledger.Metadata as Meta
import Cardano.Ledger.Plutus (ExUnits (..), Language (..), SLanguage (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), dataToBinaryData)
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import qualified Data.ByteString.Short as SBS
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Paths_cardano_ledger_dijkstra (getDataFileName)
import PlutusLedgerApi.Common (Data (..))
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds, alwaysSucceedsLang)
import Test.Cardano.Ledger.Binary.Plain.Golden (
  Enc (..),
  goldenPrettyCBOR,
 )
import Test.Cardano.Ledger.Common (Spec, describe, it)
import Test.Cardano.Ledger.Conway.Binary.Golden hiding (spec)
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash, mkKeyPair, mkWitnessVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Dijkstra.Era (DijkstraEraTest)
import Test.Cardano.Ledger.Imp.Common (forEachEraVersion)

spec :: forall era. DijkstraEraTest era => Spec
spec = do
  goldenPrettyCBOREncodings @era
  describe "Golden" . forEachEraVersion @era $ \version -> do
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

goldenKeyHash :: KeyHash kr
goldenKeyHash = KeyHash $ UnsafeHash $ SBS.pack [0 .. 27]

goldenAddr :: Addr
goldenAddr = Addr Testnet (KeyHashObj (mkKeyHash 1)) (StakeRefBase (KeyHashObj (mkKeyHash 2)))

goldenPrettyCBOREncodings :: forall era. DijkstraEraTest era => Spec
goldenPrettyCBOREncodings = describe "Pretty CBOR golden" $ do
  let v = eraProtVerHigh @era
      golden name = getDataFileName ("golden/cbor/" <> name <> ".golden")
  describe "DijkstraGenesis" $
    it "genesis" $ do
      path <- golden "genesis"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( DijkstraGenesis
                  ( UpgradeDijkstraPParams
                      1048576
                      204800
                      (knownNonZeroBounded @25600)
                      (fromJust $ boundRational 1.2)
                  )
              )
          )
          path
  describe "UpgradeDijkstraPParams" $
    it "upgrade-dijkstra-pparams" $ do
      path <- golden "upgrade-dijkstra-pparams"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( UpgradeDijkstraPParams @Identity @era
                  1048576
                  204800
                  (knownNonZeroBounded @25600)
                  (fromJust $ boundRational 1.2)
              )
          )
          path
  describe "TxBody TopTx" $ do
    it "minimal" $ do
      path <- golden "txbody-toptx-minimal"
      pure $ goldenPrettyCBOR (Ev v (mkBasicTxBody @era @TopTx)) path
    it "with-validity-interval" $ do
      path <- golden "txbody-toptx-validity-interval"
      pure $
        goldenPrettyCBOR
          (Ev v (mkBasicTxBody @era @TopTx & vldtTxBodyL .~ ValidityInterval (SJust 50) (SJust 100)))
          path
    it "with-guards" $ do
      path <- golden "txbody-toptx-guards"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              (mkBasicTxBody @era @TopTx & guardsTxBodyL .~ OSet.singleton (KeyHashObj (coerce goldenKeyHash)))
          )
          path
    it "with-subtransactions" $ do
      path <- golden "txbody-toptx-subtransactions"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxBody @era @TopTx
                  & subTransactionsTxBodyL .~ OMap.singleton (mkBasicTx @era @SubTx (mkBasicTxBody @era @SubTx))
              )
          )
          path
    it "with-direct-deposits" $ do
      path <- golden "txbody-toptx-direct-deposits"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxBody @era @TopTx
                  & directDepositsTxBodyL
                    .~ DirectDeposits
                      (Map.singleton (AccountAddress Testnet (AccountId (KeyHashObj goldenKeyHash))) (Coin 1000))
              )
          )
          path
    it "with-account-balance-intervals" $ do
      path <- golden "txbody-toptx-account-balance-intervals"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxBody @era @TopTx
                  & accountBalanceIntervalsTxBodyL
                    .~ AccountBalanceIntervals
                      ( Map.singleton
                          (AccountId (KeyHashObj goldenKeyHash))
                          (AccountBalanceBothBounds (Inclusive (Coin 100)) (Exclusive (Coin 200)))
                      )
              )
          )
          path
  describe "TxBody SubTx" $ do
    it "minimal" $ do
      path <- golden "txbody-subtx-minimal"
      pure $ goldenPrettyCBOR (Ev v (mkBasicTxBody @era @SubTx)) path
    it "with-guards" $ do
      path <- golden "txbody-subtx-guards"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              (mkBasicTxBody @era @SubTx & guardsTxBodyL .~ OSet.singleton (KeyHashObj (coerce goldenKeyHash)))
          )
          path
    it "with-required-top-level-guards" $ do
      path <- golden "txbody-subtx-required-top-level-guards"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxBody @era @SubTx
                  & requiredTopLevelGuardsL .~ Map.singleton (KeyHashObj (coerce goldenKeyHash)) SNothing
              )
          )
          path
    it "with-direct-deposits" $ do
      path <- golden "txbody-subtx-direct-deposits"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxBody @era @SubTx
                  & directDepositsTxBodyL
                    .~ DirectDeposits
                      (Map.singleton (AccountAddress Testnet (AccountId (KeyHashObj goldenKeyHash))) (Coin 1000))
              )
          )
          path
    it "with-account-balance-intervals" $ do
      path <- golden "txbody-subtx-account-balance-intervals"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxBody @era @SubTx
                  & accountBalanceIntervalsTxBodyL
                    .~ AccountBalanceIntervals
                      ( Map.singleton
                          (AccountId (KeyHashObj goldenKeyHash))
                          (AccountBalanceBothBounds (Inclusive (Coin 100)) (Exclusive (Coin 200)))
                      )
              )
          )
          path
  describe "Tx TopTx" $ do
    it "minimal" $ do
      path <- golden "tx-toptx-minimal"
      pure $
        goldenPrettyCBOR
          (Ev v (mkBasicTx @era @TopTx (mkBasicTxBody @era @TopTx)))
          path
    it "with-validity-interval" $ do
      path <- golden "tx-toptx-with-validity-interval"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTx @era @TopTx
                  (mkBasicTxBody @era @TopTx & vldtTxBodyL .~ ValidityInterval (SJust 50) (SJust 100))
              )
          )
          path
  describe "Tx SubTx" $ do
    it "minimal" $ do
      path <- golden "tx-subtx-minimal"
      pure $
        goldenPrettyCBOR
          (Ev v (mkBasicTx @era @SubTx (mkBasicTxBody @era @SubTx)))
          path
    it "with-validity-interval" $ do
      path <- golden "tx-subtx-with-validity-interval"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTx @era @SubTx
                  (mkBasicTxBody @era @SubTx & vldtTxBodyL .~ ValidityInterval (SJust 50) (SJust 100))
              )
          )
          path
  describe "TxOut" $ do
    it "lovelace-only" $ do
      path <- golden "txout-lovelace-only"
      pure $
        goldenPrettyCBOR
          (Ev v (mkBasicTxOut @era goldenAddr (inject (Coin 2500000))))
          path
    it "with-datum-hash" $ do
      path <- golden "txout-with-datum-hash"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxOut @era goldenAddr (inject (Coin 2500000))
                  & dataHashTxOutL .~ SJust (mkDummySafeHash 0)
              )
          )
          path
    it "with-inline-datum" $ do
      path <- golden "txout-with-inline-datum"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxOut @era
                  goldenAddr
                  (inject (Coin 2500000))
                  & datumTxOutL .~ Datum (dataToBinaryData (Data @era (I 99)))
              )
          )
          path
    it "with-script-ref" $ do
      path <- golden "txout-with-script-ref"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( mkBasicTxOut @era
                  goldenAddr
                  (inject (Coin 2500000))
                  & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV3 3)
              )
          )
          path
  describe "TxWits" $
    it "with-native-script" $ do
      path <- golden "txwits-with-native-script"
      let nScript = fromNativeScript @era (RequireSignature goldenKeyHash)
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              (mkBasicTxWits @era & scriptTxWitsL .~ Map.singleton (hashScript @era nScript) nScript)
          )
          path
  describe "TxAuxData" $
    it "with-metadata" $ do
      path <- golden "txauxdata-with-metadata"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              (mkBasicTxAuxData @era & metadataTxAuxDataL .~ Map.singleton 0 (Meta.I 42))
          )
          path
  describe "NativeScript" $ do
    it "require-signature" $ do
      path <- golden "native-script-require-signature"
      pure $ goldenPrettyCBOR (Ev v (RequireSignature @era goldenKeyHash)) path
    it "require-all-of" $ do
      path <- golden "native-script-require-all-of"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( RequireAllOf @era
                  ( SSeq.fromList
                      [RequireSignature goldenKeyHash, mkTimeStart @era 100, mkTimeExpire @era 200]
                  )
              )
          )
          path
    it "require-any-of" $ do
      path <- golden "native-script-require-any-of"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( RequireAnyOf @era
                  ( SSeq.fromList
                      [RequireSignature goldenKeyHash, mkTimeStart @era 50]
                  )
              )
          )
          path
    it "require-m-of-n" $ do
      path <- golden "native-script-require-m-of-n"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( RequireMOf @era
                  2
                  ( SSeq.fromList
                      [ RequireSignature goldenKeyHash
                      , RequireGuard @era (KeyHashObj (coerce goldenKeyHash))
                      , mkTimeStart @era 500
                      ]
                  )
              )
          )
          path
    it "time-start" $ do
      path <- golden "native-script-time-start"
      pure $ goldenPrettyCBOR (Ev v (mkTimeStart @era 100)) path
    it "time-expire" $ do
      path <- golden "native-script-time-expire"
      pure $ goldenPrettyCBOR (Ev v (mkTimeExpire @era 200)) path
    it "require-guard" $ do
      path <- golden "native-script-require-guard"
      pure $
        goldenPrettyCBOR
          (Ev v (RequireGuard @era (KeyHashObj (coerce goldenKeyHash))))
          path
  describe "TxCert" $ do
    it "register" $ do
      path <- golden "txcert-register"
      pure $
        goldenPrettyCBOR
          (Ev v (DijkstraTxCertDeleg @era (DijkstraRegCert (KeyHashObj (mkKeyHash 1)) (Coin 2000000))))
          path
    it "unregister" $ do
      path <- golden "txcert-unregister"
      pure $
        goldenPrettyCBOR
          (Ev v (DijkstraTxCertDeleg @era (DijkstraUnRegCert (KeyHashObj (mkKeyHash 1)) (Coin 2000000))))
          path
    it "delegate-stake" $ do
      path <- golden "txcert-delegate-stake"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              (DijkstraTxCertDeleg @era (DijkstraDelegCert (KeyHashObj (mkKeyHash 1)) (DelegStake (mkKeyHash 3))))
          )
          path
    it "register-delegate" $ do
      path <- golden "txcert-register-delegate"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( DijkstraTxCertDeleg @era
                  (DijkstraRegDelegCert (KeyHashObj (mkKeyHash 1)) (DelegStake (mkKeyHash 3)) (Coin 2000000))
              )
          )
          path
    it "retire-pool" $ do
      path <- golden "txcert-retire-pool"
      pure $
        goldenPrettyCBOR
          (Ev v (DijkstraTxCertPool @era (RetirePool (mkKeyHash 1) (EpochNo 100))))
          path
    it "register-drep" $ do
      path <- golden "txcert-register-drep"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( DijkstraTxCertGov @era
                  (ConwayRegDRep (KeyHashObj (mkKeyHash 5)) (Coin 500000000) SNothing)
              )
          )
          path
    it "auth-committee" $ do
      path <- golden "txcert-auth-committee"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( DijkstraTxCertGov @era
                  (ConwayAuthCommitteeHotKey (KeyHashObj (mkKeyHash 6)) (KeyHashObj (mkKeyHash 7)))
              )
          )
          path
    it "resign-committee" $ do
      path <- golden "txcert-resign-committee"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( DijkstraTxCertGov @era
                  (ConwayResignCommitteeColdKey (KeyHashObj (mkKeyHash 6)) SNothing)
              )
          )
          path
  describe "Redeemers" $
    it "single-spend" $ do
      path <- golden "redeemers-single-spend"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( Redeemers @era
                  (Map.singleton (SpendingPurpose (AsIx 0)) (Data (I 42), ExUnits 500000 200000000))
              )
          )
          path
  describe "Data" $
    it "integer" $ do
      path <- golden "data-integer"
      pure $ goldenPrettyCBOR (Ev v (Data @era (I 42))) path
  describe "VotingProcedure" $
    it "vote-yes" $ do
      path <- golden "voting-procedure-yes"
      pure $ goldenPrettyCBOR (Ev v (VotingProcedure @era VoteYes SNothing)) path
  describe "GovAction" $ do
    it "info-action" $ do
      path <- golden "gov-action-info"
      pure $ goldenPrettyCBOR (Ev v (InfoAction @era)) path
    it "treasury-withdrawals" $ do
      path <- golden "gov-action-treasury-withdrawals"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( TreasuryWithdrawals @era
                  (Map.singleton (AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 1)))) (Coin 1000000))
                  SNothing
              )
          )
          path
  describe "ProposalProcedure" $
    it "info-action" $ do
      path <- golden "proposal-procedure-info"
      pure $
        goldenPrettyCBOR
          ( Ev
              v
              ( ProposalProcedure
                  (Coin 500000000)
                  (AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 1))))
                  (InfoAction @era)
                  (Anchor (fromJust $ textToUrl 64 "https://example.com") (mkDummySafeHash 0))
              )
          )
          path

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
