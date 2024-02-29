{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.UtxosSpec where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.TxInfo
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (Data (..), TxOutSource (..), hashData)
import Cardano.Ledger.Plutus.Language (Plutus, PlutusLanguage)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Data.Default.Class (def)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.V1 as P1
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (ScriptTestContext (..))
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3, guessTheNumber3V2)

spendDatum :: P1.Data
spendDatum = P1.I 3

setupScriptPlutus ::
  (AlonzoEraScript era, PlutusLanguage p) =>
  Plutus p ->
  ImpTestM era (ScriptHash (EraCrypto era))
setupScriptPlutus script =
  impAddPlutusScript
    ScriptTestContext
      { stcScript = script
      , stcArgs =
          PlutusArgs
            { paSpendDatum = Just spendDatum
            , paData = spendDatum
            }
      }

setupScriptPlutusV1 :: AlonzoEraScript era => ImpTestM era (ScriptHash (EraCrypto era))
setupScriptPlutusV1 = setupScriptPlutus guessTheNumber3

setupScriptPlutusV2 :: AlonzoEraScript era => ImpTestM era (ScriptHash (EraCrypto era))
setupScriptPlutusV2 = setupScriptPlutus guessTheNumber3V2

txWithPlutus ::
  forall era.
  ConwayEraImp era =>
  ImpTestM era (ScriptHash (EraCrypto era)) ->
  ImpTestM era (Tx era)
txWithPlutus setupScript = do
  sh <- setupScript
  submitTxAnn "Submit a Plutus" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.singleton (scriptLockedTxOut sh)

scriptLockedTxOut ::
  forall era.
  AlonzoEraTxOut era =>
  ScriptHash (EraCrypto era) ->
  TxOut era
scriptLockedTxOut shSpending =
  mkBasicTxOut
    (Addr Testnet (ScriptHashObj shSpending) StakeRefNull)
    (inject $ Coin 1_000_000)
    & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)

mkRefTxOut :: BabbageEraTxOut era => ScriptHash (EraCrypto era) -> ImpTestM era (TxOut era)
mkRefTxOut sh = do
  kpPayment <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  mbyPlutusScript <- impLookupPlutusScript sh
  pure $
    mkBasicTxOut (mkAddr (kpPayment, kpStaking)) (inject $ Coin 100)
      & referenceScriptTxOutL .~ maybeToStrictMaybe (fromPlutusScript <$> mbyPlutusScript)

setupRefTx ::
  forall era.
  ( BabbageEraTxOut era
  , ShelleyEraImp era
  ) =>
  ImpTestM era (TxId (EraCrypto era))
setupRefTx = do
  shSpending <- setupScriptPlutusV1
  refTxOut <- mkRefTxOut shSpending
  fmap txIdTx . submitTxAnn "Producing transaction" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.fromList
          [ refTxOut
          , scriptLockedTxOut shSpending
          , scriptLockedTxOut shSpending
          ]

testPlutusV1V2Failure ::
  forall era a.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , HasCallStack
  ) =>
  ImpTestM era (ScriptHash (EraCrypto era)) ->
  a ->
  Lens' (TxBody era) a ->
  ContextError era ->
  ImpTestM era ()
testPlutusV1V2Failure setupScript badField lenz errorField = do
  tx <- txWithPlutus @era setupScript
  submitFailingTx
    ( mkBasicTx mkBasicTxBody
        & bodyTxL . inputsTxBodyL
          .~ Set.singleton (txInAt (0 :: Int) tx)
        & bodyTxL . lenz
          .~ badField
    )
    [injectFailure $ CollectErrors [BadTranslation errorField]]

spec ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , ConwayEraImp era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "UTXOS" $ do
    datumAndReferenceInputsSpec
    conwayFeaturesPlutusV1V2FailureSpec

datumAndReferenceInputsSpec ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , ConwayEraImp era
  ) =>
  SpecWith (ImpTestState era)
datumAndReferenceInputsSpec = do
  it "can use reference scripts" $ do
    producingTx <- setupRefTx
    referringTx <-
      submitTxAnn "Transaction that refers to the script" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 1)
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
    (referringTx ^. witsTxL . scriptTxWitsL) `shouldBe` mempty
  it "can use regular inputs for reference" $ do
    producingTx <- setupRefTx
    referringTx <-
      submitTxAnn "Consuming transaction" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL
            .~ Set.fromList
              [ mkTxInPartial producingTx 0
              , mkTxInPartial producingTx 1
              ]
    (referringTx ^. witsTxL . scriptTxWitsL) `shouldBe` mempty
  it "fails with same txIn in regular inputs and reference inputs" $ do
    producingTx <- setupRefTx
    let
      consumingTx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL
            .~ Set.fromList
              [ mkTxInPartial producingTx 0
              , mkTxInPartial producingTx 1
              ]
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
    _ <-
      submitFailingTx
        consumingTx
        [ injectFailure . BabbageNonDisjointRefInputs $
            mkTxInPartial producingTx 0 :| []
        ]
    pure ()
  it "fails when using inline datums for PlutusV1" $ do
    shSpending <- setupScriptPlutusV1
    refTxOut <- mkRefTxOut shSpending
    producingTx <-
      fmap txIdTx . submitTxAnn "Producing transaction" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL
            .~ SSeq.fromList
              [ refTxOut
              , scriptLockedTxOut shSpending & dataTxOutL .~ SJust (Data spendDatum)
              ]
    let
      lockedTxIn = mkTxInPartial producingTx 1
      consumingTx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton lockedTxIn
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
    impAnn "Consuming transaction" $
      submitFailingTx
        consumingTx
        [ injectFailure $
            CollectErrors
              [BadTranslation . inject . InlineDatumsNotSupported @era $ TxOutFromInput lockedTxIn]
        ]

conwayFeaturesPlutusV1V2FailureSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  SpecWith (ImpTestState era)
conwayFeaturesPlutusV1V2FailureSpec = do
  describe "Conway features fail in Plutusdescribe v1 and v2" $ do
    describe "Unsupported Fields" $ do
      describe "CurrentTreasuryValue" $ do
        it "V1"
          $ testPlutusV1V2Failure
            setupScriptPlutusV1
            (SJust (Coin 10_000))
            currentTreasuryValueTxBodyL
          $ inject
          $ CurrentTreasuryFieldNotSupported @era
          $ Coin 10_000
        it "V2"
          $ testPlutusV1V2Failure
            setupScriptPlutusV2
            (SJust (Coin 10_000))
            currentTreasuryValueTxBodyL
          $ inject
          $ CurrentTreasuryFieldNotSupported @era
          $ Coin 10_000
      describe "VotingProcedures" $ do
        it "V1" $ do
          drepC <- KeyHashObj <$> registerDRep
          cc <- KeyHashObj <$> freshKeyHash
          proposal <-
            submitGovAction $
              UpdateCommittee SNothing mempty (Map.singleton cc $ EpochNo 10) (1 %! 2)
          let badField =
                VotingProcedures
                  $ Map.singleton
                    (DRepVoter drepC)
                  $ Map.singleton proposal
                  $ VotingProcedure VoteYes SNothing
          testPlutusV1V2Failure
            setupScriptPlutusV1
            badField
            votingProceduresTxBodyL
            $ inject
            $ VotingProceduresFieldNotSupported badField
        it "V2" $ do
          drepC <- KeyHashObj <$> registerDRep
          cc <- KeyHashObj <$> freshKeyHash
          proposal <-
            submitGovAction $
              UpdateCommittee SNothing mempty (Map.singleton cc $ EpochNo 10) (1 %! 2)
          let badField =
                VotingProcedures
                  $ Map.singleton
                    (DRepVoter drepC)
                  $ Map.singleton proposal
                  $ VotingProcedure VoteYes SNothing
          testPlutusV1V2Failure
            setupScriptPlutusV2
            badField
            votingProceduresTxBodyL
            $ inject
            $ VotingProceduresFieldNotSupported badField
      describe "ProposalProcedures" $ do
        it "V1" $ do
          deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
          rewardAccount <- registerRewardAccount
          let badField = OSet.singleton $ ProposalProcedure deposit rewardAccount InfoAction def
          testPlutusV1V2Failure
            setupScriptPlutusV1
            badField
            proposalProceduresTxBodyL
            $ inject
            $ ProposalProceduresFieldNotSupported badField
        it "V2" $ do
          deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
          rewardAccount <- registerRewardAccount
          let badField = OSet.singleton $ ProposalProcedure deposit rewardAccount InfoAction def
          testPlutusV1V2Failure
            setupScriptPlutusV2
            badField
            proposalProceduresTxBodyL
            $ inject
            $ ProposalProceduresFieldNotSupported badField
      describe "TreasuryDonation" $ do
        it "V1"
          $ testPlutusV1V2Failure
            setupScriptPlutusV1
            (Coin 10_000)
            treasuryDonationTxBodyL
          $ inject
          $ TreasuryDonationFieldNotSupported @era
          $ Coin 10_000
        it "V2"
          $ testPlutusV1V2Failure
            setupScriptPlutusV2
            (Coin 10_000)
            treasuryDonationTxBodyL
          $ inject
          $ TreasuryDonationFieldNotSupported @era
          $ Coin 10_000
    describe "Certificates" $ do
      describe "Translated" $ do
        let testCertificateTranslated okCert tx = do
              submitTx_
                ( mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL
                      .~ Set.singleton (txInAt (0 :: Int) tx)
                    & bodyTxL . certsTxBodyL
                      .~ SSeq.singleton okCert
                )
        describe "RegDepositTxCert" $ do
          it "V1" $ do
            stakingC <- KeyHashObj <$> freshKeyHash
            let regDepositTxCert = RegDepositTxCert stakingC (Coin 0)
            testCertificateTranslated regDepositTxCert =<< txWithPlutus setupScriptPlutusV1
          it "V2" $ do
            stakingC <- KeyHashObj <$> freshKeyHash
            let regDepositTxCert = RegDepositTxCert stakingC (Coin 0)
            testCertificateTranslated regDepositTxCert =<< txWithPlutus setupScriptPlutusV2
        describe "UnRegDepositTxCert" $ do
          it "V1" $ do
            (_poolKH, _spendingC, stakingC) <- setupPoolWithStake $ Coin 1_000
            let unRegDepositTxCert = UnRegDepositTxCert stakingC (Coin 0)
            testCertificateTranslated unRegDepositTxCert =<< txWithPlutus setupScriptPlutusV1
          it "V2" $ do
            (_poolKH, _spendingC, stakingC) <- setupPoolWithStake $ Coin 1_000
            let unRegDepositTxCert = UnRegDepositTxCert stakingC (Coin 0)
            testCertificateTranslated unRegDepositTxCert =<< txWithPlutus setupScriptPlutusV2
      describe "Unsupported" $ do
        let testCertificateNotSupportedV1 badCert =
              testCertificateNotSupported badCert =<< txWithPlutus @era setupScriptPlutusV1
            testCertificateNotSupportedV2 badCert =
              testCertificateNotSupported badCert =<< txWithPlutus @era setupScriptPlutusV2
            testCertificateNotSupported badCert tx = do
              submitFailingTx
                ( mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL
                      .~ Set.singleton (txInAt (0 :: Int) tx)
                    & bodyTxL . certsTxBodyL
                      .~ SSeq.singleton badCert
                )
                [ injectFailure $
                    CollectErrors
                      [ BadTranslation $
                          inject $
                            CertificateNotSupported badCert
                      ]
                ]
        describe "DelegTxCert" $ do
          it "V1" $ do
            (drepKH, delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            let delegTxCert =
                  DelegTxCert @era
                    (KeyHashObj delegatorKH)
                    (DelegVote (DRepCredential $ KeyHashObj drepKH))
            testCertificateNotSupportedV1 delegTxCert
          it "V2" $ do
            (drepKH, delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            let delegTxCert =
                  DelegTxCert @era
                    (KeyHashObj delegatorKH)
                    (DelegVote (DRepCredential $ KeyHashObj drepKH))
            testCertificateNotSupportedV2 delegTxCert
        describe "RegDepositDelegTxCert" $ do
          it "V1" $ do
            (drepKH, _delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            unregisteredDelegatorKH <- freshKeyHash
            let regDepositDelegTxCert =
                  RegDepositDelegTxCert @era
                    (KeyHashObj unregisteredDelegatorKH)
                    (DelegVote (DRepCredential $ KeyHashObj drepKH))
                    (Coin 0)
            testCertificateNotSupportedV1 regDepositDelegTxCert
          it "V2" $ do
            (drepKH, _delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            unregisteredDelegatorKH <- freshKeyHash
            let regDepositDelegTxCert =
                  RegDepositDelegTxCert @era
                    (KeyHashObj unregisteredDelegatorKH)
                    (DelegVote (DRepCredential $ KeyHashObj drepKH))
                    (Coin 0)
            testCertificateNotSupportedV2 regDepositDelegTxCert
        describe "AuthCommitteeHotKeyTxCert" $ do
          it "V1" $ do
            coldKey <- KeyHashObj <$> freshKeyHash
            hotKey <- KeyHashObj <$> freshKeyHash
            let authCommitteeHotKeyTxCert = AuthCommitteeHotKeyTxCert @era coldKey hotKey
            testCertificateNotSupportedV1 authCommitteeHotKeyTxCert
          it "V2" $ do
            coldKey <- KeyHashObj <$> freshKeyHash
            hotKey <- KeyHashObj <$> freshKeyHash
            let authCommitteeHotKeyTxCert = AuthCommitteeHotKeyTxCert @era coldKey hotKey
            testCertificateNotSupportedV2 authCommitteeHotKeyTxCert
        describe "ResignCommitteeColdTxCert" $ do
          it "V1" $ do
            coldKey <- KeyHashObj <$> freshKeyHash
            let resignCommitteeColdTxCert = ResignCommitteeColdTxCert @era coldKey SNothing
            testCertificateNotSupportedV1 resignCommitteeColdTxCert
          it "V2" $ do
            coldKey <- KeyHashObj <$> freshKeyHash
            let resignCommitteeColdTxCert = ResignCommitteeColdTxCert @era coldKey SNothing
            testCertificateNotSupportedV2 resignCommitteeColdTxCert
        describe "RegDRepTxCert" $ do
          it "V1" $ do
            unregisteredDRepKH <- freshKeyHash
            let regDRepTxCert = RegDRepTxCert @era (KeyHashObj unregisteredDRepKH) (Coin 0) SNothing
            testCertificateNotSupportedV1 regDRepTxCert
          it "V2" $ do
            unregisteredDRepKH <- freshKeyHash
            let regDRepTxCert = RegDRepTxCert @era (KeyHashObj unregisteredDRepKH) (Coin 0) SNothing
            testCertificateNotSupportedV2 regDRepTxCert
        describe "UnRegDRepTxCert" $ do
          it "V1" $ do
            (drepKH, _delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            let unRegDRepTxCert = UnRegDRepTxCert @era (KeyHashObj drepKH) (Coin 0)
            testCertificateNotSupportedV1 unRegDRepTxCert
          it "V1" $ do
            (drepKH, _delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            let unRegDRepTxCert = UnRegDRepTxCert @era (KeyHashObj drepKH) (Coin 0)
            testCertificateNotSupportedV2 unRegDRepTxCert
        describe "UpdateDRepTxCert" $ do
          it "V1" $ do
            (drepKH, _delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            let updateDRepTxCert = UpdateDRepTxCert @era (KeyHashObj drepKH) SNothing
            testCertificateNotSupportedV1 updateDRepTxCert
          it "V2" $ do
            (drepKH, _delegatorKH, _spendingKP) <- setupSingleDRep 1_000
            let updateDRepTxCert = UpdateDRepTxCert @era (KeyHashObj drepKH) SNothing
            testCertificateNotSupportedV2 updateDRepTxCert
