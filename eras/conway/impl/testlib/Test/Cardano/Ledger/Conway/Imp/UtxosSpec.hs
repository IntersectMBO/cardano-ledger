{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.UtxosSpec (
  spec,
  relevantDuringBootstrapSpec,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
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
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Mary.Value (
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
 )
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
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
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (testingCostModels)
import Test.Cardano.Ledger.Plutus.Examples (alwaysFails2, alwaysSucceeds2, guessTheNumber3)

spec ::
  forall era.
  ( ConwayEraImp era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (ConwayContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "UTXOS" $ do
    relevantDuringBootstrapSpec
    govPolicySpec
    costModelsSpec

relevantDuringBootstrapSpec ::
  forall era.
  ( ConwayEraImp era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (ConwayContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  SpecWith (ImpTestState era)
relevantDuringBootstrapSpec = do
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
        ( pure . injectFailure . BabbageNonDisjointRefInputs $
            mkTxInPartial producingTx 0 :| []
        )
    pure ()
  it "fails when using inline datums for PlutusV1" $ do
    let shSpending = hashPlutusScript (guessTheNumber3 SPlutusV1)
    refTxOut <- mkRefTxOut shSpending
    let producingTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL
              .~ SSeq.fromList
                [ refTxOut
                , scriptLockedTxOut shSpending & dataTxOutL .~ SJust (Data spendDatum)
                ]
    logToExpr producingTx
    producingTxId <- txIdTx <$> submitTxAnn "Producing transaction" producingTx
    let
      lockedTxIn = mkTxInPartial producingTxId 1
      consumingTx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton lockedTxIn
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTxId 0)
    impAnn "Consuming transaction" $
      submitFailingTx
        consumingTx
        ( pure . injectFailure $
            CollectErrors
              [BadTranslation . inject . InlineDatumsNotSupported @era $ TxOutFromInput lockedTxIn]
        )
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
        ( pure . injectFailure . BabbageNonDisjointRefInputs $
            mkTxInPartial producingTx 0 :| []
        )
    pure ()
  it "fails when using inline datums for PlutusV1" $ do
    let shSpending = hashPlutusScript $ guessTheNumber3 SPlutusV1
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
        ( pure . injectFailure $
            CollectErrors
              [BadTranslation . inject . InlineDatumsNotSupported @era $ TxOutFromInput lockedTxIn]
        )

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
            (hashPlutusScript $ guessTheNumber3 SPlutusV1)
            (SJust (Coin 10_000))
            currentTreasuryValueTxBodyL
          $ inject
          $ CurrentTreasuryFieldNotSupported @era
          $ Coin 10_000
        it "V2"
          $ testPlutusV1V2Failure
            (hashPlutusScript $ guessTheNumber3 SPlutusV2)
            (SJust (Coin 10_000))
            currentTreasuryValueTxBodyL
          $ inject
          $ CurrentTreasuryFieldNotSupported @era
          $ Coin 10_000
      describe "VotingProcedures" $ do
        let action = ParameterChange SNothing (def & ppuMinFeeAL .~ SJust (Coin 10)) SNothing
        it "V1" $ do
          (ccCred :| _) <- registerInitialCommittee
          proposal <- submitGovAction action
          let badField =
                VotingProcedures
                  $ Map.singleton
                    (CommitteeVoter ccCred)
                  $ Map.singleton proposal
                  $ VotingProcedure VoteYes SNothing
          testPlutusV1V2Failure
            (hashPlutusScript $ guessTheNumber3 SPlutusV1)
            badField
            votingProceduresTxBodyL
            $ inject
            $ VotingProceduresFieldNotSupported badField
        it "V2" $ do
          (ccCred :| _) <- registerInitialCommittee
          proposal <- submitGovAction action
          let badField =
                VotingProcedures
                  $ Map.singleton
                    (CommitteeVoter ccCred)
                  $ Map.singleton proposal
                  $ VotingProcedure VoteYes SNothing
          testPlutusV1V2Failure
            (hashPlutusScript $ guessTheNumber3 SPlutusV2)
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
            (hashPlutusScript $ guessTheNumber3 SPlutusV1)
            badField
            proposalProceduresTxBodyL
            $ inject
            $ ProposalProceduresFieldNotSupported badField
        it "V2" $ do
          deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
          rewardAccount <- registerRewardAccount
          let badField = OSet.singleton $ ProposalProcedure deposit rewardAccount InfoAction def
          testPlutusV1V2Failure
            (hashPlutusScript $ guessTheNumber3 SPlutusV2)
            badField
            proposalProceduresTxBodyL
            $ inject
            $ ProposalProceduresFieldNotSupported badField
      describe "TreasuryDonation" $ do
        it "V1"
          $ testPlutusV1V2Failure
            (hashPlutusScript $ guessTheNumber3 SPlutusV1)
            (Coin 10_000)
            treasuryDonationTxBodyL
          $ inject
          $ TreasuryDonationFieldNotSupported @era
          $ Coin 10_000
        it "V2"
          $ testPlutusV1V2Failure
            (hashPlutusScript $ guessTheNumber3 SPlutusV2)
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
            testCertificateTranslated regDepositTxCert
              =<< txWithPlutus (hashPlutusScript $ guessTheNumber3 SPlutusV1)
          it "V2" $ do
            stakingC <- KeyHashObj <$> freshKeyHash
            let regDepositTxCert = RegDepositTxCert stakingC (Coin 0)
            testCertificateTranslated regDepositTxCert
              =<< txWithPlutus (hashPlutusScript $ guessTheNumber3 SPlutusV2)
        describe "UnRegDepositTxCert" $ do
          it "V1" $ do
            (_poolKH, _spendingC, stakingC) <- setupPoolWithStake $ Coin 1_000
            let unRegDepositTxCert = UnRegDepositTxCert stakingC (Coin 0)
            testCertificateTranslated unRegDepositTxCert
              =<< txWithPlutus (hashPlutusScript $ guessTheNumber3 SPlutusV1)
          it "V2" $ do
            (_poolKH, _spendingC, stakingC) <- setupPoolWithStake $ Coin 1_000
            let unRegDepositTxCert = UnRegDepositTxCert stakingC (Coin 0)
            testCertificateTranslated unRegDepositTxCert
              =<< txWithPlutus (hashPlutusScript $ guessTheNumber3 SPlutusV2)
      describe "Unsupported" $ do
        let testCertificateNotSupportedV1 badCert =
              testCertificateNotSupported badCert
                =<< txWithPlutus @era (hashPlutusScript $ guessTheNumber3 SPlutusV1)
            testCertificateNotSupportedV2 badCert =
              testCertificateNotSupported badCert
                =<< txWithPlutus @era (hashPlutusScript $ guessTheNumber3 SPlutusV2)
            testCertificateNotSupported badCert tx = do
              submitFailingTx
                ( mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL
                      .~ Set.singleton (txInAt (0 :: Int) tx)
                    & bodyTxL . certsTxBodyL
                      .~ SSeq.singleton badCert
                )
                ( pure . injectFailure $
                    CollectErrors
                      [ BadTranslation $
                          inject $
                            CertificateNotSupported badCert
                      ]
                )
        describe "DelegTxCert" $ do
          it "V1" $ do
            (drep, delegator, _) <- setupSingleDRep 1_000
            let delegTxCert =
                  DelegTxCert @era
                    delegator
                    (DelegVote (DRepCredential drep))
            testCertificateNotSupportedV1 delegTxCert
          it "V2" $ do
            (drep, delegator, _) <- setupSingleDRep 1_000
            let delegTxCert =
                  DelegTxCert @era
                    delegator
                    (DelegVote (DRepCredential drep))
            testCertificateNotSupportedV2 delegTxCert
        describe "RegDepositDelegTxCert" $ do
          it "V1" $ do
            (drep, _, _) <- setupSingleDRep 1_000
            unregisteredDelegatorKH <- freshKeyHash
            let regDepositDelegTxCert =
                  RegDepositDelegTxCert @era
                    (KeyHashObj unregisteredDelegatorKH)
                    (DelegVote (DRepCredential drep))
                    (Coin 0)
            testCertificateNotSupportedV1 regDepositDelegTxCert
          it "V2" $ do
            (drep, _, _) <- setupSingleDRep 1_000
            unregisteredDelegatorKH <- freshKeyHash
            let regDepositDelegTxCert =
                  RegDepositDelegTxCert @era
                    (KeyHashObj unregisteredDelegatorKH)
                    (DelegVote (DRepCredential drep))
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
            (drepKH, _, _) <- setupSingleDRep 1_000
            let unRegDRepTxCert = UnRegDRepTxCert @era drepKH (Coin 0)
            testCertificateNotSupportedV1 unRegDRepTxCert
          it "V1" $ do
            (drepKH, _, _) <- setupSingleDRep 1_000
            let unRegDRepTxCert = UnRegDRepTxCert @era drepKH (Coin 0)
            testCertificateNotSupportedV2 unRegDRepTxCert
        describe "UpdateDRepTxCert" $ do
          it "V1" $ do
            (drepKH, _, _) <- setupSingleDRep 1_000
            let updateDRepTxCert = UpdateDRepTxCert @era drepKH SNothing
            testCertificateNotSupportedV1 updateDRepTxCert
          it "V2" $ do
            (drepKH, _, _) <- setupSingleDRep 1_000
            let updateDRepTxCert = UpdateDRepTxCert @era drepKH SNothing
            testCertificateNotSupportedV2 updateDRepTxCert

govPolicySpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
govPolicySpec = do
  describe "Gov policy scripts" $ do
    it "failing native script govPolicy" $ do
      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      scriptHash <- impAddNativeScript $ RequireTimeStart (SlotNo 1)
      anchor <- arbitrary
      void $
        enactConstitution SNothing (Constitution anchor (SJust scriptHash)) dRep committeeMember
      rewardAccount <- registerRewardAccount
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      impAnn "ParameterChange" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 1
        let govAction = ParameterChange SNothing pparamsUpdate (SJust scriptHash)
        let proposal =
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = govAction
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = anchor
                }
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
                & bodyTxL . vldtTxBodyL .~ ValidityInterval SNothing SNothing
        submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]

      impAnn "TreasuryWithdrawals" $ do
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        let govAction = TreasuryWithdrawals withdrawals (SJust scriptHash)

        let proposal =
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = govAction
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = anchor
                }
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
                & bodyTxL . vldtTxBodyL .~ ValidityInterval SNothing SNothing
        submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]

    it "alwaysSucceeds Plutus govPolicy validates" $ do
      let alwaysSucceedsSh = hashPlutusScript (alwaysSucceeds2 SPlutusV3)
      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      void $
        enactConstitution
          SNothing
          (Constitution anchor (SJust alwaysSucceedsSh))
          dRep
          committeeMember
      rewardAccount <- registerRewardAccount

      impAnn "ParameterChange" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 1
        let govAction = ParameterChange SNothing pparamsUpdate (SJust alwaysSucceedsSh)
        let proposal =
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = govAction
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = anchor
                }
        submitProposal_ proposal
      impAnn "TreasuryWithdrawals" $ do
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        let govAction = TreasuryWithdrawals withdrawals (SJust alwaysSucceedsSh)

        let proposal =
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = govAction
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = anchor
                }
        submitProposal_ proposal

    it "alwaysFails Plutus govPolicy does not validate" $ do
      let alwaysFailsSh = hashPlutusScript (alwaysFails2 SPlutusV3)
      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      void $
        enactConstitution SNothing (Constitution anchor (SJust alwaysFailsSh)) dRep committeeMember

      rewardAccount <- registerRewardAccount
      impAnn "ParameterChange" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 1
        let govAction = ParameterChange SNothing pparamsUpdate (SJust alwaysFailsSh)
        let proposal =
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = govAction
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = anchor
                }
        let tx = mkBasicTx mkBasicTxBody & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
        expectPhase2Invalid tx

      impAnn "TreasuryWithdrawals" $ do
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        let govAction = TreasuryWithdrawals withdrawals (SJust alwaysFailsSh)
        let proposal =
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = govAction
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = anchor
                }
        let tx = mkBasicTx mkBasicTxBody & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
        expectPhase2Invalid tx

costModelsSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  SpecWith (ImpTestState era)
costModelsSpec =
  describe "PlutusV3 Initialization" $ do
    it "Updating CostModels with alwaysFails govPolicy does not validate" $ do
      -- no initial PlutusV3 CostModels
      modifyPParams $ ppCostModelsL .~ testingCostModels [PlutusV1 .. PlutusV2]

      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      govIdConstitution1 <-
        enactConstitution SNothing (Constitution anchor SNothing) dRep committeeMember
      -- propose and enact PlutusV3 Costmodels
      govIdPPUpdate1 <-
        enactCostModels SNothing (testingCostModels [PlutusV3]) dRep committeeMember

      let alwaysFailsSh = hashPlutusScript (alwaysFails2 SPlutusV3)
      void $
        enactConstitution
          (SJust (GovPurposeId govIdConstitution1))
          (Constitution anchor (SJust alwaysFailsSh))
          dRep
          committeeMember

      impAnn "Fail to update V3 Costmodels" $ do
        let pparamsUpdate = def & ppuCostModelsL .~ SJust (testingCostModels [PlutusV3])
        let govAction = ParameterChange (SJust govIdPPUpdate1) pparamsUpdate (SJust alwaysFailsSh)
        rewardAccount <- registerRewardAccount
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        let proposal =
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = govAction
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = anchor
                }
        let tx = mkBasicTx mkBasicTxBody & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
        expectPhase2Invalid tx

    it "Updating CostModels with alwaysSucceeds govPolicy but no PlutusV3 CostModels fails" $ do
      modifyPParams $ ppCostModelsL .~ testingCostModels [PlutusV1 .. PlutusV2]

      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      let alwaysSucceedsSh = hashPlutusScript (alwaysSucceeds2 SPlutusV3)
      void $
        enactConstitution
          SNothing
          (Constitution anchor (SJust alwaysSucceedsSh))
          dRep
          committeeMember

      let pparamsUpdate = def & ppuCostModelsL .~ SJust (testingCostModels [PlutusV3])
      let govAction = ParameterChange SNothing pparamsUpdate (SJust alwaysSucceedsSh)

      submitFailingGovAction govAction [injectFailure $ CollectErrors [NoCostModel PlutusV3]]

    it "Updating CostModels and setting the govPolicy afterwards succeeds" $ do
      modifyPParams $ ppCostModelsL .~ testingCostModels [PlutusV1 .. PlutusV2]

      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      govIdConstitution1 <-
        enactConstitution SNothing (Constitution anchor SNothing) dRep committeeMember

      let guessTheNumberSh = hashPlutusScript (guessTheNumber3 SPlutusV3)

      impAnn "Minting token fails" $ do
        tx <- mintingTokenTx @era (mkBasicTx @era mkBasicTxBody) guessTheNumberSh
        submitFailingTx tx [injectFailure $ CollectErrors [NoCostModel PlutusV3]]

      govIdPPUpdate1 <-
        enactCostModels
          SNothing
          (testingCostModels [PlutusV3])
          dRep
          committeeMember

      let alwaysSucceedsSh = hashPlutusScript (alwaysSucceeds2 SPlutusV3)
      void $
        enactConstitution
          (SJust (GovPurposeId govIdConstitution1))
          (Constitution anchor (SJust alwaysSucceedsSh))
          dRep
          committeeMember

      impAnn "Minting token succeeds" $ do
        tx <- mintingTokenTx @era (mkBasicTx @era mkBasicTxBody) guessTheNumberSh
        submitTx_ tx

      impAnn "Updating CostModels succeeds" $ do
        void $
          enactCostModels
            (SJust govIdPPUpdate1)
            (testingCostModels [PlutusV3])
            dRep
            committeeMember

txWithPlutus ::
  forall era.
  ConwayEraImp era =>
  ScriptHash (EraCrypto era) ->
  ImpTestM era (Tx era)
txWithPlutus sh = do
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

mkRefTxOut ::
  ( BabbageEraTxOut era
  , AlonzoEraImp era
  ) =>
  ScriptHash (EraCrypto era) ->
  ImpTestM era (TxOut era)
mkRefTxOut sh = do
  kpPayment <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  let mbyPlutusScript = impLookupPlutusScriptMaybe sh
  pure $
    mkBasicTxOut (mkAddr (kpPayment, kpStaking)) (inject $ Coin 100)
      & referenceScriptTxOutL .~ maybeToStrictMaybe (fromPlutusScript <$> mbyPlutusScript)

setupRefTx ::
  forall era.
  ( BabbageEraTxOut era
  , AlonzoEraImp era
  ) =>
  ImpTestM era (TxId (EraCrypto era))
setupRefTx = do
  let shSpending = hashPlutusScript (guessTheNumber3 SPlutusV1)
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
  ScriptHash (EraCrypto era) ->
  a ->
  Lens' (TxBody era) a ->
  ContextError era ->
  ImpTestM era ()
testPlutusV1V2Failure sh badField lenz errorField = do
  tx <- txWithPlutus @era sh
  submitFailingTx
    ( mkBasicTx mkBasicTxBody
        & bodyTxL . inputsTxBodyL
          .~ Set.singleton (txInAt (0 :: Int) tx)
        & bodyTxL . lenz
          .~ badField
    )
    ( pure . injectFailure $
        CollectErrors [BadTranslation errorField]
    )

expectPhase2Invalid :: ConwayEraImp era => Tx era -> ImpTestM era ()
expectPhase2Invalid tx = do
  res <- trySubmitTx tx
  -- TODO: find a way to check that this is a PlutusFailure
  -- without comparing the entire PredicateFailure
  void $ expectLeft res
  submitTx_ $ tx & isValidTxL .~ IsValid False

mintingTokenTx :: ConwayEraImp era => Tx era -> ScriptHash (EraCrypto era) -> ImpTestM era (Tx era)
mintingTokenTx tx sh = do
  name <- arbitrary
  count <- choose (0, 10)
  let policyId = PolicyID sh
  let ma = MultiAsset $ Map.singleton policyId [(name, count)]
  (_, addr) <- freshKeyAddr
  pure $
    tx
      & bodyTxL . mintTxBodyL .~ ma
      & bodyTxL . outputsTxBodyL <>~ [mkBasicTxOut addr (MaryValue (Coin 12345) ma)]

enactCostModels ::
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'PParamUpdatePurpose era) ->
  CostModels ->
  Credential 'DRepRole (EraCrypto era) ->
  Credential 'HotCommitteeRole (EraCrypto era) ->
  ImpTestM era (GovPurposeId 'PParamUpdatePurpose era)
enactCostModels prevGovId cms dRep committeeMember = do
  initialCms <- getsNES $ nesEsL . curPParamsEpochStateL . ppCostModelsL
  let pparamsUpdate = def & ppuCostModelsL .~ SJust cms
  govId <- submitParameterChange (unGovPurposeId <$> prevGovId) pparamsUpdate
  submitYesVote_ (DRepVoter dRep) govId
  submitYesVote_ (CommitteeVoter committeeMember) govId
  passNEpochs 2
  enactedCms <- getsNES $ nesEsL . curPParamsEpochStateL . ppCostModelsL
  enactedCms `shouldBe` (initialCms <> cms)
  pure $ GovPurposeId govId

spendDatum :: P1.Data
spendDatum = P1.I 3
