{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..), AlonzoUtxowPredFailure (..))
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
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.V1 as P1
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (testingCostModels)
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsNoDatum,
  alwaysSucceedsNoDatum,
  evenRedeemerNoDatum,
  redeemerSameAsDatum,
 )

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  govPolicySpec
  costModelsSpec
  datumAndReferenceInputsSpec
  conwayFeaturesPlutusV1V2FailureSpec
  describe "Spending script without a Datum" $ do
    forM_ ([minBound .. eraMaxLanguage @era] :: [Language]) $ \lang -> do
      it (show lang) $ do
        let scriptHash = withSLanguage lang (hashPlutusScript . evenRedeemerNoDatum)
            addr = Addr Testnet (ScriptHashObj scriptHash) StakeRefNull
        amount <- uniformRM (Coin 10_000_000, Coin 100_000_000)
        txIn <- sendCoinTo addr amount
        let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
        if lang >= PlutusV3
          then submitTx_ tx
          else
            submitFailingTx
              tx
              [ injectFailure $ UnspendableUTxONoDatumHash [txIn]
              ]

datumAndReferenceInputsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
datumAndReferenceInputsSpec = do
  it "can use reference scripts" $ do
    producingTx <- setupRefTx SPlutusV1
    referringTx <-
      submitTxAnn "Transaction that refers to the script" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 1)
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
    (referringTx ^. witsTxL . scriptTxWitsL) `shouldBe` mempty
  it "can use regular inputs for reference" $ do
    producingTx <- setupRefTx SPlutusV1
    referringTx <-
      submitTxAnn "Consuming transaction" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL
            .~ Set.fromList
              [ mkTxInPartial producingTx 0
              , mkTxInPartial producingTx 1
              ]
    (referringTx ^. witsTxL . scriptTxWitsL) `shouldBe` mempty
  it "fails with same txIn in regular inputs and reference inputs (PlutusV1)" $ do
    producingTx <- setupRefTx SPlutusV1
    let
      consumingTx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL
            .~ Set.fromList
              [ mkTxInPartial producingTx 0
              , mkTxInPartial producingTx 1
              ]
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
    let badTxIns = mkTxInPartial producingTx 0 :| []

    whenMajorVersionAtMost @10 $
      submitFailingTx
        consumingTx
        (pure . injectFailure $ BabbageNonDisjointRefInputs badTxIns)

    -- consumingTx uses PlutusV1 script, so it should be a success
    whenMajorVersionAtLeast @11 $
      submitTx_ consumingTx
  it "fails when using inline datums for PlutusV1" $ do
    let shSpending = hashPlutusScript $ redeemerSameAsDatum SPlutusV1
    refTxOut <- mkRefTxOut shSpending
    producingTxId <-
      fmap txIdTx . submitTxAnn "Producing transaction" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL
            .~ SSeq.fromList
              [ refTxOut
              , scriptLockedTxOut shSpending & dataTxOutL .~ SJust (Data spendDatum)
              ]
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
  it "fails with same txIn in regular inputs and reference inputs (PlutusV3)" $ do
    producingTx <- setupRefTx SPlutusV3
    let
      consumingTx =
        mkBasicTx @era mkBasicTxBody
          & bodyTxL . inputsTxBodyL
            .~ Set.fromList
              [ mkTxInPartial producingTx 0
              , mkTxInPartial producingTx 1
              ]
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
    let badTxIns = mkTxInPartial producingTx 0 :| []
    whenMajorVersionAtMost @10 $
      submitFailingTx @era
        consumingTx
        (pure . injectFailure $ BabbageNonDisjointRefInputs badTxIns)
    whenMajorVersionAtLeast @11 $
      submitFailingTx @era
        consumingTx
        [ injectFailure $
            CollectErrors [BadTranslation . inject $ ReferenceInputsNotDisjointFromInputs @era badTxIns]
        ]
  it "succeeds when using inline datums for PlutusV2" $ do
    let shSpending = hashPlutusScript $ redeemerSameAsDatum SPlutusV2
    refTxOut <- mkRefTxOut shSpending
    producingTxId <-
      fmap txIdTx . submitTxAnn "Producing transaction" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL
            .~ SSeq.fromList
              [ refTxOut
              , scriptLockedTxOut shSpending & dataTxOutL .~ SJust (Data spendDatum)
              ]
    let
      lockedTxIn = mkTxInPartial producingTxId 1
      consumingTx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton lockedTxIn
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTxId 0)
    impAnn "Consuming transaction" $
      submitTx_
        consumingTx

conwayFeaturesPlutusV1V2FailureSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
conwayFeaturesPlutusV1V2FailureSpec = do
  describe "Conway features fail in Plutusdescribe v1 and v2" $ do
    describe "Unsupported Fields" $ do
      describe "CurrentTreasuryValue" $ do
        it "V1" $ do
          donation <- arbitrary
          submitTx_ $ mkBasicTx (mkBasicTxBody & treasuryDonationTxBodyL .~ donation)
          passEpoch
          testPlutusV1V2Failure
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV1)
            (SJust donation)
            currentTreasuryValueTxBodyL
            $ inject (CurrentTreasuryFieldNotSupported @era donation)
        it "V2" $ do
          donation <- arbitrary
          submitTx_ $ mkBasicTx (mkBasicTxBody & treasuryDonationTxBodyL .~ donation)
          passEpoch
          testPlutusV1V2Failure
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV2)
            (SJust donation)
            currentTreasuryValueTxBodyL
            $ inject (CurrentTreasuryFieldNotSupported @era donation)
      describe "VotingProcedures" $ do
        it "V1" $ do
          action <- mkMinFeeUpdateGovAction SNothing
          (ccCred :| _) <- registerInitialCommittee
          proposal <- submitGovAction action
          let badField =
                VotingProcedures
                  $ Map.singleton
                    (CommitteeVoter ccCred)
                  $ Map.singleton proposal
                  $ VotingProcedure VoteYes SNothing
          testPlutusV1V2Failure
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV1)
            badField
            votingProceduresTxBodyL
            $ inject
            $ VotingProceduresFieldNotSupported badField
        it "V2" $ do
          action <- mkMinFeeUpdateGovAction SNothing
          (ccCred :| _) <- registerInitialCommittee
          proposal <- submitGovAction action
          let badField =
                VotingProcedures
                  $ Map.singleton
                    (CommitteeVoter ccCred)
                  $ Map.singleton proposal
                  $ VotingProcedure VoteYes SNothing
          testPlutusV1V2Failure
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV2)
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
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV1)
            badField
            proposalProceduresTxBodyL
            $ inject
            $ ProposalProceduresFieldNotSupported badField
        it "V2" $ do
          deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
          rewardAccount <- registerRewardAccount
          let badField = OSet.singleton $ ProposalProcedure deposit rewardAccount InfoAction def
          testPlutusV1V2Failure
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV2)
            badField
            proposalProceduresTxBodyL
            $ inject
            $ ProposalProceduresFieldNotSupported badField
      describe "TreasuryDonation" $ do
        it "V1"
          $ testPlutusV1V2Failure
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV1)
            (Coin 10_000)
            treasuryDonationTxBodyL
          $ inject
          $ TreasuryDonationFieldNotSupported @era
          $ Coin 10_000
        it "V2"
          $ testPlutusV1V2Failure
            (hashPlutusScript $ redeemerSameAsDatum SPlutusV2)
            (Coin 10_000)
            treasuryDonationTxBodyL
          $ inject
          $ TreasuryDonationFieldNotSupported @era
          $ Coin 10_000
    describe "Certificates" $ do
      describe "Translated" $ do
        let testCertificateTranslated okCert txIn = do
              submitTx_
                ( mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL
                      .~ Set.singleton txIn
                    & bodyTxL . certsTxBodyL
                      .~ SSeq.singleton okCert
                )
        describe "RegDepositTxCert" $ do
          it "V1" $ do
            stakingC <- KeyHashObj <$> freshKeyHash
            deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
            let regDepositTxCert = RegDepositTxCert stakingC deposit
            testCertificateTranslated regDepositTxCert
              =<< produceScript (hashPlutusScript $ redeemerSameAsDatum SPlutusV1)
          it "V2" $ do
            stakingC <- KeyHashObj <$> freshKeyHash
            deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
            let regDepositTxCert = RegDepositTxCert stakingC deposit
            testCertificateTranslated regDepositTxCert
              =<< produceScript (hashPlutusScript $ redeemerSameAsDatum SPlutusV2)
        describe "UnRegDepositTxCert" $ do
          it "V1" $ do
            (_poolKH, _spendingC, stakingC) <- setupPoolWithStake $ Coin 1_000_000_000
            deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
            let unRegDepositTxCert = UnRegDepositTxCert stakingC deposit
            testCertificateTranslated unRegDepositTxCert
              =<< produceScript (hashPlutusScript $ redeemerSameAsDatum SPlutusV1)
          it "V2" $ do
            (_poolKH, _spendingC, stakingC) <- setupPoolWithStake $ Coin 1_000_000_000
            deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
            let unRegDepositTxCert = UnRegDepositTxCert stakingC deposit
            testCertificateTranslated unRegDepositTxCert
              =<< produceScript (hashPlutusScript $ redeemerSameAsDatum SPlutusV2)
      describe "Unsupported" $ do
        let testCertificateNotSupportedV1 badCert =
              testCertificateNotSupported badCert
                =<< produceScript @era (hashPlutusScript $ redeemerSameAsDatum SPlutusV1)
            testCertificateNotSupportedV2 badCert =
              testCertificateNotSupported badCert
                =<< produceScript @era (hashPlutusScript $ redeemerSameAsDatum SPlutusV2)
            testCertificateNotSupported badCert txIn = do
              submitFailingTx
                ( mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL
                      .~ Set.singleton txIn
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
            (drep, delegator, _) <- setupSingleDRep 1_000_000_000
            let delegTxCert =
                  DelegTxCert @era
                    delegator
                    (DelegVote (DRepCredential drep))
            testCertificateNotSupportedV1 delegTxCert
          it "V2" $ do
            (drep, delegator, _) <- setupSingleDRep 1_000_000_000
            let delegTxCert =
                  DelegTxCert @era
                    delegator
                    (DelegVote (DRepCredential drep))
            testCertificateNotSupportedV2 delegTxCert
        describe "RegDepositDelegTxCert" $ do
          it "V1" $ do
            (drep, _, _) <- setupSingleDRep 1_000_000_000
            unregisteredDelegatorKH <- freshKeyHash
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            let regDepositDelegTxCert =
                  RegDepositDelegTxCert @era
                    (KeyHashObj unregisteredDelegatorKH)
                    (DelegVote (DRepCredential drep))
                    (pp ^. ppKeyDepositL)
            testCertificateNotSupportedV1 regDepositDelegTxCert
          it "V2" $ do
            (drep, _, _) <- setupSingleDRep 1_000_000_000
            unregisteredDelegatorKH <- freshKeyHash
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            let regDepositDelegTxCert =
                  RegDepositDelegTxCert @era
                    (KeyHashObj unregisteredDelegatorKH)
                    (DelegVote (DRepCredential drep))
                    (pp ^. ppKeyDepositL)
            testCertificateNotSupportedV2 regDepositDelegTxCert
        describe "AuthCommitteeHotKeyTxCert" $ do
          it "V1" $ do
            coldKey <- elements . Set.toList =<< getCommitteeMembers
            hotKey <- KeyHashObj <$> freshKeyHash
            let authCommitteeHotKeyTxCert = AuthCommitteeHotKeyTxCert @era coldKey hotKey
            testCertificateNotSupportedV1 authCommitteeHotKeyTxCert
          it "V2" $ do
            coldKey <- elements . Set.toList =<< getCommitteeMembers
            hotKey <- KeyHashObj <$> freshKeyHash
            let authCommitteeHotKeyTxCert = AuthCommitteeHotKeyTxCert @era coldKey hotKey
            testCertificateNotSupportedV2 authCommitteeHotKeyTxCert
        describe "ResignCommitteeColdTxCert" $ do
          it "V1" $ do
            coldKey <- elements . Set.toList =<< getCommitteeMembers
            let resignCommitteeColdTxCert = ResignCommitteeColdTxCert @era coldKey SNothing
            testCertificateNotSupportedV1 resignCommitteeColdTxCert
          it "V2" $ do
            coldKey <- elements . Set.toList =<< getCommitteeMembers
            let resignCommitteeColdTxCert = ResignCommitteeColdTxCert @era coldKey SNothing
            testCertificateNotSupportedV2 resignCommitteeColdTxCert
        describe "RegDRepTxCert" $ do
          it "V1" $ do
            unregisteredDRepKH <- freshKeyHash
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            testCertificateNotSupportedV1 $
              RegDRepTxCert @era (KeyHashObj unregisteredDRepKH) (pp ^. ppDRepDepositL) SNothing
          it "V2" $ do
            unregisteredDRepKH <- freshKeyHash
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            testCertificateNotSupportedV2 $
              RegDRepTxCert @era (KeyHashObj unregisteredDRepKH) (pp ^. ppDRepDepositL) SNothing
        describe "UnRegDRepTxCert" $ do
          it "V1" $ do
            (drepKH, _, _) <- setupSingleDRep 1_000_000_000
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            let unRegDRepTxCert = UnRegDRepTxCert @era drepKH (pp ^. ppDRepDepositL)
            testCertificateNotSupportedV1 unRegDRepTxCert
          it "V1" $ do
            (drepKH, _, _) <- setupSingleDRep 1_000_000_000
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            let unRegDRepTxCert = UnRegDRepTxCert @era drepKH (pp ^. ppDRepDepositL)
            testCertificateNotSupportedV2 unRegDRepTxCert
        describe "UpdateDRepTxCert" $ do
          it "V1" $ do
            (drepKH, _, _) <- setupSingleDRep 1_000_000_000
            let updateDRepTxCert = UpdateDRepTxCert @era drepKH SNothing
            testCertificateNotSupportedV1 updateDRepTxCert
          it "V2" $ do
            (drepKH, _, _) <- setupSingleDRep 1_000_000_000
            let updateDRepTxCert = UpdateDRepTxCert @era drepKH SNothing
            testCertificateNotSupportedV2 updateDRepTxCert

govPolicySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
govPolicySpec = do
  describe "Gov policy scripts" $ do
    -- These tests rely on the script in the constitution, but we can only change the constitution after bootstrap.
    -- So we cannot run these tests during bootstrap
    it "failing native script govPolicy" $ whenPostBootstrap $ do
      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      scriptHash <- impAddNativeScript $ RequireTimeStart (SlotNo 1)
      anchor <- arbitrary
      void $
        enactConstitution SNothing (Constitution anchor (SJust scriptHash)) dRep committeeMembers'
      impAnn "ParameterChange" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 1
        let govAction = ParameterChange SNothing pparamsUpdate (SJust scriptHash)
        proposal <- mkProposal govAction
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
                & bodyTxL . vldtTxBodyL .~ ValidityInterval SNothing SNothing
        submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]

      impAnn "TreasuryWithdrawals" $ do
        rewardAccount <- registerRewardAccount
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        let govAction = TreasuryWithdrawals withdrawals (SJust scriptHash)
        proposal <- mkProposal govAction
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
                & bodyTxL . vldtTxBodyL .~ ValidityInterval SNothing SNothing
        submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]

    it "alwaysSucceeds Plutus govPolicy validates" $ whenPostBootstrap $ do
      let alwaysSucceedsSh = hashPlutusScript (alwaysSucceedsNoDatum SPlutusV3)
      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      void $
        enactConstitution
          SNothing
          (Constitution anchor (SJust alwaysSucceedsSh))
          dRep
          committeeMembers'
      rewardAccount <- registerRewardAccount

      impAnn "ParameterChange" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 1
        let govAction = ParameterChange SNothing pparamsUpdate (SJust alwaysSucceedsSh)
        mkProposal govAction >>= submitProposal_
      impAnn "TreasuryWithdrawals" $ do
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        let govAction = TreasuryWithdrawals withdrawals (SJust alwaysSucceedsSh)
        mkProposal govAction >>= submitProposal_

    it "alwaysFails Plutus govPolicy does not validate" $ whenPostBootstrap $ do
      let alwaysFailsSh = hashPlutusScript (alwaysFailsNoDatum SPlutusV3)
      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      void $
        enactConstitution SNothing (Constitution anchor (SJust alwaysFailsSh)) dRep committeeMembers'

      impAnn "ParameterChange" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 1
        let govAction = ParameterChange SNothing pparamsUpdate (SJust alwaysFailsSh)
        proposal <- mkProposal govAction
        let tx = mkBasicTx mkBasicTxBody & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
        submitPhase2Invalid_ tx

      impAnn "TreasuryWithdrawals" $ do
        rewardAccount <- registerRewardAccount
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        let govAction = TreasuryWithdrawals withdrawals (SJust alwaysFailsSh)
        proposal <- mkProposal govAction
        let tx = mkBasicTx mkBasicTxBody & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
        submitPhase2Invalid_ tx

costModelsSpec :: forall era. ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
costModelsSpec =
  -- These tests rely on the script in the constitution, but we can only change the constitution after bootstrap.
  -- So we cannot run these tests during bootstrap
  describe "PlutusV3 Initialization" $ do
    it "Updating CostModels with alwaysFails govPolicy does not validate" $ whenPostBootstrap $ do
      -- no initial PlutusV3 CostModels
      modifyPParams $ ppCostModelsL .~ testingCostModels [PlutusV1 .. PlutusV2]

      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      govIdConstitution1 <-
        enactConstitution SNothing (Constitution anchor SNothing) dRep committeeMembers'
      -- propose and enact PlutusV3 Costmodels
      govIdPPUpdate1 <-
        enactCostModels SNothing (testingCostModels [PlutusV3]) dRep committeeMembers'

      let alwaysFailsSh = hashPlutusScript (alwaysFailsNoDatum SPlutusV3)
      void $
        enactConstitution
          (SJust (GovPurposeId govIdConstitution1))
          (Constitution anchor (SJust alwaysFailsSh))
          dRep
          committeeMembers'

      impAnn "Fail to update V3 Costmodels" $ do
        let pparamsUpdate = def & ppuCostModelsL .~ SJust (testingCostModels [PlutusV3])
        let govAction = ParameterChange (SJust govIdPPUpdate1) pparamsUpdate (SJust alwaysFailsSh)
        proposal <- mkProposal govAction
        let tx = mkBasicTx mkBasicTxBody & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
        submitPhase2Invalid_ tx

    it "Updating CostModels with alwaysSucceeds govPolicy but no PlutusV3 CostModels fails" $
      whenPostBootstrap $ do
        modifyPParams $ ppCostModelsL .~ testingCostModels [PlutusV1 .. PlutusV2]

        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        anchor <- arbitrary
        let alwaysSucceedsSh = hashPlutusScript (alwaysSucceedsNoDatum SPlutusV3)
        void $
          enactConstitution
            SNothing
            (Constitution anchor (SJust alwaysSucceedsSh))
            dRep
            committeeMembers'

        let pparamsUpdate = def & ppuCostModelsL .~ SJust (testingCostModels [PlutusV3])
        let govAction = ParameterChange SNothing pparamsUpdate (SJust alwaysSucceedsSh)

        submitFailingGovAction govAction [injectFailure $ CollectErrors [NoCostModel PlutusV3]]

    it "Updating CostModels and setting the govPolicy afterwards succeeds" $ whenPostBootstrap $ do
      modifyPParams $ ppCostModelsL .~ testingCostModels [PlutusV1 .. PlutusV2]

      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000_000
      anchor <- arbitrary
      govIdConstitution1 <-
        enactConstitution SNothing (Constitution anchor SNothing) dRep committeeMembers'

      mintingTokenTx <- mkTokenMintingTx $ hashPlutusScript (evenRedeemerNoDatum SPlutusV3)

      impAnn "Minting token fails" $ do
        submitFailingTx mintingTokenTx [injectFailure $ CollectErrors [NoCostModel PlutusV3]]

      govIdPPUpdate1 <-
        enactCostModels
          SNothing
          (testingCostModels [PlutusV3])
          dRep
          committeeMembers'

      let alwaysSucceedsSh = hashPlutusScript (alwaysSucceedsNoDatum SPlutusV3)
      void $
        enactConstitution
          (SJust (GovPurposeId govIdConstitution1))
          (Constitution anchor (SJust alwaysSucceedsSh))
          dRep
          committeeMembers'

      impAnn "Minting token succeeds" $ do
        submitTx_ mintingTokenTx

      impAnn "Updating CostModels succeeds" $ do
        void $
          enactCostModels
            (SJust govIdPPUpdate1)
            (testingCostModels [PlutusV3])
            dRep
            committeeMembers'

scriptLockedTxOut ::
  forall era.
  AlonzoEraTxOut era =>
  ScriptHash ->
  TxOut era
scriptLockedTxOut shSpending =
  mkBasicTxOut
    (mkAddr shSpending StakeRefNull)
    mempty
    & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)

mkRefTxOut ::
  ( BabbageEraTxOut era
  , AlonzoEraImp era
  ) =>
  ScriptHash ->
  ImpTestM era (TxOut era)
mkRefTxOut sh = do
  addr <- freshKeyAddr_
  let mbyPlutusScript = impLookupPlutusScript sh
  pure $
    mkBasicTxOut addr mempty
      & referenceScriptTxOutL .~ maybeToStrictMaybe (fromPlutusScript <$> mbyPlutusScript)

setupRefTx ::
  forall era l.
  ( BabbageEraTxOut era
  , AlonzoEraImp era
  , PlutusLanguage l
  ) =>
  SLanguage l ->
  ImpTestM era TxId
setupRefTx lang = do
  let shSpending = hashPlutusScript (redeemerSameAsDatum lang)
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
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  ScriptHash ->
  a ->
  Lens' (TxBody TopTx era) a ->
  ContextError era ->
  ImpTestM era ()
testPlutusV1V2Failure sh badField lenz errorField = do
  txIn <- produceScript @era sh
  submitFailingTx
    ( mkBasicTx mkBasicTxBody
        & bodyTxL . inputsTxBodyL .~ Set.singleton txIn
        & bodyTxL . lenz .~ badField
    )
    ( pure . injectFailure $
        CollectErrors [BadTranslation errorField]
    )

enactCostModels ::
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'PParamUpdatePurpose) ->
  CostModels ->
  Credential 'DRepRole ->
  NonEmpty (Credential 'HotCommitteeRole) ->
  ImpTestM era (GovPurposeId 'PParamUpdatePurpose)
enactCostModels prevGovId cms dRep committeeMembers' = do
  initialCms <- getsNES $ nesEsL . curPParamsEpochStateL . ppCostModelsL
  let pparamsUpdate = def & ppuCostModelsL .~ SJust cms
  govId <- submitParameterChange (unGovPurposeId <$> prevGovId) pparamsUpdate
  submitYesVote_ (DRepVoter dRep) govId
  submitYesVoteCCs_ committeeMembers' govId
  passNEpochs 2
  enactedCms <- getsNES $ nesEsL . curPParamsEpochStateL . ppCostModelsL
  enactedCms `shouldBe` (initialCms <> cms)
  pure $ GovPurposeId govId

spendDatum :: P1.Data
spendDatum = P1.I 3
