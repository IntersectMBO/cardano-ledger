{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovCertSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  EraGov (..),
  InjectRuleFailure (..),
  ppDRepDepositL,
 )
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState,
  GovAction (..),
  GovPurposeId (..),
  Voter (..),
  committeeMembersL,
 )
import Cardano.Ledger.Conway.PParams (ppCommitteeMaxTermLengthL)
import Cardano.Ledger.Conway.Rules (ConwayGovCertPredFailure (..))
import Cardano.Ledger.Conway.TxCert (
  pattern AuthCommitteeHotKeyTxCert,
  pattern RegDRepTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern UnRegDRepTxCert,
 )
import Cardano.Ledger.Core (EraTx (..), EraTxBody (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (
  curPParamsEpochStateL,
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosGovStateL,
 )
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common hiding (assertBool, shouldBe)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "GOVCERT" $ do
  it
    "A CC that has resigned will need to be first voted out and then voted in to be considered active"
    $ do
      modifyPParams $ \pp -> pp & ppCommitteeMaxTermLengthL .~ EpochInterval 20
      (drepKh, _, _) <- setupSingleDRep 1_000_000
      passNEpochs 2
      -- Add a fresh CC
      cc <- KeyHashObj <$> freshKeyHash
      let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (1 %! 2)
      addCCGaid <- submitGovAction addCCAction
      submitYesVote_ (DRepVoter (KeyHashObj drepKh)) addCCGaid
      passNEpochs 2
      -- Confirm that they are added
      SJust committee <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      let assertCCMembership comm =
            assertBool "Expected CC to be present in the committee" $
              Map.member cc (comm ^. committeeMembersL)
          assertCCMissing comm =
            assertBool "Expected CC to be absent in the committee" $
              Map.notMember cc (comm ^. committeeMembersL)
      assertCCMembership committee
      -- Confirm their hot key registration
      _hotKey <- registerCommitteeHotKey cc
      ccShouldNotBeResigned cc
      -- Have them resign
      resignCommitteeColdKey cc SNothing
      ccShouldBeResigned cc
      -- Re-add the same CC
      let reAddCCAction = UpdateCommittee (SJust $ GovPurposeId addCCGaid) mempty (Map.singleton cc 20) (1 %! 2)
      reAddCCGaid <- submitGovAction reAddCCAction
      submitYesVote_ (DRepVoter (KeyHashObj drepKh)) reAddCCGaid
      passNEpochs 2
      -- Confirm that they are still resigned
      ccShouldBeResigned cc
      -- Remove them
      let removeCCAction = UpdateCommittee (SJust $ GovPurposeId reAddCCGaid) (Set.singleton cc) mempty (1 %! 2)
      removeCCGaid <- submitGovAction removeCCAction
      submitYesVote_ (DRepVoter (KeyHashObj drepKh)) removeCCGaid
      passNEpochs 2
      -- Confirm that they have been removed
      SJust committeeAfterRemove <-
        getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      assertCCMissing committeeAfterRemove
      -- Add the same CC back a second time
      let secondAddCCAction = UpdateCommittee (SJust $ GovPurposeId removeCCGaid) mempty (Map.singleton cc 20) (1 %! 2)
      secondAddCCGaid <- submitGovAction secondAddCCAction
      submitYesVote_ (DRepVoter (KeyHashObj drepKh)) secondAddCCGaid
      passNEpochs 2
      -- Confirm that they have been added
      SJust committeeAfterRemoveAndAdd <-
        getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      assertCCMembership committeeAfterRemoveAndAdd
      -- Confirm that after registering a hot key, they are active
      _hotKey <- registerCommitteeHotKey cc
      ccShouldNotBeResigned cc
  describe "succeeds for" $ do
    it "registering and unregistering a DRep" $ do
      modifyPParams $ ppDRepDepositL .~ Coin 100
      drepCred <- KeyHashObj <$> freshKeyHash
      drepDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppDRepDepositL
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.singleton (RegDRepTxCert drepCred drepDeposit SNothing)
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.singleton (UnRegDRepTxCert drepCred drepDeposit)
    it "resigning a non-CC key" $ do
      someCred <- KeyHashObj <$> freshKeyHash
      submitTx_
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton (ResignCommitteeColdTxCert someCred SNothing)
        )
    it "re-registering a CC hot key" $ do
      void registerInitialCommittee
      initialCommittee <- getCommitteeMembers
      forM_ initialCommittee $ \kh ->
        replicateM_ 10 $ do
          ccHotCred <- KeyHashObj <$> freshKeyHash
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ SSeq.singleton (AuthCommitteeHotKeyTxCert kh ccHotCred)
  describe "fails for" $ do
    it "invalid deposit provided with DRep registration cert" $ do
      modifyPParams $ ppDRepDepositL .~ Coin 100
      expectedDRepDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppDRepDepositL
      let providedDRepDeposit = expectedDRepDeposit <+> Coin 10
      khDRep <- freshKeyHash
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton
                (RegDRepTxCert (KeyHashObj khDRep) providedDRepDeposit SNothing)
        )
        ( pure . injectFailure $
            ConwayDRepIncorrectDeposit providedDRepDeposit expectedDRepDeposit
        )
    it "invalid refund provided with DRep deregistration cert" $ do
      modifyPParams $ ppDRepDepositL .~ Coin 100
      drepDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppDRepDepositL
      let refund = drepDeposit <+> Coin 10
      drepCred <- KeyHashObj <$> freshKeyHash
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.singleton
              (RegDRepTxCert drepCred drepDeposit SNothing)
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton
                (UnRegDRepTxCert drepCred refund)
        )
        ( pure . injectFailure $
            ConwayDRepIncorrectRefund refund drepDeposit
        )
    it "DRep already registered" $ do
      modifyPParams $ ppDRepDepositL .~ Coin 100
      drepDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppDRepDepositL
      drepCred <- KeyHashObj <$> freshKeyHash
      let
        regTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton
                (RegDRepTxCert drepCred drepDeposit SNothing)
      submitTx_ regTx
      submitFailingTx
        regTx
        (pure . injectFailure $ ConwayDRepAlreadyRegistered drepCred)
    it "unregistering a nonexistent DRep" $ do
      modifyPParams $ ppDRepDepositL .~ Coin 100
      drepDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppDRepDepositL
      drepCred <- KeyHashObj <$> freshKeyHash
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton (UnRegDRepTxCert drepCred drepDeposit)
        )
        (pure . injectFailure $ ConwayDRepNotRegistered drepCred)
    it "registering a resigned CC member hotkey" $ do
      void registerInitialCommittee
      initialCommittee <- getCommitteeMembers
      forM_ initialCommittee $ \ccCred -> do
        ccHotCred <- KeyHashObj <$> freshKeyHash
        let
          registerHotKeyTx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ SSeq.singleton (AuthCommitteeHotKeyTxCert ccCred ccHotCred)
        submitTx_ registerHotKeyTx
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton (ResignCommitteeColdTxCert ccCred SNothing)
        submitFailingTx
          registerHotKeyTx
          (pure . injectFailure $ ConwayCommitteeHasPreviouslyResigned ccCred)
