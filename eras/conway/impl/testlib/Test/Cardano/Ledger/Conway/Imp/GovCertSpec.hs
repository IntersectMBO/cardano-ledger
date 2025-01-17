{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.GovCertSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayGovCertPredFailure (..), ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.Val (Val (..))
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "Resigning proposed CC key" $ do
    ccColdCred <- KeyHashObj <$> freshKeyHash
    proposal <- mkUpdateCommitteeProposal Nothing mempty [(ccColdCred, EpochInterval 1234)] (1 %! 2)
    mbGovId <-
      submitBootstrapAwareFailingProposal proposal $
        FailBootstrap [injectFailure $ DisallowedProposalDuringBootstrap proposal]
    forM_ mbGovId $ \_ ->
      submitTx_
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton (ResignCommitteeColdTxCert ccColdCred SNothing)
        )
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
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton (ResignCommitteeColdTxCert someCred SNothing)
        )
        (pure (injectFailure $ ConwayCommitteeIsUnknown someCred))
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
            ConwayDRepIncorrectDeposit $
              Mismatch
                { mismatchSupplied = providedDRepDeposit
                , mismatchExpected = expectedDRepDeposit
                }
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
            ConwayDRepIncorrectRefund $
              Mismatch
                { mismatchSupplied = refund
                , mismatchExpected = drepDeposit
                }
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
    it "resigning a nonexistent CC member hotkey" $ do
      void registerInitialCommittee
      nonExistentColdKey <- KeyHashObj <$> freshKeyHash
      let failingTx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ SSeq.singleton (ResignCommitteeColdTxCert nonExistentColdKey SNothing)
      submitFailingTx
        failingTx
        [ injectFailure $ ConwayCommitteeIsUnknown nonExistentColdKey
        ]
