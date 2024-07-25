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

module Test.Cardano.Ledger.Conway.Imp.GovCertSpec (
  spec,
  relevantDuringBootstrapSpec,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), EpochNo (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (GovAction (..), GovPurposeId (..), Voter (..))
import Cardano.Ledger.Conway.Rules (ConwayGovCertPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = do
  relevantDuringBootstrapSpec
  it "Enact UpdateCommitee with lengthy lifetime" $ do
    NonNegative n <- arbitrary
    passNEpochs n
    (drepCred, _, _) <- setupSingleDRep 1_000_000
    cc <- KeyHashObj <$> freshKeyHash
    EpochInterval committeeMaxTermLength <-
      getsNES $ nesEsL . curPParamsEpochStateL . ppCommitteeMaxTermLengthL
    secondAddCCGaid <-
      submitUpdateCommittee Nothing [(cc, EpochInterval (committeeMaxTermLength + 2))] (1 %! 2)
    submitYesVote_ (DRepVoter drepCred) secondAddCCGaid
    passNEpochs 2
    -- Due to longer than allowed lifetime we have to wait an extra epoch for this new action to be enacted
    expectCommitteeMemberAbsence cc
    passEpoch
    expectCommitteeMemberPresence cc

  it "Authorizing proposed CC key" $ do
    someCred <- KeyHashObj <$> freshKeyHash
    submitGovAction_ $
      UpdateCommittee SNothing mempty (Map.singleton someCred (EpochNo 1234)) (1 %! 2)
    submitTx_
      ( mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.singleton (ResignCommitteeColdTxCert someCred SNothing)
      )
  -- A CC that has resigned will need to be first voted out and then voted in to be considered active
  it "CC re-election" $
    do
      (drepCred, _, _) <- setupSingleDRep 1_000_000
      passNEpochs 2
      -- Add a fresh CC
      cc <- KeyHashObj <$> freshKeyHash
      addCCGaid <- submitUpdateCommittee Nothing [(cc, EpochInterval 10)] (1 %! 2)
      submitYesVote_ (DRepVoter drepCred) addCCGaid
      passNEpochs 2
      -- Confirm that they are added
      expectCommitteeMemberPresence cc
      -- Confirm their hot key registration
      _hotKey <- registerCommitteeHotKey cc
      ccShouldNotBeResigned cc
      -- Have them resign
      _ <- resignCommitteeColdKey cc SNothing
      ccShouldBeResigned cc
      -- Re-add the same CC
      reAddCCGaid <- submitUpdateCommittee Nothing [(cc, EpochInterval 20)] (1 %! 2)
      submitYesVote_ (DRepVoter drepCred) reAddCCGaid
      passNEpochs 2
      -- Confirm that they are still resigned
      ccShouldBeResigned cc
      -- Remove them
      let removeCCAction = UpdateCommittee (SJust $ GovPurposeId reAddCCGaid) (Set.singleton cc) mempty (1 %! 2)
      removeCCGaid <- submitGovAction removeCCAction
      submitYesVote_ (DRepVoter drepCred) removeCCGaid
      passNEpochs 2
      -- Confirm that they have been removed
      expectCommitteeMemberAbsence cc
      secondAddCCGaid <-
        submitUpdateCommittee Nothing [(cc, EpochInterval 20)] (1 %! 2)
      submitYesVote_ (DRepVoter drepCred) secondAddCCGaid
      passNEpochs 2
      -- Confirm that they have been added
      expectCommitteeMemberPresence cc
      -- Confirm that after registering a hot key, they are active
      _hotKey <- registerCommitteeHotKey cc
      ccShouldNotBeResigned cc

relevantDuringBootstrapSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  ) =>
  SpecWith (ImpTestState era)
relevantDuringBootstrapSpec = do
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
