{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.GovCertSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (Vote (..), Voter (..))
import Cardano.Ledger.Conway.Rules (ConwayGovCertPredFailure (..), ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.Val (Val (..))
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
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "GOVCERT" $ do
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
  describe "Potential future committee members" $ do
    let
      setupGovEnv = do
        modifyPParams $ \pp ->
          pp
            & ppCommitteeMinSizeL .~ 1
            & ppCommitteeMaxTermLengthL .~ EpochInterval 30
            & ppGovActionLifetimeL .~ EpochInterval 30
        disableTreasuryExpansion
        donateToTreasury $ Coin 1_000
        (drep, _, _) <- setupSingleDRep 1_000_000_000
        (spo, _, _) <- setupPoolWithStake $ Coin 1_000_000_000
        pure (drep, spo)
      proposeWithdrawalAndMember drep = do
        accountAddr <- registerAccountAddress
        gaiWithdrawal <- submitTreasuryWithdrawals [(accountAddr, Coin 1)]
        submitYesVote_ (DRepVoter drep) gaiWithdrawal
        initialMembers <- getCommitteeMembers
        proposedMember <- KeyHashObj <$> freshKeyHash
        gaiUpdateCommittee <-
          submitUpdateCommittee Nothing initialMembers [(proposedMember, EpochInterval 20)] (1 %! 2)
        pure (gaiWithdrawal, initialMembers, proposedMember, gaiUpdateCommittee)
    it "can authorize a hot key, but an unknown cold credential cannot" $
      whenPostBootstrap $ do
        unknownColdCred <- KeyHashObj <$> freshKeyHash
        unknownHotCred <- KeyHashObj <$> freshKeyHash
        let
          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ SSeq.singleton (AuthCommitteeHotKeyTxCert unknownColdCred unknownHotCred)
        submitFailingTx
          tx
          [injectFailure $ ConwayCommitteeIsUnknown unknownColdCred]
        void $ submitUpdateCommittee Nothing mempty [(unknownColdCred, EpochInterval 20)] (1 %! 2)
        submitTx_ tx
    it "at protocol version 10, the vote is counted once the committee update is enacted" $
      whenMajorVersion @10 $ do
        (drep, spo) <- setupGovEnv
        (gaiWithdrawal, initialMembers, proposedMember, gaiUpdateCommittee) <-
          proposeWithdrawalAndMember drep
        submitYesVote_ (DRepVoter drep) gaiUpdateCommittee
        submitYesVote_ (StakePoolVoter spo) gaiUpdateCommittee
        passEpoch
        expectMembers initialMembers
        proposedMemberHotKey <- registerCommitteeHotKey proposedMember
        submitYesVote_ (CommitteeVoter proposedMemberHotKey) gaiWithdrawal
        isCommitteeAccepted gaiWithdrawal `shouldReturn` False
        passEpoch
        expectMembers $ Set.singleton proposedMember
        ccShouldNotBeExpired proposedMember
        isCommitteeAccepted gaiWithdrawal `shouldReturn` True
        passNEpochs 2
        expectMissingGovActionId gaiUpdateCommittee
        expectMissingGovActionId gaiWithdrawal
    it "at protocol version 10, the vote is discarded when the committee update is rejected" $
      whenMajorVersion @10 $ do
        (drep, spo) <- setupGovEnv
        (gaiWithdrawal, initialMembers, proposedMember, gaiUpdateCommittee) <-
          proposeWithdrawalAndMember drep
        submitVote_ VoteNo (DRepVoter drep) gaiUpdateCommittee
        submitVote_ VoteNo (StakePoolVoter spo) gaiUpdateCommittee
        passEpoch
        expectMembers initialMembers
        proposedMemberHotKey <- registerCommitteeHotKey proposedMember
        submitYesVote_ (CommitteeVoter proposedMemberHotKey) gaiWithdrawal
        isCommitteeAccepted gaiWithdrawal `shouldReturn` False
        passNEpochs 2
        expectPresentGovActionId gaiUpdateCommittee
        expectCommitteeMemberAbsence proposedMember
        isCommitteeAccepted gaiWithdrawal `shouldReturn` False
        expectPresentGovActionId gaiWithdrawal
    it "from protocol version 11, cannot vote until elected" $
      whenMajorVersionAtLeast @11 $ do
        accountAddr <- registerAccountAddress
        gaiWithdrawal <- submitTreasuryWithdrawals [(accountAddr, Coin 1)]
        proposedMember <- KeyHashObj <$> freshKeyHash
        void $ submitUpdateCommittee Nothing mempty [(proposedMember, EpochInterval 20)] (1 %! 2)
        proposedMemberHotKey <- registerCommitteeHotKey proposedMember
        submitFailingVote
          (CommitteeVoter proposedMemberHotKey)
          gaiWithdrawal
          [injectFailure $ UnelectedCommitteeVoters [proposedMemberHotKey]]
        passNEpochs . fromIntegral =<< choose (1, 5 :: Int)
        submitFailingVote
          (CommitteeVoter proposedMemberHotKey)
          gaiWithdrawal
          [ injectFailure $ UnelectedCommitteeVoters [proposedMemberHotKey]
          , injectFailure $ VotersDoNotExist [CommitteeVoter proposedMemberHotKey]
          ]
