{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.EnactSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Val
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "ENACT" $ do
    basicSpec
    treasuryWithdrawalsSpec
    actionPrioritySpec

actionPrioritySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
actionPrioritySpec =
  describe "Action priority" $ do
    it "Only the first of proposals with same priority is enacted" $ do
      (drepC, _ccCH, ccCC, gpi) <- electBasicCommittee
      (poolKH, _paymentC, _stakingC) <- setupPoolWithStake $ Coin 1_000_000
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
      gai1 <-
        submitGovAction $
          UpdateCommittee (SJust gpi) (Set.singleton ccCC) mempty $
            1 %! 2
      -- gai2 is the first action of a higher priority
      gai2 <- submitGovAction $ NoConfidence $ SJust gpi
      gai3 <- submitGovAction $ NoConfidence $ SJust gpi
      submitYesVote_ (DRepVoter drepC) gai1
      submitYesVote_ (StakePoolVoter poolKH) gai1
      submitYesVote_ (DRepVoter drepC) gai2
      submitYesVote_ (StakePoolVoter poolKH) gai2
      submitYesVote_ (DRepVoter drepC) gai3
      submitYesVote_ (StakePoolVoter poolKH) gai3
      passNEpochs 2
      getLastEnactedCommittee
        `shouldReturn` SJust (GovPurposeId gai2)
      checkProposalsEmpty
    it "Contiguous ratified proposals are enacted in the same epoch, unless delayed" $ do
      (drepC, ccCH, _ccCC, _gpi) <- electBasicCommittee
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
      pGai0 <-
        submitParameterChange
          SNothing
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_000)
      pGai1 <-
        submitParameterChange
          (SJust $ GovPurposeId pGai0)
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_001)
      pGai2 <-
        submitParameterChange
          (SJust $ GovPurposeId pGai1)
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_002)
      submitYesVote_ (DRepVoter drepC) pGai0
      submitYesVote_ (CommitteeVoter ccCH) pGai0
      submitYesVote_ (DRepVoter drepC) pGai1
      submitYesVote_ (CommitteeVoter ccCH) pGai1
      submitYesVote_ (DRepVoter drepC) pGai2
      submitYesVote_ (CommitteeVoter ccCH) pGai2
      passNEpochs 2
      getLastEnactedParameterChange
        `shouldReturn` SJust (GovPurposeId pGai2)
      checkProposalsEmpty

treasuryWithdrawalsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
treasuryWithdrawalsSpec =
  describe "Treasury withdrawals" $ do
    describe "Do not result in negative balance" $ do
      it "Multiple proposals in a single Tx that add up to more than the balance" $ do
        (drepC, committeeC, _, _gpi) <- electBasicCommittee
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
        Coin curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
        rewardAccount1 <- registerRewardAccount
        rewardAccount2 <- registerRewardAccount
        rewardAccount3 <- registerRewardAccount
        rewardAccount4 <- registerRewardAccount
        rewardAccount5 <- registerRewardAccount
        let eachWithdrawal = curTreasury `div` 4
        gpi <-
          submitTreasuryWithdrawals
            [ (rewardAccount1, Coin eachWithdrawal)
            , (rewardAccount2, Coin eachWithdrawal)
            , (rewardAccount3, Coin eachWithdrawal)
            , (rewardAccount4, Coin eachWithdrawal)
            , (rewardAccount5, Coin eachWithdrawal)
            ]
        submitYesVote_ (DRepVoter drepC) gpi
        submitYesVote_ (CommitteeVoter committeeC) gpi
        passNEpochs 2
        getsNES (nesEsL . esAccountStateL . asTreasuryL)
          `shouldReturn` Coin curTreasury
        modifyNES $
          nesEsL . esAccountStateL . asTreasuryL
            %~ (<+> Coin eachWithdrawal)
        passNEpochs 2
        getsNES (nesEsL . esAccountStateL . asTreasuryL)
          `shouldReturn` Coin 0
      describe "Larger than maxBound" $ do
        it "Multiple proposals in a single Tx" $ do
          (drepC, committeeC, _, _gpi) <- electBasicCommittee
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
          modifyNES $
            nesEsL . esAccountStateL . asTreasuryL
              .~ Coin (fromIntegral (maxBound :: Word64))
          Coin curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
          rewardAccount1 <- registerRewardAccount
          rewardAccount2 <- registerRewardAccount
          rewardAccount3 <- registerRewardAccount
          rewardAccount4 <- registerRewardAccount
          rewardAccount5 <- registerRewardAccount
          let eachWithdrawal = curTreasury `div` 4
          gpi <-
            submitTreasuryWithdrawals
              [ (rewardAccount1, Coin eachWithdrawal)
              , (rewardAccount2, Coin eachWithdrawal)
              , (rewardAccount3, Coin eachWithdrawal)
              , (rewardAccount4, Coin eachWithdrawal)
              , (rewardAccount5, Coin eachWithdrawal)
              ]
          submitYesVote_ (DRepVoter drepC) gpi
          submitYesVote_ (CommitteeVoter committeeC) gpi
          passNEpochs 2
          getsNES (nesEsL . esAccountStateL . asTreasuryL)
            `shouldReturn` Coin curTreasury
        it "Proposals in multiple epochs" $ do
          (drepC, committeeC, _, _gpi) <- electBasicCommittee
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
          modifyNES $
            nesEsL . esAccountStateL . asTreasuryL
              .~ Coin (fromIntegral (maxBound :: Word64))
          Coin curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
          rewardAccount1 <- registerRewardAccount
          rewardAccount2 <- registerRewardAccount
          rewardAccount3 <- registerRewardAccount
          rewardAccount4 <- registerRewardAccount
          rewardAccount5 <- registerRewardAccount
          let eachWithdrawal = curTreasury `div` 4
          gpi1 <-
            submitTreasuryWithdrawals
              [ (rewardAccount1, Coin eachWithdrawal)
              , (rewardAccount2, Coin eachWithdrawal)
              , (rewardAccount3, Coin eachWithdrawal)
              ]
          submitYesVote_ (DRepVoter drepC) gpi1
          submitYesVote_ (CommitteeVoter committeeC) gpi1
          passNEpochs 2
          getsNES (nesEsL . esAccountStateL . asTreasuryL)
            `shouldReturn` Coin (curTreasury - 3 * eachWithdrawal)
          gpi2 <-
            submitTreasuryWithdrawals
              [ (rewardAccount4, Coin eachWithdrawal)
              , (rewardAccount5, Coin eachWithdrawal)
              ]
          submitYesVote_ (DRepVoter drepC) gpi2
          submitYesVote_ (CommitteeVoter committeeC) gpi2
          passNEpochs 2
          getsNES (nesEsL . esAccountStateL . asTreasuryL)
            `shouldReturn` Coin (curTreasury - 3 * eachWithdrawal)

basicSpec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  SpecWith (ImpTestState era)
basicSpec =
  describe "Basic" $ do
    it "TreasuryWithdrawal" $ do
      rewardAcount1 <- registerRewardAccount
      govActionId <- submitTreasuryWithdrawals [(rewardAcount1, Coin 666)]
      gas <- getGovActionState govActionId
      let govAction = gasAction gas
      enactStateInit <- getEnactState
      let signal =
            EnactSignal
              { esGovActionId = govActionId
              , esGovAction = govAction
              }
          enactState =
            enactStateInit
              { ensTreasury = Coin 1000
              }
      enactState' <- runImpRule @"ENACT" () enactState signal
      ensWithdrawals enactState' `shouldBe` [(raCredential rewardAcount1, Coin 666)]

      rewardAcount2 <- registerRewardAccount
      let withdrawals' =
            [ (rewardAcount1, Coin 111)
            , (rewardAcount2, Coin 222)
            ]
      govActionId' <- submitTreasuryWithdrawals withdrawals'
      gas' <- getGovActionState govActionId'
      let govAction' = gasAction gas'
      let signal' =
            EnactSignal
              { esGovActionId = govActionId'
              , esGovAction = govAction'
              }

      enactState'' <- runImpRule @"ENACT" () enactState' signal'

      ensWithdrawals enactState''
        `shouldBe` [ (raCredential rewardAcount1, Coin 777)
                   , (raCredential rewardAcount2, Coin 222)
                   ]
      ensTreasury enactState'' `shouldBe` Coin 1
    it "HardForkInitiation" $ do
      (_, committeeMember, _, _) <- electBasicCommittee
      modifyPParams $ \pp ->
        pp
          & ppDRepVotingThresholdsL
            %~ ( \dvt ->
                  dvt
                    { dvtHardForkInitiation = 2 %! 3
                    }
               )
          & ppPoolVotingThresholdsL
            %~ ( \pvt ->
                  pvt
                    { pvtHardForkInitiation = 2 %! 3
                    }
               )
          & ppGovActionLifetimeL
            .~ EpochInterval 20
      _ <- setupPoolWithStake $ Coin 22_000_000
      (stakePoolId1, _, _) <- setupPoolWithStake $ Coin 22_000_000
      (stakePoolId2, _, _) <- setupPoolWithStake $ Coin 22_000_000
      (dRep1, _, _) <- setupSingleDRep 11_000_000
      (dRep2, _, _) <- setupSingleDRep 11_000_000
      curProtVer <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL
      nextMajorVersion <- succVersion $ pvMajor curProtVer
      let nextProtVer = curProtVer {pvMajor = nextMajorVersion}
      govActionId <- submitGovAction $ HardForkInitiation SNothing nextProtVer
      submitYesVote_ (CommitteeVoter committeeMember) govActionId
      submitYesVote_ (DRepVoter (KeyHashObj dRep1)) govActionId
      submitYesVote_ (StakePoolVoter stakePoolId1) govActionId
      passNEpochs 2
      getsNES (nesEsL . curPParamsEpochStateL . ppProtocolVersionL) `shouldReturn` curProtVer
      submitYesVote_ (DRepVoter (KeyHashObj dRep2)) govActionId
      passNEpochs 2
      getsNES (nesEsL . curPParamsEpochStateL . ppProtocolVersionL) `shouldReturn` curProtVer
      submitYesVote_ (StakePoolVoter stakePoolId2) govActionId
      passNEpochs 2
      getsNES (nesEsL . curPParamsEpochStateL . ppProtocolVersionL) `shouldReturn` nextProtVer
    it "NoConfidence" $ do
      modifyPParams $ \pp ->
        pp
          & ppDRepVotingThresholdsL . dvtCommitteeNoConfidenceL .~ 1 %! 2
          & ppPoolVotingThresholdsL . pvtCommitteeNoConfidenceL .~ 1 %! 2
          & ppCommitteeMaxTermLengthL .~ EpochInterval 200
      let
        getCommittee =
          getsNES $
            nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
        assertNoCommittee :: HasCallStack => ImpTestM era ()
        assertNoCommittee =
          do
            committee <- getCommittee
            impAnn "There should not be a committee" $ committee `shouldBe` SNothing
      assertNoCommittee
      khCC <- freshKeyHash
      (drep, _, _) <- setupSingleDRep 1_000_000
      let committeeMap =
            Map.fromList
              [ (KeyHashObj khCC, EpochNo 50)
              ]
      prevGaidCommittee@(GovPurposeId gaidCommittee) <-
        electCommittee
          SNothing
          drep
          mempty
          committeeMap
      (khSPO, _, _) <- setupPoolWithStake $ Coin 42_000_000
      logStakeDistr
      submitYesVote_ (StakePoolVoter khSPO) gaidCommittee
      replicateM_ 4 passEpoch
      impAnn "Committee should be elected" $ do
        committee <- getCommittee
        committee `shouldBe` SJust (Committee committeeMap $ 1 %! 2)
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      returnAddr <- registerRewardAccount
      gaidNoConf <-
        submitProposal $
          ProposalProcedure
            { pProcReturnAddr = returnAddr
            , pProcGovAction = NoConfidence (SJust prevGaidCommittee)
            , pProcDeposit = pp ^. ppGovActionDepositL
            , pProcAnchor = def
            }
      submitYesVote_ (StakePoolVoter khSPO) gaidNoConf
      submitYesVote_ (DRepVoter $ KeyHashObj drep) gaidNoConf
      replicateM_ 4 passEpoch
      assertNoCommittee
    it "Constitution" $ do
      (dRep, committeeMember, _, _) <- electBasicCommittee
      (govActionId, constitution) <- submitConstitution SNothing

      ConwayGovState proposalsBeforeVotes enactStateBeforeVotes pulserBeforeVotes _ _ _ <-
        getsNES $ newEpochStateGovStateL @era
      submitYesVote_ (DRepVoter dRep) govActionId
      submitYesVote_ (CommitteeVoter committeeMember) govActionId
      ConwayGovState proposalsAfterVotes enactStateAfterVotes pulserAfterVotes _ _ _ <-
        getsNES newEpochStateGovStateL

      impAnn "Votes are recorded in the proposals" $ do
        let proposalsWithVotes =
              proposalsAddVote
                (CommitteeVoter committeeMember)
                VoteYes
                govActionId
                ( proposalsAddVote
                    (DRepVoter dRep)
                    VoteYes
                    govActionId
                    proposalsBeforeVotes
                )
        proposalsAfterVotes `shouldBe` proposalsWithVotes

      impAnn "EnactState has not changed" $
        enactStateAfterVotes `shouldBe` enactStateBeforeVotes

      impAnn "Pulser has not changed" $
        pulserAfterVotes `shouldBe` pulserBeforeVotes

      passEpoch

      impAnn "New constitution is not enacted after one epoch" $ do
        constitutionAfterOneEpoch <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        constitutionAfterOneEpoch `shouldBe` def

      impAnn "Pulser should reflect the constitution to be enacted" $ do
        ConwayGovState _ _ _ _ _ pulser <- getsNES newEpochStateGovStateL
        let ratifyState = extractDRepPulsingState pulser
        gasId <$> rsEnacted ratifyState `shouldBe` govActionId Seq.:<| Seq.Empty
        rsEnactState ratifyState ^. ensConstitutionL `shouldBe` constitution

      passEpoch

      impAnn "Constitution is enacted after two epochs" $ do
        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        curConstitution `shouldBe` constitution

      impAnn "Pulser is reset" $ do
        ConwayGovState _ _ _ _ _ pulser <- getsNES newEpochStateGovStateL
        let pulserRatifyState = extractDRepPulsingState pulser
        rsEnacted pulserRatifyState `shouldBe` Seq.empty
        enactState <- getEnactState
        rsEnactState pulserRatifyState `shouldBe` enactState
