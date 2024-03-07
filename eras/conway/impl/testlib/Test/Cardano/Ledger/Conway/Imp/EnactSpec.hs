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
import Cardano.Ledger.Val ((<+>))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational
import Test.Cardano.Ledger.Imp.Common

spec :: ConwayEraImp era => SpecWith (ImpTestState era)
spec =
  describe "ENACT" $ do
    treasuryWithdrawalsSpec
    hardForkInitiationSpec
    noConfidenceSpec
    constitutionSpec

treasuryWithdrawalsSpec :: ConwayEraImp era => SpecWith (ImpTestState era)
treasuryWithdrawalsSpec =
  describe "Treasury withdrawals" $ do
    it "modify EnactState as expected" $ do
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

    it "Multiple proposals in a single Tx that add up to more than the balance" $ do
      (drepC, committeeC, _gpi) <- electBasicCommittee
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
    it "Multiple proposals in a single Tx that add up to more than maxBound Word64" $ do
      (drepC, committeeC, _gpi) <- electBasicCommittee
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
      (drepC, committeeC, _gpi) <- electBasicCommittee
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

hardForkInitiationSpec :: ConwayEraImp era => SpecWith (ImpTestState era)
hardForkInitiationSpec =
  it "HardForkInitiation" $ do
    (_, committeeMember, _) <- electBasicCommittee
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

noConfidenceSpec :: forall era. ConwayEraImp era => SpecWith (ImpTestState era)
noConfidenceSpec =
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

constitutionSpec :: ConwayEraImp era => SpecWith (ImpTestState era)
constitutionSpec =
  it "Constitution" $ do
    (dRep, committeeMember, _) <- electBasicCommittee
    (govActionId, constitution) <- submitConstitution SNothing

    proposalsBeforeVotes <- getsNES $ newEpochStateGovStateL . proposalsGovStateL
    pulserBeforeVotes <- getsNES newEpochStateDRepPulsingStateL

    submitYesVote_ (DRepVoter dRep) govActionId
    submitYesVote_ (CommitteeVoter committeeMember) govActionId

    proposalsAfterVotes <- getsNES $ newEpochStateGovStateL . proposalsGovStateL
    pulserAfterVotes <- getsNES newEpochStateDRepPulsingStateL

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

    impAnn "Pulser has not changed" $
      pulserAfterVotes `shouldBe` pulserBeforeVotes

    passEpoch

    impAnn "New constitution is not enacted after one epoch" $ do
      constitutionAfterOneEpoch <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
      constitutionAfterOneEpoch `shouldBe` def

    impAnn "Pulser should reflect the constitution to be enacted" $ do
      pulser <- getsNES newEpochStateDRepPulsingStateL
      let ratifyState = extractDRepPulsingState pulser
      gasId <$> rsEnacted ratifyState `shouldBe` govActionId Seq.:<| Seq.Empty
      rsEnactState ratifyState ^. ensConstitutionL `shouldBe` constitution

    passEpoch

    impAnn "Constitution is enacted after two epochs" $ do
      curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
      curConstitution `shouldBe` constitution

    impAnn "Pulser is reset" $ do
      pulser <- getsNES newEpochStateDRepPulsingStateL
      let pulserRatifyState = extractDRepPulsingState pulser
      rsEnacted pulserRatifyState `shouldBe` Seq.empty
      enactState <- getEnactState
      rsEnactState pulserRatifyState `shouldBe` enactState
