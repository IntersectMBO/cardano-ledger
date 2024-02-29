{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.EnactSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (EpochInterval (..), ProtVer (..), StrictMaybe (..), succVersion)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (EnactSignal (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (
  curPParamsEpochStateL,
  nesEsL,
 )
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spec =
  describe "ENACT" $ do
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
