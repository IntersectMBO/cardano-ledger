{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.EnactSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (EnactSignal (..))
import Cardano.Ledger.Shelley.LedgerState
import Lens.Micro.Mtl
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  (ConwayEraImp era, GovState era ~ ConwayGovState era) =>
  SpecWith (ImpTestState era)
spec =
  describe "ENACT" $ do
    it "TreasuryWithdrawal" $ do
      rewardAcount1 <- registerRewardAccount
      govActionId <- submitTreasuryWithdrawals [(rewardAcount1, Coin 666)]
      GovActionState {gasAction = govAction} <- getGovActionState govActionId
      enactStateInit <- use $ impNESG . newEpochStateGovStateL . cgEnactStateL
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
      ensWithdrawals enactState' `shouldBe` [(getRwdCred rewardAcount1, Coin 666)]

      rewardAcount2 <- registerRewardAccount
      let withdrawals' =
            [ (rewardAcount1, Coin 111)
            , (rewardAcount2, Coin 222)
            ]
      govActionId' <- submitTreasuryWithdrawals withdrawals'
      GovActionState {gasAction = govAction'} <- getGovActionState govActionId'
      let signal' =
            EnactSignal
              { esGovActionId = govActionId'
              , esGovAction = govAction'
              }

      enactState'' <- runImpRule @"ENACT" () enactState' signal'

      ensWithdrawals enactState''
        `shouldBe` [ (getRwdCred rewardAcount1, Coin 777)
                   , (getRwdCred rewardAcount2, Coin 222)
                   ]
      ensTreasury enactState'' `shouldBe` Coin 1
