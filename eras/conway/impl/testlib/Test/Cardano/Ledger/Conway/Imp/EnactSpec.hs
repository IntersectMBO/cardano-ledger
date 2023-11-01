{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.ImpTest

spec :: forall era. (ConwayEraImp era, GovState era ~ ConwayGovState era) => Spec
spec =
  describe "ENACT" $ do
    itM @era "TreasuryWithdrawal" $ do
      rewardAcount1 <- registerRewardAccount
      let govAction = TreasuryWithdrawals [(rewardAcount1, Coin 666)]
      govActionId <- submitProposal govAction
      enactStateInit <- use $ impNESL . newEpochStateGovStateL . cgEnactStateL
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
      impIO $ ensWithdrawals enactState' `shouldBe` [(getRwdCred rewardAcount1, Coin 666)]

      rewardAcount2 <- registerRewardAccount
      let withdrawals' =
            [ (rewardAcount1, Coin 111)
            , (rewardAcount2, Coin 222)
            ]
          govAction' = TreasuryWithdrawals withdrawals'
      govActionId' <- submitProposal govAction'
      let signal' =
            EnactSignal
              { esGovActionId = govActionId'
              , esGovAction = govAction'
              }

      enactState'' <- runImpRule @"ENACT" () enactState' signal'

      impIO $ do
        ensWithdrawals enactState''
          `shouldBe` [ (getRwdCred rewardAcount1, Coin 777)
                     , (getRwdCred rewardAcount2, Coin 222)
                     ]
        ensTreasury enactState'' `shouldBe` Coin 1
