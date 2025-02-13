{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.LedgerSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayLedgerPredFailure (..),
  maxRefScriptSizePerTx,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..), ApplyTxError (..), applyTx, mkMempoolEnv)
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsWithDatum,
  alwaysSucceedsNoDatum,
 )

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , ApplyTx era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "TxRefScriptsSizeTooBig" $ do
    -- we use here the largest script we currently have as many times as necessary to
    -- trigger the predicate failure
    Just plutusScript <- pure $ mkPlutusScript @era $ alwaysFailsWithDatum SPlutusV3
    let script :: Script era
        script = fromPlutusScript plutusScript
        size = originalBytesSize script
        n = maxRefScriptSizePerTx `div` size + 1
    txIns <- replicateM n (produceRefScript script)
    let tx :: Tx era
        tx = mkBasicTx (mkBasicTxBody & referenceInputsTxBodyL .~ Set.fromList txIns)
    submitFailingTx
      tx
      [ injectFailure $
          ConwayTxRefScriptsSizeTooBig $
            Mismatch
              { mismatchSupplied = size * n
              , mismatchExpected = maxRefScriptSizePerTx
              }
      ]

  it "Withdraw from delegated and non-delegated staking key" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    kh <- freshKeyHash
    let cred = KeyHashObj kh
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    let tx = mkBasicTx $ mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, reward)]

    pv <- getProtVer
    if HF.bootstrapPhase pv
      then submitTx_ tx
      else
        submitFailingTx
          tx
          [injectFailure $ ConwayWdrlNotDelegatedToDRep [kh]]
    _ <- delegateToDRep cred (Coin 1_000_000) DRepAlwaysAbstain
    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL
            .~ Withdrawals
              [(ra, if HF.bootstrapPhase pv then mempty else reward)]

  it "Withdraw from a key delegated to an unregistered DRep" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    kh <- freshKeyHash
    let cred = KeyHashObj kh
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    (drep, _, _) <- setupSingleDRep 1_000_000

    _ <- delegateToDRep cred (Coin 1_000_000) (DRepCredential drep)

    unRegisterDRep drep
    expectDRepNotRegistered drep
    let tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(ra, reward)]
    ifBootstrap (submitTx_ tx >> (lookupReward cred `shouldReturn` mempty)) $ do
      submitFailingTx tx [injectFailure $ ConwayWdrlNotDelegatedToDRep [kh]]

  it "Withdraw and unregister staking credential in the same transaction" $ do
    refund <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
    kh <- freshKeyHash
    let cred = KeyHashObj kh
    ra <- registerStakeCredential cred
    Positive newDeposit <- arbitrary
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 2
        & ppKeyDepositL .~ Coin newDeposit

    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    (drep, _, _) <- setupSingleDRep 1_000_000

    _ <- delegateToDRep cred (Coin 1_000_000) (DRepCredential drep)

    let tx =
          mkBasicTx $
            mkBasicTxBody
              & certsTxBodyL .~ [UnRegDepositTxCert cred refund]
              & (withdrawalsTxBodyL .~ Withdrawals [(ra, reward)])
    submitTx_ tx

  it "Withdraw from a key delegated to an expired DRep" $ do
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 4
        & ppDRepActivityL .~ EpochInterval 1
    kh <- freshKeyHash
    let cred = KeyHashObj kh
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    (drep, _, _) <- setupSingleDRep 1_000_000

    -- expire the drep before delegation
    mkMinFeeUpdateGovAction SNothing >>= submitGovAction_
    passNEpochs 4
    isDRepExpired drep `shouldReturn` True

    _ <- delegateToDRep cred (Coin 1_000_000) (DRepCredential drep)

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL
            .~ Withdrawals
              [(ra, reward)]

  it "Withdraw from a key delegated to a DRep that expired after delegation" $ do
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 4
        & ppDRepActivityL .~ EpochInterval 1
    kh <- freshKeyHash
    let cred = KeyHashObj kh
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    (drep, _, _) <- setupSingleDRep 1_000_000

    _ <- delegateToDRep cred (Coin 1_000_000) (DRepCredential drep)

    -- expire the drep after delegation
    mkMinFeeUpdateGovAction SNothing >>= submitGovAction_

    passNEpochs 4
    isDRepExpired drep `shouldReturn` True

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL
            .~ Withdrawals
              [(ra, reward)]

  it "Withdraw from delegated and non-delegated staking script" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    let scriptHash = hashPlutusScript $ alwaysSucceedsNoDatum SPlutusV3
    let cred = ScriptHashObj scriptHash
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    reward <- lookupReward cred

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, reward)]

    _ <- delegateToDRep cred (Coin 1_000_000) DRepAlwaysAbstain
    submitTx_ $
      mkBasicTx $
        mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, mempty)]

  describe "Mempool" $ do
    it "Unelected Committee voting" $ whenPostBootstrap $ do
      globals <- use impGlobalsL
      slotNo <- use impLastTickG
      _ <- registerInitialCommittee
      ccCold <- KeyHashObj <$> freshKeyHash
      curEpochNo <- getsNES nesELL
      let action =
            UpdateCommittee
              SNothing
              mempty
              (Map.singleton ccCold (addEpochInterval curEpochNo (EpochInterval 7)))
              (1 %! 1)
      proposal <- mkProposal action
      submitTx_ $
        mkBasicTx (mkBasicTxBody & proposalProceduresTxBodyL .~ [proposal])
      ccHot <- registerCommitteeHotKey ccCold
      govActionId <- do
        rewardAccount <- registerRewardAccount
        submitTreasuryWithdrawals [(rewardAccount, Coin 1)]

      nes <- use impNESL
      let ls = nes ^. nesEsL . esLStateL
          mempoolEnv = mkMempoolEnv nes slotNo
      tx <-
        fixupTx $
          mkBasicTx $
            mkBasicTxBody
              & votingProceduresTxBodyL
                .~ VotingProcedures
                  ( Map.singleton
                      (CommitteeVoter ccHot)
                      (Map.singleton govActionId (VotingProcedure VoteYes SNothing))
                  )

      case applyTx globals mempoolEnv ls tx of
        Left err ->
          let expectedFailure =
                ConwayMempoolFailure $
                  "Unelected committee members are not allowed to cast votes: " <> T.pack (show (pure @[] ccHot))
           in err `shouldBe` ApplyTxError @era (pure (injectFailure expectedFailure))
        Right _ -> assertFailure $ "Expected failure due to an unallowed vote: " <> show tx
      withNoFixup $ submitTx_ tx
