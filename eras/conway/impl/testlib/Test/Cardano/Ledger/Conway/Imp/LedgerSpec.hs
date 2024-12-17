{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.LedgerSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayLedgerEvent (..),
  ConwayLedgerPredFailure (..),
  ConwayMempoolEvent (..),
  maxRefScriptSizePerTx,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..), mkMempoolEnv)
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ShelleyLedgersEnv (..), ShelleyLedgersEvent (..))
import Control.State.Transition.Extended
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsWithDatum,
  alwaysSucceedsNoDatum,
 )

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , Event (EraRule "MEMPOOL" era) ~ ConwayMempoolEvent era
  , BaseM (EraRule "LEDGERS" era) ~ ShelleyBase
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq.Seq (Tx era)
  , Event (EraRule "LEDGERS" era) ~ ShelleyLedgersEvent era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  , STS (EraRule "LEDGERS" era)
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

  describe "Mempool events" $ do
    it "No Mempool events should be emitted via LEDGERS rules " $ do
      nes <- use impNESL
      slotNo <- use impLastTickG
      let ls = nes ^. nesEsL . esLStateL
          pp = nes ^. nesEsL . curPParamsEpochStateL
          account = nes ^. nesEsL . esAccountStateL
          epochNo = nes ^. nesELL
      tx <- fixupTx $ mkBasicTx mkBasicTxBody
      Right (_, evs) <-
        tryRunImpRule @"LEDGERS"
          (LedgersEnv slotNo epochNo pp account)
          ls
          (Seq.singleton tx)
      let mempoolEvents = [ev | LedgerEvent ev@(MempoolEvent (ConwayMempoolEvent _)) <- evs]
      mempoolEvents `shouldBeExpr` []

    it "Mempool events should be emitted via `applyTx` with `mkMempoolEnv`" $ do
      globals <- use impGlobalsL
      slotNo <- use impLastTickG
      nes <- use impNESL
      let ls = nes ^. nesEsL . esLStateL

      let mempoolEnv = mkMempoolEnv nes slotNo
      tx <- fixupTx $ mkBasicTx mkBasicTxBody
      let stsOpts =
            ApplySTSOpts
              { asoAssertions = AssertionsAll
              , asoValidation = ValidateAll
              , asoEvents = EPReturn
              }
      case applyTxOpts stsOpts globals mempoolEnv ls tx of
        Left e ->
          assertFailure $ "Unexpected failure while applyingTx: " <> show tx <> ": " <> show e
        Right (_, evs) ->
          length [ev | ev@(MempoolEvent (ConwayMempoolEvent _)) <- evs] `shouldBe` 1
