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
import Cardano.Ledger.Conway (
  hardforkConwayBootstrapPhase,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules (
  ConwayLedgerPredFailure (..),
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Set as Set
import Data.Word (Word32)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysSucceedsNoDatum,
  purposeIsWellformedNoDatum,
 )

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "TxRefScriptsSizeTooBig" $ do
    -- we use here the largest script we currently have as many times as necessary to
    -- trigger the predicate failure
    plutusScript <- mkPlutusScript @era $ purposeIsWellformedNoDatum SPlutusV3
    pp <- getsPParams id
    let script :: Script era
        script = fromPlutusScript plutusScript
        size = originalBytesSize script
        maxRefScriptSizePerTx = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerTxG
        n = maxRefScriptSizePerTx `div` size + 1
    txIns <- replicateM n (produceRefScript script)
    let tx :: Tx TopTx era
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
    balance <- getBalance cred

    let tx = mkBasicTx $ mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, balance)]

    pv <- getProtVer
    if hardforkConwayBootstrapPhase pv
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
              [(ra, if hardforkConwayBootstrapPhase pv then mempty else balance)]

  it "Withdraw from a key delegated to an unregistered DRep" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    kh <- freshKeyHash
    let cred = KeyHashObj kh
    ra <- registerStakeCredential cred
    submitAndExpireProposalToMakeReward cred
    balance <- getBalance cred

    (drep, _, _) <- setupSingleDRep 1_000_000

    unRegisterDRep drep
    expectDRepNotRegistered drep
    let tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(ra, balance)]
    ifBootstrap (submitTx_ tx >> (getBalance cred `shouldReturn` mempty)) $ do
      submitFailingTx tx [injectFailure $ ConwayWdrlNotDelegatedToDRep [kh]]

  -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
  -- TODO: Re-enable after issue is resolved, by removing this override
  disableInConformanceIt "Withdraw and unregister staking credential in the same transaction" $ do
    refund <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
    (_, cred, _) <- setupSingleDRep 1_000_000
    ra <- getRewardAccountFor cred

    Positive newDeposit <- arbitrary
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 2
        & ppKeyDepositL .~ Coin newDeposit

    submitAndExpireProposalToMakeReward cred
    balance <- getBalance cred

    let tx =
          mkBasicTx $
            mkBasicTxBody
              & certsTxBodyL .~ [UnRegDepositTxCert cred refund]
              & (withdrawalsTxBodyL .~ Withdrawals [(ra, balance)])
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
    balance <- getBalance cred

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
              [(ra, balance)]

  -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/635
  -- TODO: Re-enable after issue is resolved, by removing this override
  disableInConformanceIt "Withdraw from a key delegated to a DRep that expired after delegation" $ do
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 4
        & ppDRepActivityL .~ EpochInterval 1
    (drep, cred, _) <- setupSingleDRep 1_000_000
    ra <- getRewardAccountFor cred
    submitAndExpireProposalToMakeReward cred
    balance <- getBalance cred

    -- expire the drep after delegation
    mkMinFeeUpdateGovAction SNothing >>= submitGovAction_

    passNEpochs 4
    isDRepExpired drep `shouldReturn` True

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL
            .~ Withdrawals
              [(ra, balance)]

  it "Withdraw from delegated and non-delegated staking script" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    let scriptHash = hashPlutusScript $ alwaysSucceedsNoDatum SPlutusV3
    let cred = ScriptHashObj scriptHash
    ra <- registerStakeCredential cred
    void $ delegateToDRep cred (Coin 1_000_000) DRepAlwaysAbstain
    submitAndExpireProposalToMakeReward cred
    balance <- getBalance cred

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, balance)]

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(ra, mempty)]
