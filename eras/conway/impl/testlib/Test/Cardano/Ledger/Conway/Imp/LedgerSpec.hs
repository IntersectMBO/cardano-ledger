{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.LedgerSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (
  hardforkConwayBootstrapPhase,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules (
  ConwayGovPredFailure (UnelectedCommitteeVoters),
  ConwayLedgerPredFailure (..),
  ConwayUtxoPredFailure (BadInputsUTxO),
  PredicateFailure,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.API.Mempool (applyTx, mkMempoolEnv)
import Cardano.Ledger.Shelley.LedgerState
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
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
    ra <- getAccountAddressFor cred

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
    ra <- getAccountAddressFor cred
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

  describe "Mempool" $ do
    let
      submitFailingMempoolTx ::
        String ->
        Tx TopTx era ->
        NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
        ImpM (LedgerSpec era) (Tx TopTx era)
      submitFailingMempoolTx cause tx expectedFailures = do
        globals <- use impGlobalsL
        nes <- use impNESL
        slotNo <- use impCurSlotNoG
        let
          mempoolEnv = mkMempoolEnv nes slotNo
          ls = nes ^. nesEsL . esLStateL
        txFixed <- (tx &) =<< asks iteFixup
        logToExpr txFixed
        case applyTx globals mempoolEnv ls txFixed of
          Left err -> do
            err `shouldBe` inject expectedFailures
          Right _ ->
            assertFailure $ "Expected failure due to " <> cause <> ": " <> show txFixed
        pure txFixed
      submitFailingMempoolTx_ ::
        String ->
        Tx TopTx era ->
        NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
        ImpM (LedgerSpec era) ()
      submitFailingMempoolTx_ c t = void . submitFailingMempoolTx c t

    -- We disable this test for Dijkstra, for now.
    -- It will need to be moved to its own `MempoolSpec` module and enabled for Dijkstra as well
    -- when we start calling `applyTx` in ImpSpec, instead of directly the `LEDGER` rule
    it "Duplicate transactions" $ whenMajorVersionAtMost @11 $ do
      let
        newInput = do
          addr <- freshKeyAddr_
          amount <- Coin <$> choose (2_000_000, 8_000_000)
          sendCoinTo addr amount

      inputsCommon <- replicateM 5 newInput
      inputsCommonNES <- case NES.fromFoldable inputsCommon of
        Nothing -> error "Impossible empty set"
        Just nes -> pure nes
      inputs1 <- replicateM 2 newInput
      inputs2 <- replicateM 3 newInput

      txFinal <-
        submitTx $
          mkBasicTx $
            mkBasicTxBody & inputsTxBodyL <>~ fromList (inputsCommon <> inputs1)

      impAnn "Identical transaction" $ do
        withNoFixup $
          submitFailingMempoolTx_ "duplicate transaction" txFinal $
            NonEmpty.singleton . injectFailure . ConwayMempoolFailure $
              "All inputs are spent. Transaction has probably already been included"

      impAnn "Overlapping transaction" $ do
        let txOverlap = mkBasicTx $ mkBasicTxBody & inputsTxBodyL <>~ fromList (inputsCommon <> inputs2)
        submitFailingMempoolTx_
          "overlapping transaction"
          txOverlap
          [injectFailure $ BadInputsUTxO inputsCommonNES]

    it "Unelected Committee voting" $ whenPostBootstrap $ do
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
        accountAddress <- registerAccountAddress
        submitTreasuryWithdrawals [(accountAddress, Coin 1)]

      let
        tx =
          mkBasicTx $
            mkBasicTxBody
              & votingProceduresTxBodyL
                .~ VotingProcedures
                  ( Map.singleton
                      (CommitteeVoter ccHot)
                      (Map.singleton govActionId (VotingProcedure VoteYes SNothing))
                  )
      pv <- getProtVer
      if hardforkConwayDisallowUnelectedCommitteeFromVoting pv
        then
          submitFailingTx tx [injectFailure $ UnelectedCommitteeVoters [ccHot]]
        else do
          txFixed <-
            submitFailingMempoolTx "unallowed votes" tx $
              NonEmpty.singleton . injectFailure . ConwayMempoolFailure $
                "Unelected committee members are not allowed to cast votes: " <> T.pack (show (pure @[] ccHot))
          withNoFixup $ submitTx_ txFixed
