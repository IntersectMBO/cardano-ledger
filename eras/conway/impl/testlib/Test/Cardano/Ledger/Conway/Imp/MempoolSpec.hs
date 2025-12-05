{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.MempoolSpec (conwayEraSpecificSpec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayGovPredFailure (UnelectedCommitteeVoters),
  ConwayLedgerPredFailure (..),
  ConwayUtxoPredFailure (BadInputsUTxO),
  PredicateFailure,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.API.Mempool (applyTx, mkMempoolEnv)
import Cardano.Ledger.Shelley.LedgerState
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

conwayEraSpecificSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
conwayEraSpecificSpec =
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

    it "Duplicate transactions" $ do
      let
        newInput = do
          addr <- freshKeyAddr_
          amount <- Coin <$> choose (2_000_000, 8_000_000)
          sendCoinTo addr amount

      inputsCommon <- replicateM 5 newInput
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
          [injectFailure $ BadInputsUTxO $ fromList inputsCommon]

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
        rewardAccount <- registerRewardAccount
        submitTreasuryWithdrawals [(rewardAccount, Coin 1)]

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
              pure . injectFailure . ConwayMempoolFailure $
                "Unelected committee members are not allowed to cast votes: " <> T.pack (show (pure @[] ccHot))
          withNoFixup $ submitTx_ txFixed
