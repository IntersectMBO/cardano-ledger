{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.MempoolSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (hardforkConwayDisallowUnelectedCommitteeFromVoting)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayGovPredFailure (UnelectedCommitteeVoters),
  ConwayLedgerPredFailure (..),
  ConwayUtxoPredFailure (BadInputsUTxO),
  PredicateFailure,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (nesELL)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "MEMPOOL" $ do
  it "Duplicate transactions" $ whenMajorVersionAtMost @11 $ do
    inputsCommon <- replicateM 5 freshFundedTxIn
    inputsCommonNES <- expectJust $ NES.fromFoldable inputsCommon
    inputs1 <- replicateM 2 freshFundedTxIn
    inputs2 <- replicateM 3 freshFundedTxIn

    txFinal <-
      submitTx . mkBasicTx $
        mkBasicTxBody & inputsTxBodyL .~ fromList (inputsCommon <> inputs1)

    impAnn "Identical transaction" $
      withNoFixup $
        submitFailingMempoolTx
          txFinal
          [ injectFailure . ConwayMempoolFailure $
              "All inputs are spent. Transaction has probably already been included"
          ]

    impAnn "Overlapping transaction" $
      submitFailingMempoolTx
        (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ fromList (inputsCommon <> inputs2))
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
    submitTx_ $ mkBasicTx (mkBasicTxBody & proposalProceduresTxBodyL .~ [proposal])
    ccHot <- registerCommitteeHotKey ccCold
    govActionId <- do
      accountAddress <- registerAccountAddress
      submitTreasuryWithdrawals [(accountAddress, Coin 1)]

    let tx =
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
      then submitFailingTx tx [injectFailure $ UnelectedCommitteeVoters [ccHot]]
      else do
        txFixed <- fixupTx tx
        withNoFixup $ do
          submitFailingMempoolTx
            txFixed
            [ injectFailure . ConwayMempoolFailure $
                "Unelected committee members are not allowed to cast votes: "
                  <> T.pack (show (pure @[] ccHot))
            ]
          submitTx_ txFixed

submitFailingMempoolTx ::
  (HasCallStack, ConwayEraImp era) =>
  Tx TopTx era ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
  ImpTestM era ()
submitFailingMempoolTx tx expectedFailures = do
  result <- trySubmitMempoolTx tx
  case result of
    Left applyTxError -> applyTxError `shouldBeExpr` inject expectedFailures
    Right _ -> assertFailure $ "Expected a mempool rejection with: " <> show expectedFailures
