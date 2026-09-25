{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.CertSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance (Voter (..))
import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "CERT" $ do
  it "Subtransaction consumes correct refund after keyDeposit is changed" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakingCred

    initialKeyDeposit <- getsPParams ppKeyDepositL
    let newKeyDeposit = initialKeyDeposit <> initialKeyDeposit
    impAnn "Change key deposit" $ do
      (dRep, _, _) <- setupSingleDRep 100_000_000
      ccHotCreds <- registerInitialCommittee
      ppChangeId <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuKeyDepositL .~ SJust newKeyDeposit
      submitYesVote_ (DRepVoter dRep) ppChangeId
      submitYesVoteCCs_ ccHotCreds ppChangeId
      getsPParams ppKeyDepositL `shouldReturn` initialKeyDeposit
      passNEpochs 2
      getsPParams ppKeyDepositL `shouldReturn` newKeyDeposit

    impAnn "Unregister staking credential" $ do
      let
        unregTxWithRefund refund =
          mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ SSeq.singleton (UnRegDepositTxCert stakingCred refund)
            ]

      -- the refund has to match the deposit that was actually paid at registration,
      -- not the key deposit currently in the protocol parameters
      submitFailingTx
        (unregTxWithRefund newKeyDeposit)
        [ injectFailure . DijkstraSubDelegPredFailure $
            RefundIncorrectDELEG (Mismatch newKeyDeposit initialKeyDeposit)
        ]

      submitTx_ $ unregTxWithRefund initialKeyDeposit
      expectStakeCredNotRegistered stakingCred

  it "Two sub-transactions cannot unregister the same credential" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakingCred
    keyDeposit <- getsPParams ppKeyDepositL
    (_, addr1) <- freshKeyAddr
    input1 <- sendCoinTo addr1 mempty
    (_, addr2) <- freshKeyAddr
    input2 <- sendCoinTo addr2 mempty
    let
      subTx1 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton input1
          & bodyTxL . certsTxBodyL .~ [UnRegDepositTxCert stakingCred keyDeposit]
      subTx2 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton input2
          & bodyTxL . certsTxBodyL .~ [UnRegDepositTxCert stakingCred keyDeposit]
    submitFailingTx
      (mkTopTxWithSubTxs [subTx1, subTx2])
      [injectFailure $ DijkstraSubDelegPredFailure $ StakeKeyNotRegisteredDELEG stakingCred]
