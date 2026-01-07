{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.CertSpec (spec) where

import Cardano.Ledger.Conway.Governance (Voter (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  xit "Subtransaction consumes correct refund after keyDeposit is changed" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakingCred

    initialKeyDeposit <- getsPParams ppKeyDepositL
    impAnn "Change key deposit" $ do
      (dRep, _, _) <- setupSingleDRep 100_000_000
      ccHotCreds <- registerInitialCommittee
      let newKeyDeposit = initialKeyDeposit <> initialKeyDeposit
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
      expectStakeCredRegistered stakingCred
      let
        deRegCert = UnRegDepositTxCert stakingCred initialKeyDeposit
        subTransaction =
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ SSeq.singleton deRegCert
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTransaction
      expectStakeCredNotRegistered stakingCred

  xit "Multiple subtransactions cannot get the same refund" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakingCred
    keyDeposit <- getsPParams ppKeyDepositL
    value1 <- arbitrary
    (_, addr1) <- freshKeyAddr
    input1 <- sendCoinTo addr1 value1
    value2 <- arbitrary
    (_, addr2) <- freshKeyAddr
    input2 <- sendCoinTo addr2 value2
    let
      subTx1 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton input1
          & bodyTxL . certsTxBodyL .~ SSeq.singleton (UnRegDepositTxCert stakingCred keyDeposit)
      subTx2 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton input2
          & bodyTxL . certsTxBodyL .~ SSeq.singleton (UnRegDepositTxCert stakingCred keyDeposit)
      tx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable [subTx1, subTx2]
    submitFailingTx tx . NE.singleton $ error "TODO: predicate failure not yet implemented"
