{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.CertSpec (spec) where

import Cardano.Ledger.Conway.Governance (Voter (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core (
  EraTx (..),
  EraTxBody (..),
  emptyPParamsUpdate,
  ppKeyDepositL,
  ppuKeyDepositL,
  pattern UnRegDepositTxCert,
 )
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest (
  DijkstraEraImp,
  ImpInit,
  LedgerSpec,
  expectStakeCredNotRegistered,
  expectStakeCredRegistered,
  freshKeyHash,
  getsPParams,
  impAnn,
  passNEpochs,
  registerInitialCommittee,
  registerStakeCredential,
  setupSingleDRep,
  submitParameterChange,
  submitTx_,
  submitYesVoteCCs_,
  submitYesVote_,
 )
import Test.Cardano.Ledger.Imp.Common (SpecWith, it, shouldReturn)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "Subtransaction consumes correct refund after keyDeposit is changed" $ do
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

  -- it "Multiple subtransactions cannot get the same refund" $ do
  --   undefined
