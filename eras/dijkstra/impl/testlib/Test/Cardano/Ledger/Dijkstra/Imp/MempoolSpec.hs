{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.MempoolSpec (spec) where

import Cardano.Ledger.BaseTypes (Mismatch (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraMempoolPredFailure (..),
  DijkstraSubUtxoPredFailure (..),
  DijkstraUtxoPredFailure (..),
 )
import Cardano.Ledger.State (utxoG)
import Data.Maybe (isNothing)
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "MEMPOOL" $ do
  describe "AllInputsAreSpent" $ do
    it "a transaction that has already been applied" $ do
      txIn <- freshFundedTxIn
      appliedTx <- submitTx $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
      withNoFixup $ submitFailingMempoolTx appliedTx [AllInputsAreSpent]

    it "a transaction whose inputs were each spent by a different transaction" $ do
      firstTxIn <- freshFundedTxIn
      secondTxIn <- freshFundedTxIn
      submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [firstTxIn]
      submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [secondTxIn]
      withNoFixup $
        submitFailingMempoolTx
          (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [firstTxIn, secondTxIn])
          [AllInputsAreSpent]

    it "a transaction with no inputs" $
      withNoFixup $
        submitFailingMempoolTx (mkBasicTx mkBasicTxBody) [AllInputsAreSpent]

    it "a batch whose sub-transaction inputs are unspent, but whose own are not" $ do
      spentTxIn <- freshFundedTxIn
      submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [spentTxIn]
      unspentTxIn <- freshFundedTxIn
      let subTx :: Tx SubTx era
          subTx = mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [unspentTxIn]
      withNoFixup $
        submitFailingMempoolTx
          (mkTopTxWithSubTxs [subTx] & bodyTxL . inputsTxBodyL .~ [spentTxIn])
          [AllInputsAreSpent]

  describe "LedgerFailure" $ do
    it "an input that is already spent, alongside one that is not" $ do
      spentTxIn <- freshFundedTxIn
      submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [spentTxIn]
      unspentTxIn <- freshFundedTxIn
      submitFailingMempoolTx
        (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [spentTxIn, unspentTxIn])
        [ LedgerFailure . injectFailure . BadInputsUTxO $ NES.singleton spentTxIn
        , LedgerFailure . injectFailure . BadInputsUTxO $ NES.singleton spentTxIn
        ]

    it "an input of a sub-transaction that is already spent" $ do
      spentTxIn <- freshFundedTxIn
      submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [spentTxIn]
      let subTx :: Tx SubTx era
          subTx = mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [spentTxIn]
      submitFailingMempoolTx
        (mkTopTxWithSubTxs [subTx])
        [ LedgerFailure . injectFailure . SubBadInputsUTxO $ NES.singleton spentTxIn
        , LedgerFailure . injectFailure . SubBadInputsUTxO $ NES.singleton spentTxIn
        ]

  -- `MempoolFailure` is unreachable in Dijkstra: the check that raises
  -- it in Conway, on votes cast by unelected committee members, is not
  -- part of this transition. It remains as the target that such a Conway
  -- failure injects into, and as the meaning of CBOR tag 2 on the wire.

  describe "Accepted at the boundary" $
    it "a transaction whose inputs are all unspent" $ do
      txIn <- freshFundedTxIn
      (mempoolState, _) <-
        expectRight
          =<< trySubmitMempoolTx (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn])
      expectUTxOContent (mempoolState ^. utxoG) [(txIn, isNothing)]

  describe "Composite tests" $ do
    it "no ledger check runs when all inputs are spent" $ do
      spentTxIn <- freshFundedTxIn
      submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [spentTxIn]
      let subTx :: Tx SubTx era
          subTx = mkBasicTx $ mkBasicTxBody & networkIdTxBodyL .~ SJust Mainnet
      withNoFixup $
        submitFailingMempoolTx
          (mkTopTxWithSubTxs [subTx] & bodyTxL . inputsTxBodyL .~ [spentTxIn])
          [AllInputsAreSpent]
      submitFailingMempoolTx
        (mkTopTxWithSubTxs [subTx])
        [ LedgerFailure . injectFailure . SubWrongNetworkInTxBody $
            Mismatch {mismatchSupplied = Mainnet, mismatchExpected = Testnet}
        ]

    it "a transaction the mempool validated, once it has been applied" $ do
      txIn <- freshFundedTxIn
      tx <- fixupTx $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
      withNoFixup $ do
        (_, validatedTx) <- expectRight =<< trySubmitMempoolTx tx
        submitTx_ tx
        reapplyResult <- tryReapplyMempoolTx validatedTx
        expectMempoolRejection reapplyResult [AllInputsAreSpent]
