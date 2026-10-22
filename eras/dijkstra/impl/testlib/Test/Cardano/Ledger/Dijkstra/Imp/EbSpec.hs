{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EbSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (EbReferencesAnnouncement (..), LeiosEraBlockHeader (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (EbPredFailure (..))
import Control.Monad.State.Strict (MonadState (..))
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( DijkstraEraImp era
  , HashAnnotated (Tx TopTx era) EraIndependentTx
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "EB" $ do
  describe "Applying EB" $ do
    let
      genSimpleTx :: ImpTestM era (AnyLevelTx era)
      genSimpleTx = do
        addr <- freshKeyAddr_
        amnt <- inject . Coin <$> choose (100, 1000000000)
        pure $
          AnyLevelTx $
            mkBasicTx @era mkBasicTxBody
              & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr amnt]

      genRBandEB = simulateThenRestore $ do
        nBlockTxs <- choose (0, 10)
        blockTxs <- impRecordSubmittedTxs $ replicateM_ nBlockTxs $ do
          AnyLevelTx tx <- genSimpleTx
          submitAnyTx tx
        nEbTxs <- choose (0, 100)
        ebTxs <- impRecordSubmittedTxs $ replicateM_ nEbTxs $ do
          AnyLevelTx tx <- genSimpleTx
          submitAnyTx tx
        eb <- makeEndorserBlock ebTxs
        pure (blockTxs, eb)

    it "Succeeds when EB is announced" $ do
      (blockTxs, eb) <- genRBandEB
      let announcement = mkEbReferencesAnnouncement eb
      st <- get
      issuer <- freshKeyHash
      impAnn "Submit RB" $
        expectRightDeepExpr_
          =<< tryTxsInBlock
            (ebReferencesAnnouncementBlockHeaderL .~ SJust announcement)
            blockTxs
            st
            issuer
      impAnn "Submit EB" $ applyEndorserBlock eb
    it "Fails when the EB is not announced" $ do
      ebTxs <- simulateThenRestore $ do
        nEbTxs <- choose (0, 100)
        impRecordSubmittedTxs $ replicateM_ nEbTxs $ do
          AnyLevelTx tx <- genSimpleTx
          submitAnyTx tx
      eb <- makeEndorserBlock ebTxs
      impAnn "Submit EB" $
        applyFailingEndorserBlock
          eb
          [ injectFailure $ EbWasNotAnnounced $ hashAnnotated $ eb ^. referencesEndorserBlockL
          ]
    it "Fails when the EB announcment has a different hash" $ do
      (blockTxs, eb) <- genRBandEB
      announcement <- arbitrary
      st <- get
      issuer <- freshKeyHash
      impAnn "Submit RB" $
        expectRightDeepExpr_
          =<< tryTxsInBlock
            (ebReferencesAnnouncementBlockHeaderL .~ SJust announcement)
            blockTxs
            st
            issuer
      impAnn "Submit EB" $
        applyFailingEndorserBlock
          eb
          [ injectFailure $
              EbAnnouncementHashMismatch
                Mismatch
                  { mismatchSupplied = hashAnnotated $ eb ^. referencesEndorserBlockL
                  , mismatchExpected = ebReferencesAnnouncementHash announcement
                  }
          ]
