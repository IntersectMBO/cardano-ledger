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
import Cardano.Ledger.Dijkstra (DijkstraApplyBlock (..), applyEndorserBlockNoValidation)
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (EbPredFailure (..))
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Control.Monad.State.Strict (MonadState (..), gets)
import Control.State.Transition.Extended (SingEP (..), ValidationPolicy (..))
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.BlockHeader (TestBlockHeader)
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
        pp <- getsPParams id
        addr <- freshKeyAddr_
        amnt <- inject . Coin <$> choose (100, 1000000000)
        pure $
          AnyLevelTx $
            mkBasicTx @era mkBasicTxBody
              & bodyTxL . outputsTxBodyL .~ [ensureMinCoinTxOut pp $ mkBasicTxOut addr amnt]

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

      submitAnnouncingRB blockTxs announcement = do
        st <- get
        issuer <- freshKeyHash
        impAnn "Submit RB" $
          expectRightDeepExpr_
            =<< tryTxsInBlock
              (ebReferencesAnnouncementBlockHeaderL .~ SJust announcement)
              blockTxs
              st
              issuer

    it "Succeeds when EB is announced" $ do
      (blockTxs, eb) <- genRBandEB
      submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      impAnn "Submit EB" $ submitEndorserBlock eb
    it "Produces the same state with and without validation" $ do
      (blockTxs, eb) <- genRBandEB
      submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      nes <- getsNES id
      globals <- gets (^. impGlobalsL)
      let (validatedNes, failures, _) =
            applyEndorserBlock @TestBlockHeader @era EPDiscard ValidateAll globals nes eb
      failures `shouldBeExpr` []
      applyEndorserBlockNoValidation @TestBlockHeader @era globals nes eb
        `shouldBeExpr` validatedNes
    it "Fails when the EB is not announced" $ do
      ebTxs <- simulateThenRestore $ do
        nEbTxs <- choose (0, 100)
        impRecordSubmittedTxs $ replicateM_ nEbTxs $ do
          AnyLevelTx tx <- genSimpleTx
          submitAnyTx tx
      eb <- makeEndorserBlock ebTxs
      impAnn "Submit EB" $
        submitFailingEndorserBlock
          eb
          [ injectFailure $ EbWasNotAnnounced $ hashAnnotated $ eb ^. referencesEndorserBlockL
          ]
    it "Fails when the EB announcment has a different hash" $ do
      (blockTxs, eb) <- genRBandEB
      announcement <- arbitrary
      submitAnnouncingRB blockTxs announcement
      impAnn "Submit EB" $
        submitFailingEndorserBlock
          eb
          [ injectFailure $
              EbAnnouncementHashMismatch
                Mismatch
                  { mismatchSupplied = hashAnnotated $ eb ^. referencesEndorserBlockL
                  , mismatchExpected = ebReferencesAnnouncementHash announcement
                  }
          ]
