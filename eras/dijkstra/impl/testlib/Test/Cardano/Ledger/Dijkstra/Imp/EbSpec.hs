{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EbSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (
  EbReferencesAnnouncement (..),
  LeiosEraBlockHeader (..),
  ebReferencesAnnouncementHashL,
  ebReferencesAnnouncementSizeL,
  slotNoBlockHeaderL,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra (
  DijkstraApplyBlock (..),
  InvalidEndorserBlockReferences (..),
  applyEndorserBlockNoValidation,
 )
import Cardano.Ledger.Dijkstra.Block.Endorser (EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (ppMaxEndorserBlockReferencesSizeL)
import Cardano.Ledger.Dijkstra.Rules (
  EbAlreadyApplied (..),
  EbAnnouncementHashMismatch (..),
  EbAnnouncementSizeMismatch (..),
  EbAnnouncementSlotMismatch (..),
  EbReferencesSizeTooBig (..),
  EbWasNotAnnounced (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  lsLastEbAnnouncementL,
  nesEsL,
  nesLastTickSlotNoL,
 )
import Cardano.Ledger.State (EbAnnouncement (..))
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (MonadState (..), gets)
import Control.State.Transition.Extended (SingEP (..), ValidationPolicy (..))
import Data.Word (Word32)
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
        nEbTxs <- choose (1, 100)
        ebTxs <- impRecordSubmittedTxs $ replicateM_ nEbTxs $ do
          AnyLevelTx tx <- genSimpleTx
          submitAnyTx tx
        eb <- makeEndorserBlock ebTxs
        pure (blockTxs, eb)

      submitAnnouncingRB blockTxs announcement = do
        tickToCurSlot
        st <- get
        issuer <- freshKeyHash
        impAnn "Submit RB" $
          expectRightDeepExpr
            =<< tryTxsInBlock
              (ebReferencesAnnouncementBlockHeaderL .~ SJust announcement)
              blockTxs
              st
              issuer

      ebRefsSize eb = originalBytesSize $ eb ^. referencesEndorserBlockL

      expectNoValidationToThrow eb expected = do
        nes <- getsNES id
        globals <- gets (^. impGlobalsL)
        liftIO $
          evaluate (applyEndorserBlockNoValidation @TestBlockHeader @era globals nes eb)
            `shouldThrow` \(err :: InvalidEndorserBlockReferences era) ->
              case err of
                InvalidEndorserBlockReferences failures _ -> fmap (injectFailure @"EB") failures == expected

      expectSameStateWithoutValidation eb = do
        nes <- getsNES id
        globals <- gets (^. impGlobalsL)
        let (validatedNes, failures, _) =
              applyEndorserBlock @TestBlockHeader @era EPDiscard ValidateAll globals nes eb
        failures `shouldBeExpr` []
        applyEndorserBlockNoValidation @TestBlockHeader @era globals nes eb
          `shouldBeExpr` validatedNes

      injectEbFailure :: InjectRuleFailure "EB" t era => t era -> EraRuleFailure "EB" era
      injectEbFailure = injectFailure

      ebRefsHash eb = hashAnnotated $ eb ^. referencesEndorserBlockL

      announcementHashMismatch eb announcedHash =
        injectEbFailure $
          EbAnnouncementHashMismatch
            Mismatch {mismatchSupplied = ebRefsHash eb, mismatchExpected = announcedHash}

      announcementSizeMismatch eb announcedSize =
        injectEbFailure $
          EbAnnouncementSizeMismatch
            Mismatch
              { mismatchSupplied = ebRefsSize eb
              , mismatchExpected = fromIntegral @Word32 @Int announcedSize
              }

      referencesSizeTooBig eb maxEbRefsSize =
        injectEbFailure $
          EbReferencesSizeTooBig
            Mismatch {mismatchSupplied = ebRefsSize eb, mismatchExpected = maxEbRefsSize}

    it "Succeeds when EB is announced" $ do
      (blockTxs, eb) <- genRBandEB
      _ <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      impAnn "Submit EB" $ submitEndorserBlock eb
      fmap ebIsApplied
        <$> getsNES (nesEsL . esLStateL . lsLastEbAnnouncementL)
          `shouldReturn` SJust True
    it "Produces the same state with and without validation" $ do
      (blockTxs, eb) <- genRBandEB
      _ <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      expectSameStateWithoutValidation eb
    it "Fails when the EB is not announced" $ do
      (_, eb) <- genRBandEB
      impAnn "Submit EB" $
        submitFailingEndorserBlock eb [injectFailure $ EbWasNotAnnounced $ ebRefsHash eb]
    it "Fails when the EB announcement has a different hash" $ do
      (blockTxs, eb) <- genRBandEB
      wrongHash <- arbitrary
      _ <-
        submitAnnouncingRB blockTxs $
          mkEbReferencesAnnouncement eb & ebReferencesAnnouncementHashL .~ wrongHash
      impAnn "Submit EB" $ submitFailingEndorserBlock eb [announcementHashMismatch eb wrongHash]
    it "Fails when the EB announcement has a different size" $ do
      (blockTxs, eb) <- genRBandEB
      let announcement = mkEbReferencesAnnouncement eb
          wrongSize = ebReferencesAnnouncementSize announcement + 1
      _ <- submitAnnouncingRB blockTxs $ announcement & ebReferencesAnnouncementSizeL .~ wrongSize
      impAnn "Submit EB" $ submitFailingEndorserBlock eb [announcementSizeMismatch eb wrongSize]
    it "Fails when the EB is applied at a slot after the announcing block" $ do
      (blockTxs, eb) <- genRBandEB
      block <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      passTick
      tickToCurSlot
      laterSlot <- getsNES nesLastTickSlotNoL
      impAnn "Submit EB" $
        submitFailingEndorserBlock
          eb
          [ injectFailure $
              EbAnnouncementSlotMismatch
                Mismatch
                  { mismatchSupplied = laterSlot
                  , mismatchExpected = block ^. slotNoBlockHeaderL
                  }
          ]
    it "Succeeds when the EB references are exactly at the size limit" $ do
      (blockTxs, eb) <- genRBandEB
      _ <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ fromIntegral @Int @Word32 (ebRefsSize eb)
      expectSameStateWithoutValidation eb
    it "Fails when the EB references exceed the size limit" $ do
      (blockTxs, eb) <- genRBandEB
      let maxEbRefsSize = ebRefsSize eb - 1
      _ <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ fromIntegral @Int @Word32 maxEbRefsSize
      impAnn "Submit EB" $ submitFailingEndorserBlock eb [referencesSizeTooBig eb maxEbRefsSize]
    it "Errors without validation when the EB was not announced" $ do
      (_, eb) <- genRBandEB
      expectNoValidationToThrow eb [injectFailure $ EbWasNotAnnounced $ ebRefsHash eb]
    it "Errors without validation with every failure of the EB references, ignoring the slot" $ do
      (blockTxs, eb) <- genRBandEB
      wrongHash <- arbitrary
      let announcedSize = ebReferencesAnnouncementSize (mkEbReferencesAnnouncement eb) + 1
          maxEbRefsSize = ebRefsSize eb - 1
      _ <-
        submitAnnouncingRB blockTxs $
          mkEbReferencesAnnouncement eb
            & ebReferencesAnnouncementHashL .~ wrongHash
            & ebReferencesAnnouncementSizeL .~ announcedSize
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ fromIntegral @Int @Word32 maxEbRefsSize
      passTick
      tickToCurSlot
      expectNoValidationToThrow
        eb
        [ referencesSizeTooBig eb maxEbRefsSize
        , announcementHashMismatch eb wrongHash
        , announcementSizeMismatch eb announcedSize
        ]
    it "Fails when the EB is applied again" $ do
      (blockTxs, eb) <- genRBandEB
      _ <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      impAnn "Submit EB" $ submitEndorserBlock eb
      let alreadyApplied = injectEbFailure $ EbAlreadyApplied $ ebRefsHash eb
      impAnn "Submit EB again" $ submitFailingEndorserBlock eb [alreadyApplied]
      expectNoValidationToThrow eb [alreadyApplied]
