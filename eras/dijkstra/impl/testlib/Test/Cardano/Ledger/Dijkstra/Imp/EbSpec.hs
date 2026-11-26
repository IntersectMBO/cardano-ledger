{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Cardano.Ledger.Dijkstra.Block.Endorser (EbTransactions (..), EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (
  ppMaxEndorserBlockExUnitsL,
  ppMaxEndorserBlockReferencesSizeL,
  ppMaxEndorserBlockTxsSizeL,
  ppMaxRefScriptSizePerEndorserBlockL,
 )
import Cardano.Ledger.Dijkstra.Rules (
  EbAlreadyApplied (..),
  EbAnnouncementHashMismatch (..),
  EbAnnouncementSizeMismatch (..),
  EbAnnouncementSlotMismatch (..),
  EbExUnitsTooBig (..),
  EbRefScriptsSizeTooBig (..),
  EbReferencesSizeTooBig (..),
  EbTxsSizeTooBig (..),
  EbWasNotAnnounced (..),
 )
import Cardano.Ledger.Plutus (ExUnits (..), OrdExUnits (..), SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  lsLastEbAnnouncementL,
  nesEsL,
  nesLastTickSlotNoL,
 )
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.State (CanGetUTxO (..), EbAnnouncement (..))
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (MonadState (..), gets)
import Control.State.Transition.Extended (SingEP (..), ValidationPolicy (..))
import Data.Foldable (toList)
import Data.Word (Word32)
import Lens.Micro (Lens', (&), (.~), (^.))
import Test.Cardano.Ledger.BlockHeader (TestBlockHeader)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

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

      submitSimpleTxs :: Int -> ImpTestM era ()
      submitSimpleTxs n = replicateM_ n $ do
        AnyLevelTx tx <- genSimpleTx
        submitAnyTx tx

      genRBandEB = simulateThenRestore $ do
        nBlockTxs <- choose (0, 10)
        blockTxs <- impRecordSubmittedTxs $ submitSimpleTxs nBlockTxs
        nEbTxs <- choose (1, 100)
        ebTxs <- impRecordSubmittedTxs $ submitSimpleTxs nEbTxs
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

      applyEbWithoutValidation eb = do
        globals <- gets (^. impGlobalsL)
        nes <- getsNES id
        pure $ applyEndorserBlockNoValidation @TestBlockHeader @era globals nes eb

      expectNoValidationToThrow eb expected = do
        nesWithoutValidation <- applyEbWithoutValidation eb
        liftIO $
          evaluate nesWithoutValidation
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
            Mismatch
              { mismatchSupplied = ebRefsSize eb
              , mismatchExpected = fromIntegral @Word32 @Int maxEbRefsSize
              }

      endorsedTxs eb = toList . unEbTransactions $ eb ^. transactionsEndorserBlockL

      txsSizeTooBig txsSize maxTxsSize =
        injectEbFailure $
          EbTxsSizeTooBig Mismatch {mismatchSupplied = txsSize, mismatchExpected = maxTxsSize}

      makeAnnouncedEbFrom txsAction = do
        eb <- simulateThenRestore $ impRecordSubmittedTxs txsAction >>= makeEndorserBlock
        eb <$ announceEndorserBlock eb

      makeAnnouncedSimpleEb = choose (2, 10) >>= makeAnnouncedEbFrom . submitSimpleTxs

      expectEbLimit ::
        EndorserBlock era ->
        Lens' (PParams era) a ->
        a ->
        a ->
        EraRuleFailure "EB" era ->
        ImpTestM era ()
      expectEbLimit eb limitL exactLimit lowerLimit failure = do
        modifyPParams $ limitL .~ exactLimit
        expectSameStateWithoutValidation eb
        modifyPParams $ limitL .~ lowerLimit
        impAnn "Submit EB" $ submitFailingEndorserBlock eb [failure]

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
      maxEbRefsSize <- integralToBounded @Int @Word32 $ ebRefsSize eb
      _ <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ maxEbRefsSize
      expectSameStateWithoutValidation eb
    it "Fails when the EB references exceed the size limit" $ do
      (blockTxs, eb) <- genRBandEB
      maxEbRefsSize <- integralToBounded @Int @Word32 $ ebRefsSize eb - 1
      _ <- submitAnnouncingRB blockTxs $ mkEbReferencesAnnouncement eb
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ maxEbRefsSize
      impAnn "Submit EB" $ submitFailingEndorserBlock eb [referencesSizeTooBig eb maxEbRefsSize]
    it "Errors without validation when the EB was not announced" $ do
      (_, eb) <- genRBandEB
      expectNoValidationToThrow eb [injectFailure $ EbWasNotAnnounced $ ebRefsHash eb]
    it "Errors without validation with every failure of the EB references, ignoring the slot" $ do
      (blockTxs, eb) <- genRBandEB
      wrongHash <- arbitrary
      maxEbRefsSize <- integralToBounded @Int @Word32 $ ebRefsSize eb - 1
      let announcedSize = ebReferencesAnnouncementSize (mkEbReferencesAnnouncement eb) + 1
      _ <-
        submitAnnouncingRB blockTxs $
          mkEbReferencesAnnouncement eb
            & ebReferencesAnnouncementHashL .~ wrongHash
            & ebReferencesAnnouncementSizeL .~ announcedSize
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ maxEbRefsSize
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
    it "Enforces the transactions size limit of an EB" $ do
      eb <- makeAnnouncedSimpleEb
      let txsSizes = originalBytesSize32 <$> endorsedTxs eb
          runningTxsSizes = scanl1 (+) txsSizes
          exactLimit = sum txsSizes
      subLimit <- choose (1, exactLimit - 1)
      firstExceeding : _ <- pure $ dropWhile (<= subLimit) runningTxsSizes
      expectEbLimit eb ppMaxEndorserBlockTxsSizeL exactLimit subLimit $
        txsSizeTooBig firstExceeding subLimit
    it "Enforces the execution units limit of an EB on memory and on steps" $ do
      let alwaysSucceedsHash = hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV3
      nEbTxs <- choose (2, 5)
      txIns <- replicateM nEbTxs $ produceScript alwaysSucceedsHash
      eb <- makeAnnouncedEbFrom $ forM_ txIns $ \txIn ->
        submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
      let ebExUnits@(ExUnits mem steps) = foldMap getTotalExUnits $ endorsedTxs eb
          exactLimit = OrdExUnits ebExUnits
      forM_ @[] [ExUnits (mem + 1) (steps - 1), ExUnits (mem - 1) (steps + 1)] $ \exUnits -> do
        let insufficientLimit = OrdExUnits exUnits
        expectEbLimit eb ppMaxEndorserBlockExUnitsL exactLimit insufficientLimit $
          injectEbFailure $
            EbExUnitsTooBig
              Mismatch {mismatchSupplied = exactLimit, mismatchExpected = insufficientLimit}
    it "Enforces the reference scripts size limit of an EB" $ do
      script <- RequireSignature @era <$> freshKeyHash
      refScriptTxIn <- produceRefScript $ fromNativeScript script
      nEbTxs <- choose (2, 10)
      eb <-
        makeAnnouncedEbFrom . replicateM_ nEbTxs . submitTx_ $
          mkBasicTx mkBasicTxBody & bodyTxL . referenceInputsTxBodyL .~ [refScriptTxIn]
      let refScriptsSize = nEbTxs * originalBytesSize script
      exactLimit <- integralToBounded @Int @Word32 refScriptsSize
      expectEbLimit eb ppMaxRefScriptSizePerEndorserBlockL exactLimit (exactLimit - 1) $
        injectEbFailure $
          EbRefScriptsSizeTooBig
            Mismatch {mismatchSupplied = refScriptsSize, mismatchExpected = refScriptsSize - 1}
    it "Stops at the first transaction that exceeds a limit of an EB, only with validation" $ do
      eb <- makeAnnouncedSimpleEb
      firstTx : _ <- pure $ endorsedTxs eb
      firstTxSize <- integralToBounded @Int @Word32 $ originalBytesSize firstTx
      allTxsUTxO <- (^. utxoG) <$> applyEbWithoutValidation eb
      modifyPParams $ ppMaxEndorserBlockTxsSizeL .~ 0
      impAnn "Submit EB" $ submitFailingEndorserBlock eb [txsSizeTooBig firstTxSize 0]
      zeroLimitUTxO <- (^. utxoG) <$> applyEbWithoutValidation eb
      impAnn "Apply EB without validation, ignoring the limit" $
        zeroLimitUTxO `shouldBeExpr` allTxsUTxO
