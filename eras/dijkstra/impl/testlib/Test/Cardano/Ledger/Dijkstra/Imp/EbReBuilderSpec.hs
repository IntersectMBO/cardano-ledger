{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EbReBuilderSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (
  Block,
  EbReferencesAnnouncement (..),
  LeiosEraBlockHeader (..),
  ebReferencesAnnouncementHashL,
  ebReferencesAnnouncementSizeL,
  slotNoBlockHeaderL,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra (DijkstraApplyBlock (..))
import Cardano.Ledger.Dijkstra.Block.Endorser (EbTransactions (..), EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Block.Endorser.ReBuilder (
  EbReBuilderError (..),
  EbReBuilderFinalizerError (..),
  EbReBuilderInitError (..),
  EbReBuilderResult (..),
  EbReBuilderState,
  EraEndorserBlockReBuilder (..),
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (
  ppMaxEndorserBlockReferencesSizeL,
  ppMaxEndorserBlockTxsSizeL,
 )
import Cardano.Ledger.Dijkstra.Rules (
  EbAlreadyApplied (..),
  EbAnnouncementHashMismatch (..),
  EbAnnouncementSizeMismatch (..),
  EbAnnouncementSlotMismatch (..),
  EbReferencesSizeTooBig (..),
  EbTxsSizeTooBig (..),
  EbWasNotAnnounced (..),
 )
import Cardano.Ledger.Shelley.LedgerState (nesLastTickSlotNoL)
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Control.Monad (foldM)
import Control.Monad.State.Strict (gets)
import Control.State.Transition.Extended (SingEP (..), ValidationPolicy (..))
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.SimpleOMap.Strict as OMap
import Data.Word (Word32)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.BlockHeader (TestBlockHeader)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( DijkstraEraImp era
  , EraEndorserBlockReBuilder era
  , HashAnnotated (Tx TopTx era) EraIndependentTx
  , Eq (EndorserBlock era)
  , Show (EndorserBlock era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "EB re-builder" $ do
  let
    -- \| A transaction that only pays a fresh address, so that every one of them is distinct.
    submitSimpleTx :: ImpTestM era ()
    submitSimpleTx = do
      pp <- getsPParams id
      addr <- freshKeyAddr_
      amnt <- inject . Coin <$> choose (100, 1000000000)
      void . submitTx $
        mkBasicTx @era mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ [ensureMinCoinTxOut pp $ mkBasicTxOut addr amnt]

    -- Builds an endorser block out of @n@ freshly submitted transactions, leaving the
    -- ledger state exactly as it was before those transactions were submitted.
    makeEb :: Int -> ImpTestM era (EndorserBlock era)
    makeEb n =
      simulateThenRestore $
        impRecordSubmittedTxs (replicateM_ n submitSimpleTx) >>= makeEndorserBlock

    -- Announces an endorser block with an announcement that need not be the correct one.
    announceWith :: EbReferencesAnnouncement -> ImpTestM era (Block TestBlockHeader era)
    announceWith announcement = do
      tickToCurSlot
      withTxsInModifiedBlock
        (ebReferencesAnnouncementBlockHeaderL .~ SJust announcement)
        (pure ())

    makeAnnouncedEb :: Int -> ImpTestM era (EndorserBlock era)
    makeAnnouncedEb n = do
      eb <- makeEb n
      eb <$ announceWith (mkEbReferencesAnnouncement eb)

    endorsedTxs eb = toList . unEbTransactions $ eb ^. transactionsEndorserBlockL

    ebRefs eb = eb ^. referencesEndorserBlockL
    ebRefsHash = hashAnnotated . ebRefs
    ebRefsSize = originalBytesSize . ebRefs

    -- The total of the sizes the references declare, which is what init checks against
    -- `ppMaxEndorserBlockTxsSize`.
    declaredTxsSize :: EndorserBlock era -> Word32
    declaredTxsSize = sum . OMap.toStrictSeq . (^. txRefsEbReferencesG) . ebRefs

    runInit eb = do
      nes <- getsNES id
      pure $ mkInitEbReBuilderState nes (ebRefs eb)

    initFor eb = runInit eb >>= either (assertFailure . show) pure

    initFails :: EndorserBlock era -> EbReBuilderInitError era -> ImpTestM era ()
    initFails eb expected =
      runInit eb >>= \case
        Left err -> err `shouldBe` expected
        Right _ -> assertFailure $ "Expected initialisation to fail with " <> show expected

    snocOk :: EbReBuilderState era -> Tx TopTx era -> ImpTestM era (EbReBuilderState era)
    snocOk st tx = do
      globals <- gets (^. impGlobalsL)
      case snocTxEbReBuilder globals st (Right tx) of
        EbReBuilderSuccess st' _ -> pure st'
        EbReBuilderError e _ -> assertFailure $ "Unexpected re-builder error: " <> show e
        EbReBuilderTxError txIx _ -> assertFailure $ "Unexpected LEDGER failure at " <> show txIx

    snocFails :: EbReBuilderState era -> Tx TopTx era -> ImpTestM era (EbReBuilderError era)
    snocFails st tx = do
      globals <- gets (^. impGlobalsL)
      case snocTxEbReBuilder globals st (Right tx) of
        EbReBuilderError e _ -> pure e
        EbReBuilderSuccess _ _ -> assertFailure "Expected a re-builder error, got success"
        EbReBuilderTxError txIx _ ->
          assertFailure $ "Expected a re-builder error, got a LEDGER failure at " <> show txIx

  it "Re-builds an announced EB into the same block and state as the EB rule" $ do
    eb <- makeAnnouncedEb 5
    globals <- gets (^. impGlobalsL)
    nes <- getsNES id
    let (ruleNes, failures, _) =
          applyEndorserBlock @TestBlockHeader @era EPDiscard ValidateAll globals nes eb
    failures `shouldBeExpr` []

    initState <- initFor eb
    finalState <- foldM snocOk initState (endorsedTxs eb)
    (rebuiltEb, rebuiltNes) <- either (assertFailure . show) pure $ finalizeEbReBuilder finalState
    rebuiltEb `shouldBe` eb
    rebuiltNes `shouldBeExpr` ruleNes

  it "Rejects a transaction supplied out of order" $ do
    eb <- makeAnnouncedEb 2
    initState <- initFor eb
    case endorsedTxs eb of
      [tx0, tx1] -> do
        err <- snocFails initState tx1
        err
          `shouldBe` EbReBuilderTxHashMismatch
            minBound
            Mismatch
              { mismatchSupplied = hashAnnotated tx1
              , mismatchExpected = hashAnnotated tx0
              }
      txs -> assertFailure $ "Expected exactly 2 transactions, got " <> show (length txs)

  it "Leaves the state untouched after a failed snoc, so the position can be retried" $ do
    eb <- makeAnnouncedEb 2
    initState <- initFor eb
    case endorsedTxs eb of
      [tx0, tx1] -> do
        _ <- snocFails initState tx1
        afterRetry <- snocOk initState tx0 >>= \st -> snocOk st tx1
        (rebuiltEb, _) <- either (assertFailure . show) pure $ finalizeEbReBuilder afterRetry
        rebuiltEb `shouldBe` eb
      txs -> assertFailure $ "Expected exactly 2 transactions, got " <> show (length txs)

  it "Rejects more transactions than the references name" $ do
    eb <- makeAnnouncedEb 2
    initState <- initFor eb
    let txs = endorsedTxs eb
    fullState <- foldM snocOk initState txs
    err <- snocFails fullState (head txs)
    err `shouldBe` EbReBuilderNoSuchReference (length txs)

  it "Reports every missing transaction, in reference order" $ do
    eb <- makeAnnouncedEb 3
    initState <- initFor eb
    let txs = endorsedTxs eb
    -- Supply only the first transaction; the other two stay unresolved.
    partialState <- snocOk initState (head txs)
    case finalizeEbReBuilder partialState of
      Right _ -> assertFailure "Expected finalisation to report the missing transactions"
      Left err ->
        err
          `shouldBe` EbReBuilderMissingTxsError (NE.fromList (hashAnnotated <$> drop 1 txs))

  describe "Initialisation" $ do
    it "Fails when the EB was not announced" $ do
      eb <- makeEb 2
      initFails eb . EbReBuilderInitNoAnnouncement . EbWasNotAnnounced $ ebRefsHash eb

    it "Fails when the announcement has a different hash" $ do
      eb <- makeEb 2
      wrongHash <- arbitrary
      _ <- announceWith $ mkEbReferencesAnnouncement eb & ebReferencesAnnouncementHashL .~ wrongHash
      initFails eb . EbReBuilderInitEbAnnouncementHashMismatch $
        EbAnnouncementHashMismatch
          Mismatch {mismatchSupplied = ebRefsHash eb, mismatchExpected = wrongHash}

    it "Fails when the announcement has a different size" $ do
      eb <- makeEb 2
      let announcement = mkEbReferencesAnnouncement eb
          wrongSize = ebReferencesAnnouncementSize announcement + 1
      _ <- announceWith $ announcement & ebReferencesAnnouncementSizeL .~ wrongSize
      initFails eb . EbReBuilderInitEbAnnouncementSizeMismatch $
        EbAnnouncementSizeMismatch
          Mismatch
            { mismatchSupplied = ebRefsSize eb
            , mismatchExpected = fromIntegral @Word32 @Int wrongSize
            }

    it "Fails when initialised at a slot after the announcing block" $ do
      eb <- makeEb 2
      block <- announceWith $ mkEbReferencesAnnouncement eb
      passTick
      tickToCurSlot
      laterSlot <- getsNES nesLastTickSlotNoL
      initFails eb . EbReBuilderInitEbAnnouncementSlotMismatch $
        EbAnnouncementSlotMismatch
          Mismatch {mismatchSupplied = laterSlot, mismatchExpected = block ^. slotNoBlockHeaderL}

    it "Fails when the EB has already been applied" $ do
      eb <- makeAnnouncedEb 2
      impAnn "Submit EB" $ submitEndorserBlock eb
      initFails eb . EbReBuilderInitEbAlreadyApplied . EbAlreadyApplied $ ebRefsHash eb

    it "Fails when the references exceed the size limit" $ do
      eb <- makeAnnouncedEb 2
      maxEbRefsSize <- integralToBounded @Int @Word32 $ ebRefsSize eb - 1
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ maxEbRefsSize
      initFails eb . EbReBuilderInitEbReferencesSizeTooBig $
        EbReferencesSizeTooBig
          Mismatch
            { mismatchSupplied = ebRefsSize eb
            , mismatchExpected = fromIntegral @Word32 @Int maxEbRefsSize
            }

    it "Fails when the declared transaction sizes exceed the limit" $ do
      eb <- makeAnnouncedEb 2
      let declared = declaredTxsSize eb
      modifyPParams $ ppMaxEndorserBlockTxsSizeL .~ declared - 1
      initFails eb . EbReBuilderInitEbTxsSizeTooBig $
        EbTxsSizeTooBig
          Mismatch {mismatchSupplied = declared, mismatchExpected = declared - 1}

    it "Succeeds on a correctly announced EB" $ do
      eb <- makeAnnouncedEb 3
      _ <- initFor eb
      pure ()
