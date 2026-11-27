{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
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
import Cardano.Ledger.Shelley.LedgerState (nesLastTickSlotNoL)
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
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
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

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
    makeEbFrom :: ImpTestM era () -> ImpTestM era (EndorserBlock era)
    makeEbFrom txsAction =
      simulateThenRestore $ impRecordSubmittedTxs txsAction >>= makeEndorserBlock

    makeEb :: Int -> ImpTestM era (EndorserBlock era)
    makeEb n = makeEbFrom (replicateM_ n submitSimpleTx)

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

    -- Snocs transactions in order and returns the first re-builder error.
    firstSnocError :: EbReBuilderState era -> [Tx TopTx era] -> ImpTestM era (EbReBuilderError era)
    firstSnocError _ [] =
      assertFailure "Expected a re-builder error, but every transaction was accepted"
    firstSnocError st (tx : rest) = do
      globals <- gets (^. impGlobalsL)
      case snocTxEbReBuilder globals st (Right tx) of
        EbReBuilderSuccess st' _ -> firstSnocError st' rest
        EbReBuilderError e _ -> pure e
        EbReBuilderTxError txIx _ -> assertFailure $ "Unexpected LEDGER failure at " <> show txIx

    -- Expects the transaction to be rejected by the LEDGER rule, and returns its position.
    snocTxFails :: EbReBuilderState era -> Tx TopTx era -> ImpTestM era TxIx
    snocTxFails st tx = do
      globals <- gets (^. impGlobalsL)
      case snocTxEbReBuilder globals st (Right tx) of
        EbReBuilderTxError txIx _ -> pure txIx
        EbReBuilderSuccess _ _ -> assertFailure "Expected a LEDGER failure, got success"
        EbReBuilderError e _ -> assertFailure $ "Expected a LEDGER failure, got " <> show e

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

  describe "Transaction failures" $ do
    it "Rejects a transaction whose declared size is wrong" $ do
      eb <- makeEb 2
      -- Announce references that over-declare the size of the first transaction.
      let refs = eb ^. referencesEndorserBlockL . txRefsEbReferencesG
          bumped =
            OMap.fromFoldable $
              zipWith
                (\i (txHash, size) -> (txHash, if i == (0 :: Int) then size + 1 else size))
                [0 ..]
                (OMap.assocList refs)
          ebWrongSize = eb & referencesEndorserBlockL .~ mkEbReferences bumped
      _ <- announceWith $ mkEbReferencesAnnouncement ebWrongSize
      st <- initFor ebWrongSize
      firstTx : _ <- pure $ endorsedTxs eb
      let actualSize = originalBytesSize32 firstTx
      err <- snocFails st firstTx
      err
        `shouldBe` EbReBuilderTxSizeMismatch
          minBound
          Mismatch {mismatchSupplied = actualSize, mismatchExpected = actualSize + 1}

    it "Reports a transaction that the LEDGER rule rejects" $ do
      eb <- makeAnnouncedEb 2
      -- Raising the minimum fee leaves hashes and sizes intact, but makes every fee too small.
      modifyPParams $ ppTxFeePerByteL .~ CoinPerByte (CompactCoin 1000000)
      st <- initFor eb
      firstTx : _ <- pure $ endorsedTxs eb
      snocTxFails st firstTx `shouldReturn` minBound

    it "Rejects the transaction that pushes execution units over the limit" $ do
      let alwaysSucceedsHash = hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV3
      nEbTxs <- choose (2, 5)
      txIns <- replicateM nEbTxs $ produceScript alwaysSucceedsHash
      eb <- makeEbFrom $ forM_ txIns $ \txIn ->
        submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
      _ <- announceWith $ mkEbReferencesAnnouncement eb
      let ebExUnits@(ExUnits mem steps) = foldMap getTotalExUnits $ endorsedTxs eb
          insufficientLimit = OrdExUnits $ ExUnits (mem - 1) steps
      modifyPParams $ ppMaxEndorserBlockExUnitsL .~ insufficientLimit
      st <- initFor eb
      err <- firstSnocError st (endorsedTxs eb)
      err
        `shouldBe` EbReBuilderExUnitsTooBig
          ( EbExUnitsTooBig
              Mismatch
                { mismatchSupplied = OrdExUnits ebExUnits
                , mismatchExpected = insufficientLimit
                }
          )

    it "Rejects the transaction that pushes the reference scripts size over the limit" $ do
      script <- RequireSignature @era <$> freshKeyHash
      refScriptTxIn <- produceRefScript $ fromNativeScript script
      nEbTxs <- choose (2, 10)
      eb <-
        makeEbFrom . replicateM_ nEbTxs . submitTx_ $
          mkBasicTx mkBasicTxBody & bodyTxL . referenceInputsTxBodyL .~ [refScriptTxIn]
      _ <- announceWith $ mkEbReferencesAnnouncement eb
      let refScriptsSize = nEbTxs * originalBytesSize script
      exactLimit <- integralToBounded @Int @Word32 refScriptsSize
      modifyPParams $ ppMaxRefScriptSizePerEndorserBlockL .~ exactLimit - 1
      st <- initFor eb
      err <- firstSnocError st (endorsedTxs eb)
      err
        `shouldBe` EbReBuilderRefScriptsSizeTooBig
          ( EbRefScriptsSizeTooBig
              Mismatch
                { mismatchSupplied = refScriptsSize
                , mismatchExpected = refScriptsSize - 1
                }
          )
