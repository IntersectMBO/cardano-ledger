{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EbBuilderSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra
import Cardano.Ledger.Dijkstra.Block.Endorser
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Shelley.API.Mempool
import Cardano.Ledger.Shelley.Rules
import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import qualified Data.Sequence.Strict as SSeq
import Data.Word
import Lens.Micro
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( DijkstraEraImp era
  , HashAnnotated (Tx TopTx era) EraIndependentTx
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "EB builder" $ do
  let
    simpleTx txIn = mkBasicTx @era $ mkBasicTxBody & inputsTxBodyL .~ [txIn]

    -- Validate the transactions in the mempool one after the other, each on top of the ones
    -- before it.
    mkValidatedTxs n = do
      txIns <- replicateM n freshFundedTxIn
      simulateThenRestore . forM txIns $ \txIn -> do
        tx <- fixupTx $ simpleTx txIn
        withNoFixup $ do
          (_, validatedTx) <- expectRight =<< trySubmitMempoolTx tx
          validatedTx <$ submitTx_ tx

    -- A transaction that only fails the static checks
    staticallyInvalidTx = do
      txIn <- freshFundedTxIn
      fixupTx $ simpleTx txIn & bodyTxL . networkIdTxBodyL .~ SJust Mainnet

    unsafeValidateTxWithProtVer protVer tx = do
      globals <- gets (^. impGlobalsL)
      nes <- getsNES id
      curSlotNo <- gets (^. impCurSlotNoG)
      let mempoolEnv = mkMempoolEnv nes curSlotNo & ledgerPpL . ppProtocolVersionL .~ protVer
      pure $ unsafeMakeValidatedTx globals mempoolEnv (mkMempoolState nes) tx

    snocTx ebBuilderState validatedTx = do
      globals <- gets (^. impGlobalsL)
      pure $ snocTxEbBuilderValidated globals ebBuilderState validatedTx

    expectEbBuilderSuccess = \case
      EbBuilderSuccess ebBuilderState -> pure ebBuilderState
      EbBuilderLimitExceeded failures ->
        assertFailure $ "Expected success, got EB failures:\n" <> showExpr failures
      EbBuilderTxError err -> assertFailure $ "Expected success, got:\n" <> show err

    buildEb :: [ValidatedTx era] -> ImpTestM era (EbBuilderState era)
    buildEb validatedTxs = do
      ebBuilderState <- mkInitEbBuilderState <$> getsNES id
      foldM (\st -> snocTx st >=> expectEbBuilderSuccess) ebBuilderState validatedTxs

    expectEbBuilderLimitExceeded ebBuilderState validatedTx expected =
      snocTx ebBuilderState validatedTx >>= \case
        EbBuilderLimitExceeded failures -> failures `shouldBeExpr` expected
        EbBuilderSuccess _ -> assertFailure "Expected EB failures, got success"
        EbBuilderTxError err -> assertFailure $ "Expected EB failures, got:\n" <> show err

    expectEbBuilderTxError ebBuilderState validatedTx expected =
      snocTx ebBuilderState validatedTx >>= \case
        EbBuilderTxError err -> err `shouldBe` expected
        EbBuilderSuccess _ -> assertFailure "Expected a transaction failure, got success"
        EbBuilderLimitExceeded failures ->
          assertFailure $ "Expected a transaction failure, got EB failures:\n" <> showExpr failures

    -- The finalized EB is the one built from the transactions directly and it applies once it
    -- has been announced.
    expectFinalizedEb ebBuilderState txs = do
      eb <- expectJust $ finalizeEbBuilder ebBuilderState
      expectedEb <- makeEndorserBlock $ SSeq.fromList txs
      eb ^. referencesEndorserBlockL `shouldBe` expectedEb ^. referencesEndorserBlockL
      toList (unEbTransactions $ eb ^. transactionsEndorserBlockL) `shouldBeExpr` txs
      _ <- announceEndorserBlock eb
      impAnn "Submit EB" $ submitEndorserBlock eb

    injectEbFailure :: InjectRuleFailure "EB" t era => t era -> EraRuleFailure "EB" era
    injectEbFailure = injectFailure

  it "Builds the EB of the added transactions" $ do
    validatedTxs <- choose (1, 10) >>= mkValidatedTxs
    ebBuilderState <- buildEb validatedTxs
    expectFinalizedEb ebBuilderState $ extractValidatedTx <$> validatedTxs

  it "Finalizes to no EB when no transaction was added" $ do
    nes <- getsNES id
    isNothing (finalizeEbBuilder $ mkInitEbBuilderState nes) `shouldBe` True

  describe "EbBuilderTxError" $ do
    it "AllInputsAreSpent by a transaction added before" $ do
      txIn <- freshFundedTxIn
      validatedTxs <- forM [SNothing, SJust Testnet] $ \network -> do
        tx <- fixupTx $ simpleTx txIn & bodyTxL . networkIdTxBodyL .~ network
        withNoFixup $ snd <$> (expectRight =<< trySubmitMempoolTx tx)
      validatedTx : conflictingValidatedTx : _ <- pure validatedTxs
      ebBuilderState <- buildEb [validatedTx]
      expectEbBuilderTxError ebBuilderState conflictingValidatedTx $
        inject @(NonEmpty (PredicateFailure (EraRule "MEMPOOL" era))) [injectFailure AllInputsAreSpent]
      expectFinalizedEb ebBuilderState [extractValidatedTx validatedTx]

    it "A statically invalid transaction validated under a different major protocol version" $ do
      tx <- staticallyInvalidTx
      err <- withNoFixup (trySubmitMempoolTx tx) >>= expectLeft . void
      protVer <- getProtVer
      otherMajor <- succVersion $ pvMajor protVer
      validatedTx <- unsafeValidateTxWithProtVer protVer {pvMajor = otherMajor} tx
      ebBuilderState <- mkInitEbBuilderState <$> getsNES id
      expectEbBuilderTxError ebBuilderState validatedTx err

  it "Skips the static checks of a transaction validated under the current protocol version" $ do
    tx <- staticallyInvalidTx
    validatedTx <- getProtVer >>= (`unsafeValidateTxWithProtVer` tx)
    void $ buildEb [validatedTx]

  describe "EbBuilderLimitExceeded" $ do
    let
      genLimitedTxs = do
        n <- choose (2, 10)
        k <- choose (1, n - 1)
        validatedTxs <- mkValidatedTxs n
        (addedValidatedTxs, nextValidatedTx : _) <- pure $ splitAt k validatedTxs
        pure (addedValidatedTxs, nextValidatedTx)

    it "EbTxsSizeTooBig" $ do
      (addedValidatedTxs, nextValidatedTx) <- genLimitedTxs
      let addedTxs = extractValidatedTx <$> addedValidatedTxs
          addedTxsSize = sum $ originalBytesSize <$> addedTxs
          txsSize = addedTxsSize + originalBytesSize (extractValidatedTx nextValidatedTx)
      maxTxsSize <- integralToBounded @Int @Word32 addedTxsSize
      modifyPParams $ ppMaxEndorserBlockTxsSizeL .~ maxTxsSize
      ebBuilderState <- buildEb addedValidatedTxs
      expectEbBuilderLimitExceeded
        ebBuilderState
        nextValidatedTx
        [ injectEbFailure $
            EbTxsSizeTooBig Mismatch {mismatchSupplied = txsSize, mismatchExpected = addedTxsSize}
        ]
      expectFinalizedEb ebBuilderState addedTxs

    it "EbReferencesSizeTooBig" $ do
      (addedValidatedTxs, nextValidatedTx) <- genLimitedTxs
      let addedTxs = extractValidatedTx <$> addedValidatedTxs
          ebRefsSize = fmap (originalBytesSize . (^. referencesEndorserBlockL)) . makeEndorserBlock
      addedRefsSize <- ebRefsSize $ SSeq.fromList addedTxs
      refsSize <- ebRefsSize . SSeq.fromList $ addedTxs <> [extractValidatedTx nextValidatedTx]
      maxRefsSize <- integralToBounded @Int @Word32 addedRefsSize
      modifyPParams $ ppMaxEndorserBlockReferencesSizeL .~ maxRefsSize
      ebBuilderState <- buildEb addedValidatedTxs
      expectEbBuilderLimitExceeded
        ebBuilderState
        nextValidatedTx
        [ injectEbFailure $
            EbReferencesSizeTooBig Mismatch {mismatchSupplied = refsSize, mismatchExpected = addedRefsSize}
        ]
      expectFinalizedEb ebBuilderState addedTxs
