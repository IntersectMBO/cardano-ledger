{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EbReBuilderSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra (DijkstraApplyBlock (..))
import Cardano.Ledger.Dijkstra.Block.Endorser (EbTransactions (..), EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.Block.Endorser.ReBuilder (
  EbReBuilderError (..),
  EbReBuilderFinalizerError (..),
  EbReBuilderResult (..),
  EbReBuilderState,
  EraEndorserBlockReBuilder (..),
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Control.Monad (foldM)
import Control.Monad.State.Strict (gets)
import Control.State.Transition.Extended (SingEP (..), ValidationPolicy (..))
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
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

    -- \| Builds an endorser block out of @n@ freshly submitted transactions and announces it,
    -- leaving the ledger state exactly as it was before those transactions were submitted.
    makeAnnouncedEb :: Int -> ImpTestM era (EndorserBlock era)
    makeAnnouncedEb n = do
      eb <-
        simulateThenRestore $
          impRecordSubmittedTxs (replicateM_ n submitSimpleTx) >>= makeEndorserBlock
      eb <$ announceEndorserBlock eb

    endorsedTxs eb = toList . unEbTransactions $ eb ^. transactionsEndorserBlockL

    initFor eb = do
      nes <- getsNES id
      either (assertFailure . show) pure $
        mkInitEbReBuilderState nes (eb ^. referencesEndorserBlockL)

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
