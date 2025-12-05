{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.Imp.MempoolSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra ()
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraMempoolPredFailure (AllInputsAreSpent, LedgerFailure),
  DijkstraUtxoPredFailure (BadInputsUTxO),
  PredicateFailure,
 )
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (ApplyTxError), applyTx, mkMempoolEnv)
import Cardano.Ledger.Shelley.LedgerState
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Exts (fromList)
import Lens.Micro ((&), (<>~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( DijkstraEraImp era
  , PredicateFailure (EraRule "MEMPOOL" era) ~ DijkstraMempoolPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure era
  , Inject (NonEmpty (DijkstraMempoolPredFailure era)) (ApplyTxError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec =
  describe "Mempool" $ do
    let
      submitFailingMempoolTx ::
        String ->
        Tx TopTx era ->
        NonEmpty (PredicateFailure (EraRule "MEMPOOL" era)) ->
        ImpM (LedgerSpec era) (Tx TopTx era)
      submitFailingMempoolTx cause tx expectedFailures = do
        globals <- use impGlobalsL
        nes <- use impNESL
        slotNo <- use impCurSlotNoG
        let
          mempoolEnv = mkMempoolEnv nes slotNo
          ls = nes ^. nesEsL . esLStateL
        txFixed <- (tx &) =<< asks iteFixup
        logToExpr txFixed
        case applyTx globals mempoolEnv ls txFixed of
          Left err -> do
            err `shouldBe` inject expectedFailures
          Right _ ->
            assertFailure $ "Expected failure due to " <> cause <> ": " <> show txFixed
        pure txFixed

      submitFailingMempoolTx_ ::
        String ->
        Tx TopTx era ->
        NonEmpty (PredicateFailure (EraRule "MEMPOOL" era)) ->
        ImpM (LedgerSpec era) ()
      submitFailingMempoolTx_ c t = void . submitFailingMempoolTx c t

    it "Duplicate transactions" $ do
      let
        newInput = do
          addr <- freshKeyAddr_
          amount <- Coin <$> choose (2_000_000, 8_000_000)
          sendCoinTo addr amount

      inputsCommon <- replicateM 5 newInput
      inputs1 <- replicateM 2 newInput
      inputs2 <- replicateM 3 newInput

      txFinal <-
        submitTx $
          mkBasicTx $
            mkBasicTxBody & inputsTxBodyL <>~ fromList (inputsCommon <> inputs1)

      impAnn "Identical transaction" $ do
        withNoFixup $
          submitFailingMempoolTx_ "duplicate transaction" txFinal $
            NonEmpty.singleton $
              AllInputsAreSpent

      impAnn "Overlapping transaction" $ do
        let txOverlap = mkBasicTx $ mkBasicTxBody & inputsTxBodyL <>~ fromList (inputsCommon <> inputs2)
        submitFailingMempoolTx_
          "overlapping transaction"
          txOverlap
          [LedgerFailure $ injectFailure $ BadInputsUTxO $ fromList inputsCommon]
