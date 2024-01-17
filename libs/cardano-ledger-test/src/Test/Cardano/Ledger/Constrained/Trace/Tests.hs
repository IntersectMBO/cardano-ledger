{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Trace.Tests
where

import Cardano.Ledger.Babbage.TxOut ()
import Cardano.Ledger.BaseTypes (TxIx)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraRule, EraTx (..), EraTxBody (..), Tx)
import Cardano.Ledger.Plutus.Data ()
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Control.State.Transition.Extended (STS (..), TRC (..))
import Data.Foldable (toList)
import Lens.Micro ((^.))
import System.IO (hSetEncoding, stdout, utf8)
import Test.Cardano.Ledger.Constrained.Ast (runTarget)
import Test.Cardano.Ledger.Constrained.Classes (TxF (..), TxOutF (..))
import Test.Cardano.Ledger.Constrained.Env (Env (..), emptyEnv)
import Test.Cardano.Ledger.Constrained.Monad (Typed)
import Test.Cardano.Ledger.Constrained.Preds.Repl (goRepl)
import Test.Cardano.Ledger.Constrained.Trace.Actions (feesAction, inputsAction, outputsAction)
import Test.Cardano.Ledger.Constrained.Trace.DrepCertTx (drepTree)
import Test.Cardano.Ledger.Constrained.Trace.SimpleTx (getSTSLedgerEnv, simpleTx)
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad (
  TraceM,
  TraceStep (..),
  beforeAfterTrace,
  fstTriple,
  genLedgerStateEnv,
  getEnv,
  getTarget,
  liftGen,
  liftTyped,
  runTraceM,
  setVar,
  toGen,
 )
import Test.Cardano.Ledger.Constrained.Vars hiding (drepDeposit)
import Test.Cardano.Ledger.Generic.PrettyCore (pcTx)
import Test.Cardano.Ledger.Generic.Proof hiding (LEDGER, lift)
import Test.Cardano.Ledger.Generic.TxGen (applySTSByProof)
import Test.QuickCheck (Arbitrary (..), Property, conjoin, counterexample, whenFail, withMaxSuccess, (===))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- =======================================================================
-- Test that simpleTx and the 'actions' actually agree with the applySTS

-- | Construct and run one simpleTx, and run it through applySTS
--  Check that the computed LedgerState is the same as the expected LedgerState
--  Computed by using 'inputsAction' , 'outputsAction' , and 'feesAction'
genAndRunSimpleTx :: TraceM (ConwayEra StandardCrypto) Property
genAndRunSimpleTx = do
  let proof = Conway
  _ <- genLedgerStateEnv proof

  -- Compute the TRC before we make the Tx, because that adds things to the Env
  txIx <- liftGen arbitrary
  env0 <- getEnv
  (lenv, ledgerstate) <- liftTyped $ getSTSLedgerEnv proof txIx env0

  -- Now generate a simpleTx, and store it in the Env
  -- apply the changes we expect this Tx to make, and save the result.
  tx <- simpleTx proof (Coin 70000)
  setVar txterm (TxF proof tx)
  let txb = tx ^. bodyTxL
      feeCoin = txb ^. feeTxBodyL
  inputsAction proof (txb ^. inputsTxBodyL)
  outputsAction proof txb (fmap (TxOutF proof) (toList (txb ^. outputsTxBodyL)))
  feesAction feeCoin
  expectedLedgerState <- getTarget (ledgerStateT proof)
  env1 <- getEnv
  -- Now compute the env we compute using the STS
  pure $ ledgerStateEqProp proof env1 expectedLedgerState lenv ledgerstate tx

-- | Create a Property by testing that applying the STS "LEDGER" rule, succeeds and
--   returns the expected LedgerState. If it fails, print out the failures and
--   drop into the Repl, so that users can explore the inputs.
ledgerStateEqProp ::
  ( Signal (EraRule "LEDGER" era) ~ Tx era
  , Reflect era
  , Show (State (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Eq (State (EraRule "LEDGER" era))
  ) =>
  Proof era ->
  Env era ->
  State (EraRule "LEDGER" era) ->
  Environment (EraRule "LEDGER" era) ->
  State (EraRule "LEDGER" era) ->
  Signal (EraRule "LEDGER" era) ->
  Property
ledgerStateEqProp proof env1 expectedLedgerState ledgerenv ledgerstate tx =
  case applySTSByProof proof (TRC (ledgerenv, ledgerstate, tx)) of
    Right ledgerState' ->
      ledgerState' === {- `ediffEq` -} expectedLedgerState
    Left errs ->
      let errsLines = "" : "applySTS fails" : map show errs
       in counterexample
            (unlines (errsLines ++ ["Tx =", show (pcTx proof tx)]))
            ( whenFail
                (putStrLn (unlines errsLines) >> goRepl proof env1 "")
                False
            )

main1 :: IO ()
main1 = do
  hSetEncoding stdout utf8
  defaultMain $ testProperty "TraceMain" (withMaxSuccess 50 (toGen genAndRunSimpleTx))

-- =================================================================
-- Show that each step in a trace computes the right LedgerState

runOne ::
  ( Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Reflect era
  ) =>
  Proof era ->
  TxIx ->
  TraceStep era (Signal (EraRule "LEDGER" era)) ->
  Typed Property
runOne proof txIx (TraceStep beforeEnv afterEnv tx) = do
  (lenv, ledgerstate) <- getSTSLedgerEnv proof txIx beforeEnv
  expectedLedgerState <- runTarget afterEnv (ledgerStateT proof)
  pure $ ledgerStateEqProp proof afterEnv expectedLedgerState lenv ledgerstate tx

oneTx :: Reflect era => Proof era -> Int -> TraceM era (Tx era)
oneTx proof _n = do
  !tx <- simpleTx proof (Coin 70000)
  setVar txterm (TxF proof tx)
  let !txb = tx ^. bodyTxL
      !feeCoin = txb ^. feeTxBodyL
  inputsAction proof (txb ^. inputsTxBodyL)
  outputsAction proof txb (fmap (TxOutF proof) (toList (txb ^. outputsTxBodyL)))
  feesAction feeCoin
  pure tx

testTrace ::
  ( Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , Reflect era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Proof era ->
  Int ->
  TraceM era Property
testTrace proof tracelen = do
  !_ <- genLedgerStateEnv proof
  !txIx <- liftGen arbitrary
  !pairs <- beforeAfterTrace tracelen (oneTx proof)
  conjoin <$> mapM (\ !x -> liftTyped (runOne proof txIx x)) pairs

conwayTrace :: TestTree
conwayTrace =
  testProperty
    ("Testing each Tx in a Conway trace of length=" ++ show tracelen ++ " passes applySTS.")
    (withMaxSuccess n (fstTriple <$> (runTraceM 0 emptyEnv (testTrace Conway tracelen))))
  where
    tracelen = 100
    n = 10

conwayTxwithDRepCertsTraceTests :: TestTree
conwayTxwithDRepCertsTraceTests =
  testGroup
    "Tx with DRep Certs trace tests"
    [ conwayTrace
    , drepTree
    ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  defaultMain conwayTxwithDRepCertsTraceTests
