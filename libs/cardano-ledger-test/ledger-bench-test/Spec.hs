{-# LANGUAGE NamedFieldPuns #-}

-- | Test suite for the ledger-bench application.
--
-- Verifies that:
--
--  1. The generator produces a valid Conway ledger environment (state + transaction).
--  2. 'applyTx' accepts the generated transaction without error.
--  3. The generated transaction has a non-zero serialised size.
--  4. 'measureThroughput' runs without error and returns sensible values.
module Main where

import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Data.ByteString.Lazy as BL
import Cardano.Ledger.Shelley.API (applyTx)
import Control.Exception (evaluate)
import LedgerBench.Conway (ApplyTxEnv (..), generateConwayApplyTxEnv)
import LedgerBench.SizedConway (SizedTx (..), generateSizedConwayApplyTxEnv)
import LedgerBench.Throughput (ThroughputResult (..), measureThroughput)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "ledger-bench"
    [ generatorTests
    , applyTxTests
    , throughputTests
    ]

-- | Check that Conway environments can be generated successfully.
generatorTests :: TestTree
generatorTests =
  testGroup
    "Environment generation"
    [ testCase "Small generates non-empty Conway transaction" $
        checkGenSized Small
    , testCase "Medium generates non-empty Conway transaction" $
        checkGenSized Medium
    , testCase "Large generates non-empty Conway transaction" $
        checkGenSized Large
    , testCase "Random generates non-empty Conway transaction" $ do
        let ApplyTxEnv {ateTx} = generateConwayApplyTxEnv 24601 0
            txSize = BL.length (Plain.serialize ateTx)
        assertBool "serialised transaction must be non-empty" (txSize > 0)
    ]

checkGenSized :: SizedTx -> Assertion
checkGenSized size =
  let ApplyTxEnv {ateTx} = generateSizedConwayApplyTxEnv size 24601 0
      txSize = BL.length (Plain.serialize ateTx)
   in assertBool "serialised transaction must be non-empty" (txSize > 0)

-- | Check that 'applyTx' accepts the generated Conway transactions.
applyTxTests :: TestTree
applyTxTests =
  testGroup
    "applyTx validation"
    [ testCase "Small Conway transaction passes applyTx" $
        checkApplyTxSized Small
    , testCase "Medium Conway transaction passes applyTx" $
        checkApplyTxSized Medium
    , testCase "Large Conway transaction passes applyTx" $
        checkApplyTxSized Large
    , testCase "Random Conway transaction passes applyTx" $ do
        let ApplyTxEnv {ateGlobals, ateMempoolEnv, ateState, ateTx} =
              generateConwayApplyTxEnv 24601 0
            result = applyTx ateGlobals ateMempoolEnv ateState ateTx
        case result of
          Left err ->
            assertFailure $
              "applyTx rejected a randomly generated Conway transaction: " <> show err
          Right _ -> pure ()
    ]

checkApplyTxSized :: SizedTx -> Assertion
checkApplyTxSized size =
  let ApplyTxEnv {ateGlobals, ateMempoolEnv, ateState, ateTx} =
        generateSizedConwayApplyTxEnv size 24601 0
      result = applyTx ateGlobals ateMempoolEnv ateState ateTx
   in case result of
        Left err ->
          assertFailure $
            "applyTx rejected a " <> show size <> " Conway transaction: " <> show err
        Right _ -> pure ()

-- | Check that 'measureThroughput' produces sensible numeric output.
throughputTests :: TestTree
throughputTests =
  testGroup
    "measureThroughput"
    [ testCase "returns non-negative latency and throughput" $ do
        result <- measureThroughput "test" 400 10 (evaluate $! (42 :: Int) `seq` ())
        assertBool "median latency must be >= 0" (trMedianLatencySec result >= 0)
        assertBool "throughput must be > 0" (trThroughputKBs result > 0)
    , testCase "Conway/Small applyTx reports positive throughput" $ do
        let ApplyTxEnv {ateGlobals, ateMempoolEnv, ateState, ateTx} =
              generateSizedConwayApplyTxEnv Small 24601 0
            txBytes = fromIntegral $ BL.length (Plain.serialize ateTx)
            action = do
              _ <-
                evaluate $! either (error . show) fst $
                  applyTx ateGlobals ateMempoolEnv ateState ateTx
              pure ()
        result <- measureThroughput "Conway/Small" txBytes 10 action
        assertBool "throughput must be positive" (trThroughputKBs result > 0)
        assertBool "median latency must be positive" (trMedianLatencySec result > 0)
    ]
