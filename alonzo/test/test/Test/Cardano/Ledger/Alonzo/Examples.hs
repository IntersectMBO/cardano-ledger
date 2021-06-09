{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples where

import Data.ByteString.Short (ShortByteString, toShort)
import Data.Maybe (fromMaybe)
import Flat (flat)
import qualified Plutus.V1.Ledger.Api as P
  ( EvaluationError (..),
    ExBudget (..),
    ExCPU (..),
    ExMemory (..),
    VerboseMode (..),
    defaultCostModelParams,
    evaluateScriptRestricting,
  )
import Plutus.V1.Ledger.Examples
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
  )
import qualified Plutus.V1.Ledger.Scripts as P
import qualified PlutusTx as P
import qualified PlutusTx.Prelude as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- Tests running Plutus scripts directely

data ShouldSucceed = ShouldSucceed | ShouldFail

directPlutusTest :: ShouldSucceed -> ShortByteString -> [P.Data] -> Assertion
directPlutusTest expectation script ds =
  case (expectation, evalWithHugeBudget script ds) of
    (ShouldSucceed, (_, Left e)) ->
      assertBool ("This script should have succeeded, but: " <> show e) False
    (ShouldSucceed, (_, Right _)) ->
      assertBool "" True
    (ShouldFail, (_, Left ((P.CekError _)))) ->
      assertBool "" True -- TODO rule out cost model failure
    (ShouldFail, (_, Left e)) ->
      assertBool ("Not the script failure we expected: " <> show e) False
    (ShouldFail, (_, Right _)) ->
      assertBool "This script should have failed" False
  where
    costModel = fromMaybe (error "corrupt default cost model") P.defaultCostModelParams
    -- Evaluate a script with sufficient budget to run it.
    evalWithHugeBudget scr datums =
      P.evaluateScriptRestricting
        P.Verbose
        costModel
        (P.ExBudget (P.ExCPU 100000000) (P.ExMemory 10000000))
        scr
        datums

guessTheNumber' :: P.Data -> P.Data -> ()
guessTheNumber' d1 d2 = if d1 P.== d2 then () else (P.error ())

guessTheNumber :: ShortByteString
guessTheNumber =
  toShort . flat . P.fromCompiledCode $
    $$(P.compile [||guessTheNumber'||])

plutusScriptExamples :: TestTree
plutusScriptExamples =
  testGroup
    "run plutus script directly"
    [ testCase "always true" $
        directPlutusTest
          ShouldSucceed
          (alwaysSucceedingNAryFunction 0)
          [],
      testCase "always false" $
        directPlutusTest
          ShouldFail
          (alwaysFailingNAryFunction 0)
          [],
      testCase "guess the number, correct" $
        directPlutusTest
          ShouldSucceed
          guessTheNumber
          [P.I 3, P.I 3],
      testCase "guess the number, incorrect" $
        directPlutusTest
          ShouldFail
          guessTheNumber
          [P.I 3, P.I 4]
    ]
