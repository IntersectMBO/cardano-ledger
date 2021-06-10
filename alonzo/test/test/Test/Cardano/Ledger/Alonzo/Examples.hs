{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples where

import Cardano.Ledger.Alonzo.Scripts (Script (..))
import Data.ByteString.Short (ShortByteString)
import Data.Maybe (fromMaybe)
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
import qualified PlutusTx as P
import qualified Test.Cardano.Ledger.Alonzo.PlutusScripts as Generated
  ( evendata3,
    guessTheNumber2,
    guessTheNumber3,
    odddata3,
  )
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

-- | Expects 3 args (data, redeemer, context)
guessTheNumber3 :: ShortByteString
guessTheNumber3 = case Generated.guessTheNumber3 of
  PlutusScript sbs -> sbs
  _ -> error ("Should not happen 'guessTheNumber3' is a plutus script")

-- | Expects 2 args (data, redeemer)
guessTheNumber2 :: ShortByteString
guessTheNumber2 = case Generated.guessTheNumber2 of
  PlutusScript sbs -> sbs
  _ -> error ("Should not happen 'guessTheNumber2' is a plutus script")

even3 :: ShortByteString
even3 = case Generated.evendata3 of
  PlutusScript sbs -> sbs
  _ -> error ("Should not happen 'evendata3' is a plutus script")

odd3 :: ShortByteString
odd3 = case Generated.odddata3 of
  PlutusScript sbs -> sbs
  _ -> error ("Should not happen 'odddata3' is a plutus script")

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
          guessTheNumber2
          [P.I 3, P.I 3],
      testCase "guess the number, incorrect" $
        directPlutusTest
          ShouldFail
          guessTheNumber2
          [P.I 3, P.I 4],
      testCase "guess the number with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          guessTheNumber3
          [P.I 3, P.I 3, P.I 9],
      testCase "evendata with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          even3
          [P.I 4, P.I 3, P.I 9],
      testCase "evendata with 3 args, incorrect" $
        directPlutusTest
          ShouldFail
          even3
          [P.I 3, P.I 3, P.I 9],
      testCase "odd data with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          odd3
          [P.I 3, P.I 3, P.I 9],
      testCase "odd data with 3 args, incorrect" $
        directPlutusTest
          ShouldFail
          odd3
          [P.I 4, P.I 3, P.I 9]
    ]
