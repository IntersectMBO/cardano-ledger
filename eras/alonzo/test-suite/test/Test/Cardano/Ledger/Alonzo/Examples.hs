{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..), Script (..))
import Cardano.Ledger.Alonzo.TxInfo (ScriptResult (Fails, Passes), runPLCScript)
import Data.ByteString.Short (ShortByteString)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Debug.Trace
import qualified Plutus.V1.Ledger.Api as P
import Plutus.V1.Ledger.Examples
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
  )
import qualified Test.Cardano.Ledger.Alonzo.PlutusScripts as Generated
  ( evenRedeemer2,
    evendata3,
    guessTheNumber2,
    guessTheNumber3,
    oddRedeemer2,
    odddata3,
    redeemerIs102,
    sumsTo103,
  )
import Test.Cardano.Ledger.EraBuffet (StandardCrypto)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- =============================================

-- Tests running Plutus scripts directely

data ShouldSucceed = ShouldSucceed | ShouldFail

directPlutusTest :: ShouldSucceed -> ShortByteString -> [P.Data] -> Assertion
directPlutusTest expectation script ds =
  case (expectation, evalWithTightBudget script ds) of
    (ShouldSucceed, Left e) ->
      assertBool ("This script should have succeeded, but: " <> show e) False
    (ShouldSucceed, Right _) ->
      assertBool "" True
    (ShouldFail, Left ((P.CekError _))) ->
      assertBool "" True -- TODO rule out cost model failure
    (ShouldFail, Left e) ->
      assertBool ("Not the script failure we expected: " <> show e) False
    (ShouldFail, Right _) ->
      assertBool "This script should have failed" False
  where
    costModel = fromMaybe (error "corrupt default cost model") P.defaultCostModelParams
    -- Evaluate a script with sufficient budget to run it.
    evalWithTightBudget :: ShortByteString -> [P.Data] -> Either P.EvaluationError P.ExBudget
    evalWithTightBudget scr datums = do
      budget <- snd $ P.evaluateScriptCounting P.Quiet costModel scr datums
      snd $ P.evaluateScriptRestricting P.Verbose costModel budget scr datums

-- | Expects 3 args (data, redeemer, context)
guessTheNumber3 :: ShortByteString
guessTheNumber3 = case Generated.guessTheNumber3 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'guessTheNumber3' is a plutus script")

-- | Expects 2 args (data, redeemer)
guessTheNumber2 :: ShortByteString
guessTheNumber2 = case Generated.guessTheNumber2 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'guessTheNumber2' is a plutus script")

even3 :: ShortByteString
even3 = case Generated.evendata3 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'evendata3' is a plutus script")

odd3 :: ShortByteString
odd3 = case Generated.odddata3 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'odddata3' is a plutus script")

sum103 :: ShortByteString
sum103 = case Generated.sumsTo103 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'sumsTo1033' is a plutus script")

evenRed2 :: ShortByteString
evenRed2 = case Generated.evenRedeemer2 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'evenredeemer2' is a plutus script")

redeemer102 :: ShortByteString
redeemer102 = case Generated.redeemerIs102 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'redeemeris102' is a plutus script")

oddredeemer2 :: ShortByteString
oddredeemer2 = case Generated.oddRedeemer2 of
  PlutusScript _ sbs -> sbs
  _ -> error ("Should not happen 'oddredeemer2' is a plutus script")

plutusScriptExamples :: TestTree
plutusScriptExamples =
  testGroup
    "run plutus script directly"
    [ testCase "always true" $
        directPlutusTest
          ShouldSucceed
          (alwaysSucceedingNAryFunction 4)
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
          [P.I 4, P.I 3, P.I 9],
      testCase "sumsTo10 with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          sum103
          [P.I 3, P.I 7, P.I 9],
      testCase "sumsTo10 with 3 args, incorrect" $
        directPlutusTest
          ShouldFail
          sum103
          [P.I 4, P.I 3, P.I 9],
      testCase "even redeemer with 2 args, correct" $
        directPlutusTest
          ShouldSucceed
          evenRed2
          [P.I 12, P.I 9],
      testCase "odd redeemer with 2 args, correct" $
        directPlutusTest
          ShouldSucceed
          oddredeemer2
          [P.I 11, P.I 9],
      testCase "redeemer is 10 with 2 args, correct" $
        directPlutusTest
          ShouldSucceed
          redeemer102
          [P.I 10, P.I 10],
      explainTestTree
    ]

-- =========================================

alonzo :: Proxy (AlonzoEra StandardCrypto)
alonzo = Proxy

explainTest :: Script (AlonzoEra StandardCrypto) -> ShouldSucceed -> [P.Data] -> Assertion
explainTest (script@(PlutusScript _ bytes)) mode ds =
  let cost = fromMaybe (error "corrupt default cost model") P.defaultCostModelParams
   in case (mode, runPLCScript alonzo PlutusV1 (CostModel cost) bytes (ExUnits 100000000 10000000) ds) of
        (ShouldSucceed, Passes) -> assertBool "" True
        (ShouldSucceed, Fails xs) -> assertBool (show xs) (trace (show (head xs)) False)
        (ShouldFail, Passes) -> assertBool ("Test that should fail, passes: " ++ show script) False
        (ShouldFail, Fails _) -> assertBool "" True
explainTest _other _mode _ds = assertBool "BAD Script" False

explainTestTree :: TestTree
explainTestTree =
  testGroup
    "explain failures tests"
    [ testCase
        "even data with 3 args, fails as expected"
        (explainTest Generated.evendata3 ShouldFail [P.I 3, P.I 3, P.I 5]),
      testCase
        "even data with 3 args, succeeds as expected"
        (explainTest Generated.evendata3 ShouldSucceed [P.I 4, P.I 3, P.I 5]),
      testCase
        "guess the number with 3 args, succeeds as expected"
        ( explainTest
            Generated.guessTheNumber3
            ShouldSucceed
            [P.I 4, P.I 4, P.I 5]
        ),
      testCase
        "guess the number with 3 args, fails as expected"
        (explainTest Generated.guessTheNumber3 ShouldFail [P.I 4, P.I 5, P.I 5])
    ]
