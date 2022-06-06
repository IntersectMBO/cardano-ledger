{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Alonzo.Examples where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (..))
import Cardano.Ledger.Alonzo.TxInfo
  ( PlutusDebug (..),
    PlutusDebugInfo (..),
    ScriptFailure (..),
    ScriptResult (Fails, Passes),
    runPLCScript,
  )
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Data.ByteString.Short (ShortByteString)
import Data.Proxy (Proxy (..))
import qualified Plutus.V1.Ledger.Api as P
import qualified Plutus.V1.Ledger.EvaluationContext as P
import Plutus.V1.Ledger.Examples
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
  )
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1)
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

-- Do not remove these instances. They are here for two resons:
--
--  * Prevent usage of Show on these huge data types in production
--  * Allow printing for testing.
deriving instance Show PlutusDebug

deriving instance Show PlutusDebugInfo

deriving instance Show ScriptFailure

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
    -- Evaluate a script with sufficient budget to run it.
    pv = P.ProtocolVersion 6 0
    evalWithTightBudget :: ShortByteString -> [P.Data] -> Either P.EvaluationError P.ExBudget
    evalWithTightBudget scr datums = do
      budget <- snd $ P.evaluateScriptCounting pv P.Quiet P.evalCtxForTesting scr datums
      snd $ P.evaluateScriptRestricting pv P.Verbose P.evalCtxForTesting budget scr datums

getRawPlutusScript :: String -> Script () -> ShortByteString
getRawPlutusScript name =
  \case
    PlutusScript _ sbs -> sbs
    _ -> error $ "Should not happen '" ++ name ++ "' is a plutus script"

-- | Expects 3 args (data, redeemer, context)
guessTheNumber3 :: ShortByteString
guessTheNumber3 = getRawPlutusScript "guessTheNumber3" Generated.guessTheNumber3

-- | Expects 2 args (data, redeemer)
guessTheNumber2 :: ShortByteString
guessTheNumber2 = getRawPlutusScript "guessTheNumber2" Generated.guessTheNumber2

even3 :: ShortByteString
even3 = getRawPlutusScript "evendata3" Generated.evendata3

odd3 :: ShortByteString
odd3 = getRawPlutusScript "odddata3" Generated.odddata3

sum103 :: ShortByteString
sum103 = getRawPlutusScript "sumsTo1033" Generated.sumsTo103

evenRed2 :: ShortByteString
evenRed2 = getRawPlutusScript "evenRedeemer2" Generated.evenRedeemer2

redeemer102 :: ShortByteString
redeemer102 = getRawPlutusScript "redeemeris102" Generated.redeemerIs102

oddredeemer2 :: ShortByteString
oddredeemer2 = getRawPlutusScript "oddredeemer2" Generated.oddRedeemer2

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
explainTest script@(PlutusScript _ bytes) mode ds =
  case ( mode,
         runPLCScript
           alonzo
           (ProtVer 6 0)
           PlutusV1
           testingCostModelV1
           bytes
           (ExUnits 100000000 10000000)
           ds
       ) of
    (ShouldSucceed, Passes _) -> assertBool "" True
    (ShouldSucceed, Fails _ xs) -> assertBool (show xs) False
    (ShouldFail, Passes _) -> assertBool ("Test that should fail, passes: " ++ show script) False
    (ShouldFail, Fails _ _) -> assertBool "" True
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
