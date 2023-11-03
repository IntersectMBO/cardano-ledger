{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Alonzo.PlutusScriptExamples (
  tests,
)
where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Alonzo.TxInfo (
  PlutusWithContext (..),
  ScriptResult (Fails, Passes),
  runPlutusScript,
 )
import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Plutus.Language (BinaryPlutus (..), Language (..), Plutus (..))
import Control.Monad.Writer (runWriterT)
import Data.Bifunctor (bimap)
import Data.ByteString.Short (ShortByteString)
import Data.Either (fromRight)
import PlutusLedgerApi.Test.Examples (
  alwaysFailingNAryFunction,
  alwaysSucceedingNAryFunction,
 )
import qualified PlutusLedgerApi.Test.V1.EvaluationContext as PV1
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1)
import qualified Test.Cardano.Ledger.Alonzo.PlutusScripts as Generated (
  evenRedeemer2,
  evendata3,
  guessTheNumber2,
  guessTheNumber3,
  oddRedeemer2,
  odddata3,
  redeemerIs102,
  sumsTo103,
 )
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- =============================================

-- Tests running Plutus scripts directely

evalCtxForTesting :: PV1.EvaluationContext
evalCtxForTesting = fst $ fromRight (error "failed to make evaluation context") $ runWriterT $ PV1.mkEvaluationContext (fmap snd PV1.costModelParamsForTesting)

data ShouldSucceed = ShouldSucceed | ShouldFail

directPlutusTest :: ShouldSucceed -> ShortByteString -> [PV1.Data] -> Assertion
directPlutusTest expectation script ds =
  case (expectation, evalWithTightBudget script ds) of
    (ShouldSucceed, Left e) ->
      assertBool ("This script should have succeeded, but: " <> show e) False
    (ShouldSucceed, Right _) ->
      assertBool "" True
    (ShouldFail, Left ((PV1.CekError _))) ->
      assertBool "" True -- TODO rule out cost model failure
    (ShouldFail, Left e) ->
      assertBool ("Not the script failure we expected: " <> show e) False
    (ShouldFail, Right _) ->
      assertBool "This script should have failed" False
  where
    -- Evaluate a script with sufficient budget to run it.
    pv = PV1.MajorProtocolVersion 6
    evalWithTightBudget :: ShortByteString -> [PV1.Data] -> Either PV1.EvaluationError PV1.ExBudget
    evalWithTightBudget ss datums = do
      scr <- bimap PV1.CodecError id $ PV1.deserialiseScript pv ss
      budget <- snd $ PV1.evaluateScriptCounting pv PV1.Quiet evalCtxForTesting scr datums
      snd $ PV1.evaluateScriptRestricting pv PV1.Verbose evalCtxForTesting budget scr datums

getRawPlutusScript :: String -> AlonzoScript () -> ShortByteString
getRawPlutusScript name =
  \case
    PlutusScript (Plutus _ (BinaryPlutus sbs)) -> sbs
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

tests :: TestTree
tests =
  testGroup
    "run plutus script directly"
    [ testCase "always true" $
        directPlutusTest
          ShouldSucceed
          (alwaysSucceedingNAryFunction 4)
          []
    , testCase "always false" $
        directPlutusTest
          ShouldFail
          (alwaysFailingNAryFunction 0)
          []
    , testCase "guess the number, correct" $
        directPlutusTest
          ShouldSucceed
          guessTheNumber2
          [PV1.I 3, PV1.I 3]
    , testCase "guess the number, incorrect" $
        directPlutusTest
          ShouldFail
          guessTheNumber2
          [PV1.I 3, PV1.I 4]
    , testCase "guess the number with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          guessTheNumber3
          [PV1.I 3, PV1.I 3, PV1.I 9]
    , testCase "evendata with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          even3
          [PV1.I 4, PV1.I 3, PV1.I 9]
    , testCase "evendata with 3 args, incorrect" $
        directPlutusTest
          ShouldFail
          even3
          [PV1.I 3, PV1.I 3, PV1.I 9]
    , testCase "odd data with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          odd3
          [PV1.I 3, PV1.I 3, PV1.I 9]
    , testCase "odd data with 3 args, incorrect" $
        directPlutusTest
          ShouldFail
          odd3
          [PV1.I 4, PV1.I 3, PV1.I 9]
    , testCase "sumsTo10 with 3 args, correct" $
        directPlutusTest
          ShouldSucceed
          sum103
          [PV1.I 3, PV1.I 7, PV1.I 9]
    , testCase "sumsTo10 with 3 args, incorrect" $
        directPlutusTest
          ShouldFail
          sum103
          [PV1.I 4, PV1.I 3, PV1.I 9]
    , testCase "even redeemer with 2 args, correct" $
        directPlutusTest
          ShouldSucceed
          evenRed2
          [PV1.I 12, PV1.I 9]
    , testCase "odd redeemer with 2 args, correct" $
        directPlutusTest
          ShouldSucceed
          oddredeemer2
          [PV1.I 11, PV1.I 9]
    , testCase "redeemer is 10 with 2 args, correct" $
        directPlutusTest
          ShouldSucceed
          redeemer102
          [PV1.I 10, PV1.I 10]
    , explainTestTree
    ]

-- =========================================

explainTest :: AlonzoScript Alonzo -> ShouldSucceed -> [PV1.Data] -> Assertion
explainTest script@(PlutusScript plutus) mode ds =
  let pwc =
        PlutusWithContext
          { pwcScript = plutus {plutusLanguage = PlutusV1}
          , pwcDatums = map (Data @Alonzo) ds
          , pwcExUnits = ExUnits 100000000 10000000
          , pwcCostModel = testingCostModelV1
          }
   in case (mode, runPlutusScript (ProtVer (natVersion @6) 0) pwc) of
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
        (explainTest Generated.evendata3 ShouldFail [PV1.I 3, PV1.I 3, PV1.I 5])
    , testCase
        "even data with 3 args, succeeds as expected"
        (explainTest Generated.evendata3 ShouldSucceed [PV1.I 4, PV1.I 3, PV1.I 5])
    , testCase
        "guess the number with 3 args, succeeds as expected"
        ( explainTest
            Generated.guessTheNumber3
            ShouldSucceed
            [PV1.I 4, PV1.I 4, PV1.I 5]
        )
    , testCase
        "guess the number with 3 args, fails as expected"
        (explainTest Generated.guessTheNumber3 ShouldFail [PV1.I 4, PV1.I 5, PV1.I 5])
    ]
