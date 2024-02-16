{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Alonzo.PlutusScriptExamples (
  tests,
)
where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Core (EraCrypto, eraProtVerLow)
import Cardano.Ledger.Plutus.Evaluate (
  PlutusDatums (..),
  PlutusWithContext (..),
  ScriptResult (Fails, Passes),
  runPlutusScript,
 )
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusLanguage (..),
  SLanguage (..),
  hashPlutusScript,
  plutusLanguage,
 )
import Data.Bifunctor (first)
import GHC.Stack
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Plutus (
  alwaysFailsPlutus,
  alwaysSucceedsPlutus,
  testingEvaluationContext,
  zeroTestingCostModel,
 )
import Test.Cardano.Ledger.Plutus.Examples
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- =============================================

data ShouldSucceed = ShouldSucceed | ShouldFail

directPlutusTest ::
  forall era l.
  (HasCallStack, EraPlutusTxInfo l era) =>
  ShouldSucceed ->
  Plutus l ->
  [P.Data] ->
  Assertion
directPlutusTest expectation plutus datums =
  case (expectation, evalWithTightBudget) of
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
    lang = plutusLanguage plutus
    evalContext = testingEvaluationContext lang
    -- Evaluate a script with sufficient budget to run it.
    pv = eraProtVerLow @era
    evalWithTightBudget :: Either P.EvaluationError P.ExBudget
    evalWithTightBudget = do
      plutusRunnable <- first P.CodecError $ decodePlutusRunnable pv plutus
      budget <-
        snd $ evaluatePlutusRunnableBudget pv P.Quiet evalContext plutusRunnable datums
      snd $ evaluatePlutusRunnable pv P.Verbose evalContext budget plutusRunnable datums

tests :: TestTree
tests =
  testGroup
    "run plutus script directly"
    [ testCase "always true" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (alwaysSucceedsPlutus @'PlutusV1 4)
          []
    , testCase "always false" $
        directPlutusTest @Alonzo
          ShouldFail
          (alwaysFailsPlutus @'PlutusV1 0)
          []
    , testCase "guess the number, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (guessTheNumber2 SPlutusV1)
          [P.I 3, P.I 3]
    , testCase "guess the number, incorrect" $
        directPlutusTest @Alonzo
          ShouldFail
          (guessTheNumber2 SPlutusV1)
          [P.I 3, P.I 4]
    , testCase "guess the number with 3 args, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (guessTheNumber3 SPlutusV1)
          [P.I 3, P.I 3, P.I 9]
    , testCase "evendata with 3 args, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (evendata3 SPlutusV1)
          [P.I 4, P.I 3, P.I 9]
    , testCase "evendata with 3 args, incorrect" $
        directPlutusTest @Alonzo
          ShouldFail
          (evendata3 SPlutusV1)
          [P.I 3, P.I 3, P.I 9]
    , testCase "odd data with 3 args, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (odddata3 SPlutusV1)
          [P.I 3, P.I 3, P.I 9]
    , testCase "odd data with 3 args, incorrect" $
        directPlutusTest @Alonzo
          ShouldFail
          (odddata3 SPlutusV1)
          [P.I 4, P.I 3, P.I 9]
    , testCase "sumsTo10 with 3 args, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (sumsTo103 SPlutusV1)
          [P.I 3, P.I 7, P.I 9]
    , testCase "sumsTo10 with 3 args, incorrect" $
        directPlutusTest @Alonzo
          ShouldFail
          (sumsTo103 SPlutusV1)
          [P.I 4, P.I 3, P.I 9]
    , testCase "even redeemer with 2 args, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (evenRedeemer2 SPlutusV1)
          [P.I 12, P.I 9]
    , testCase "odd redeemer with 2 args, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (oddRedeemer2 SPlutusV1)
          [P.I 11, P.I 9]
    , testCase "redeemer is 10 with 2 args, correct" $
        directPlutusTest @Alonzo
          ShouldSucceed
          (redeemerIs102 SPlutusV1)
          [P.I 10, P.I 10]
    , explainTestTree
    ]

-- =========================================

explainTest ::
  forall era l.
  EraPlutusTxInfo l era =>
  Plutus l ->
  ShouldSucceed ->
  [P.Data] ->
  Assertion
explainTest plutus mode ds =
  let pwc =
        PlutusWithContext
          { pwcProtocolVersion = eraProtVerLow @era
          , pwcScript = Left plutus
          , pwcScriptHash = hashPlutusScript @l @(EraCrypto era) plutus
          , pwcDatums = PlutusDatums ds
          , pwcExUnits = ExUnits 100000000 10000000
          , pwcCostModel = zeroTestingCostModel (plutusLanguage plutus)
          }
   in case (mode, runPlutusScript pwc) of
        (ShouldSucceed, Passes _) -> assertBool "" True
        (ShouldSucceed, Fails _ xs) -> assertBool (show xs) False
        (ShouldFail, Passes _) -> assertBool ("Test that should fail, passes: " ++ show pwc) False
        (ShouldFail, Fails _ _) -> assertBool "" True

explainTestTree :: TestTree
explainTestTree =
  testGroup
    "explain failures tests"
    [ testCase
        "even data with 3 args, fails as expected"
        (explainTest @Alonzo (evendata3 SPlutusV1) ShouldFail [P.I 3, P.I 3, P.I 5])
    , testCase
        "even data with 3 args, succeeds as expected"
        (explainTest @Alonzo (evendata3 SPlutusV1) ShouldSucceed [P.I 4, P.I 3, P.I 5])
    , testCase
        "guess the number with 3 args, succeeds as expected"
        (explainTest @Alonzo (guessTheNumber3 SPlutusV1) ShouldSucceed [P.I 4, P.I 4, P.I 5])
    , testCase
        "guess the number with 3 args, fails as expected"
        (explainTest @Alonzo (guessTheNumber3 SPlutusV1) ShouldFail [P.I 4, P.I 5, P.I 5])
    ]
