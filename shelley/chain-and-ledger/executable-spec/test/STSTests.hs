{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module STSTests (stsTests) where

import           Data.Either (isRight)
import qualified Data.Map.Strict as Map (empty, singleton)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import           Examples (CHAINExample (..), alicePay, bobPay, carlPay, dariaPay, ex1, ex2A, ex2B,
                     ex2C, ex2D, ex2E, ex2F, ex2G, ex2H, ex2I, ex3A, ex3B, ex3C, ex4A, ex4B, ex4C)
import           MockTypes (CHAIN)
import           MultiSigExamples (aliceAndBob, aliceAndBobOrCarl, aliceAndBobOrCarlAndDaria,
                     aliceAndBobOrCarlOrDaria, aliceOnly, aliceOrBob, applyTxWithScript, bobOnly)

import           BaseTypes (Seed (..), (⭒))
import           Control.State.Transition (TRC (..), applySTS)
import           Control.State.Transition.Trace (checkTrace, (.-), (.->))
import           Slot (Slot (..))
import           STS.Updn (UPDN)
import           STS.Utxow (PredicateFailure (..))
import           Tx (hashScript)
import           TxData (pattern RewardAcnt, pattern ScriptHashObj)

-- | The UPDN transition should update both the evolving nonce and
-- the candidate nonce during the first two-thirds of the epoch.
-- In order for the candidate nonce to catch up with the evolving
-- nonce after an epoch change, the candidate nonce is set to
-- the same value as the evolving nonce during this time.
-- Note that the number of slots per epoch is hard-coded in the Slot module.
testUPNEarly :: Assertion
testUPNEarly =
  let
    st = applySTS @UPDN (TRC (Nonce 1, (Nonce 2, Nonce 3), Slot.Slot 5))
  in
    st @?= Right (Nonce 2 ⭒ Nonce 1, Nonce 2 ⭒ Nonce 1)

-- | The UPDN transition should update only the evolving nonce
-- in the last thirds of the epoch.
-- Note that the number of slots per epoch is hard-coded in the Slot module.
testUPNLate :: Assertion
testUPNLate =
  let
    st = applySTS @UPDN (TRC (Nonce 1, (Nonce 2, Nonce 3), Slot.Slot 85))
  in
    st @?= Right (SeedOp (Nonce 2) (Nonce 1), Nonce 3)

testCHAINExample :: CHAINExample -> Assertion
testCHAINExample (CHAINExample slotNow initSt block expectedSt) =
  checkTrace @CHAIN slotNow $ pure initSt .- block .-> expectedSt

testCHAINExample1 :: Assertion
testCHAINExample1 = testCHAINExample ex1

testCHAINExample2A :: Assertion
testCHAINExample2A = testCHAINExample ex2A

testCHAINExample2B :: Assertion
testCHAINExample2B = testCHAINExample ex2B

testCHAINExample2C :: Assertion
testCHAINExample2C = testCHAINExample ex2C

testCHAINExample2D :: Assertion
testCHAINExample2D = testCHAINExample ex2D

testCHAINExample2E :: Assertion
testCHAINExample2E = testCHAINExample ex2E

testCHAINExample2F :: Assertion
testCHAINExample2F = testCHAINExample ex2F

testCHAINExample2G :: Assertion
testCHAINExample2G = testCHAINExample ex2G

testCHAINExample2H :: Assertion
testCHAINExample2H = testCHAINExample ex2H

testCHAINExample2I :: Assertion
testCHAINExample2I = testCHAINExample ex2I

testCHAINExample3A :: Assertion
testCHAINExample3A = testCHAINExample ex3A

testCHAINExample3B :: Assertion
testCHAINExample3B = testCHAINExample ex3B

testCHAINExample3C :: Assertion
testCHAINExample3C = testCHAINExample ex3C

testCHAINExample4A :: Assertion
testCHAINExample4A = testCHAINExample ex4A

testCHAINExample4B :: Assertion
testCHAINExample4B = testCHAINExample ex4B

testCHAINExample4C :: Assertion
testCHAINExample4C = testCHAINExample ex4C

stsTests :: TestTree
stsTests = testGroup "STS Tests"
  [ testCase "update nonce early in the epoch" testUPNEarly
  , testCase "update nonce late in the epoch" testUPNLate
  , testCase "CHAIN example 1 - empty block" testCHAINExample1
  , testCase "CHAIN example 2A - register stake key" testCHAINExample2A
  , testCase "CHAIN example 2B - delegate stake and create reward update" testCHAINExample2B
  , testCase "CHAIN example 2C - new epoch changes" testCHAINExample2C
  , testCase "CHAIN example 2D - second reward update" testCHAINExample2D
  , testCase "CHAIN example 2E - nonempty pool distr" testCHAINExample2E
  , testCase "CHAIN example 2F - decentralized block" testCHAINExample2F
  , testCase "CHAIN example 2G - prelude to the first nontrivial rewards" testCHAINExample2G
  , testCase "CHAIN example 2H - create a nontrivial rewards" testCHAINExample2H
  , testCase "CHAIN example 2I - apply a nontrivial rewards" testCHAINExample2I
  , testCase "CHAIN example 3A - get 3/7 votes for a pparam update" testCHAINExample3A
  , testCase "CHAIN example 3B - get 5/7 votes for a pparam update" testCHAINExample3B
  , testCase "CHAIN example 3C - processes a pparam update" testCHAINExample3C
  , testCase "CHAIN example 4A - get 3/7 votes for a version update" testCHAINExample4A
  , testCase "CHAIN example 4B - create a future app version" testCHAINExample4B
  , testCase "CHAIN example 4C - adopt a future app version" testCHAINExample4C
  , testCase "Alice uses SingleSig script" testAliceSignsAlone
  , testCase "FAIL: Alice doesn't sign in multi-sig" testAliceDoesntSign
  , testCase "Everybody signs in multi-sig" testEverybodySigns
  , testCase "FAIL: Wrong script for correct signatures" testWrongScript
  , testCase "Alice || Bob, Alice signs" testAliceOrBob
  , testCase "Alice || Bob, Bob signs" testAliceOrBob'
  , testCase "Alice && Bob, both sign" testAliceAndBob
  , testCase "FAIL: Alice && Bob, Alice signs" testAliceAndBob'
  , testCase "FAIL: Alice && Bob, Bob signs" testAliceAndBob''
  , testCase "Alice && Bob || Carl, Alice && Bob sign" testAliceAndBobOrCarl
  , testCase "Alice && Bob || Carl, Carl signs" testAliceAndBobOrCarl'
  , testCase "Alice && Bob || Carl && Daria, Alice && Bob sign" testAliceAndBobOrCarlAndDaria
  , testCase "Alice && Bob || Carl && Daria, Carl && Daria sign" testAliceAndBobOrCarlAndDaria'
  , testCase "Alice && Bob || Carl || Daria, Alice && Bob sign" testAliceAndBobOrCarlOrDaria
  , testCase "Alice && Bob || Carl || Daria, Carl signs" testAliceAndBobOrCarlOrDaria'
  , testCase "Alice && Bob || Carl || Daria, Daria signs" testAliceAndBobOrCarlOrDaria''
  , testCase "two scripts: Alice Or Bob / alice And Bob Or Carl" testTwoScripts
  , testCase "FAIL: two scripts: Alice Or Bob / alice And Bob Or Carl" testTwoScripts'
  , testCase "script and Key: Alice And Bob and alicePay" testScriptAndSKey
  , testCase "FAIL: script and Key: Alice And Bob and alicePay" testScriptAndSKey'
  , testCase "script and Key: Alice Or Bob and alicePay, only Alice" testScriptAndSKey''
  , testCase "script and Key: Alice And Bob Or Carl and alicePay, Alice and Carl sign" testScriptAndSKey'''
  , testCase "withdraw from script locked account, same script" testRwdAliceSignsAlone
  , testCase "FAIL: withdraw from script locked account" testRwdAliceSignsAlone'
  , testCase "withdraw from script locked account, different script" testRwdAliceSignsAlone''
  , testCase "FAIL: withdraw from script locked account, signed, missing script" testRwdAliceSignsAlone'''
  ]

testAliceSignsAlone :: Assertion
testAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] Map.empty 0 [alicePay]
        s = "problem: " ++ show utxoSt'

testAliceDoesntSign :: Assertion
testAliceDoesntSign =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] Map.empty 0 [bobPay, carlPay, dariaPay]

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] Map.empty 0 [alicePay, bobPay, carlPay, dariaPay]
        s = "problem: " ++ show utxoSt'

testWrongScript :: Assertion
testWrongScript =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW]]
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOrBob] Map.empty 0 [alicePay, bobPay]

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceOrBob, 11000)] [aliceOrBob] Map.empty 0 [alicePay]
        s = "problem: " ++ show utxoSt'

testAliceOrBob' :: Assertion
testAliceOrBob' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceOrBob, 11000)] [aliceOrBob] Map.empty 0 [bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBob :: Assertion
testAliceAndBob =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBob, 11000)] [aliceAndBob] Map.empty 0 [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBob' :: Assertion
testAliceAndBob' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' =
          applyTxWithScript [(aliceAndBob, 11000)] [aliceAndBob] Map.empty 0 [alicePay]

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' =
          applyTxWithScript [(aliceAndBob, 11000)] [aliceAndBob] Map.empty 0 [bobPay]

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBobOrCarl, 11000)] [aliceAndBobOrCarl] Map.empty 0 [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarl' :: Assertion
testAliceAndBobOrCarl' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBobOrCarl, 11000)] [aliceAndBobOrCarl] Map.empty 0 [carlPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria :: Assertion
testAliceAndBobOrCarlAndDaria =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBobOrCarlAndDaria, 11000)] [aliceAndBobOrCarlAndDaria] Map.empty 0 [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria' :: Assertion
testAliceAndBobOrCarlAndDaria' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBobOrCarlAndDaria, 11000)] [aliceAndBobOrCarlAndDaria] Map.empty 0 [carlPay, dariaPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria :: Assertion
testAliceAndBobOrCarlOrDaria =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBobOrCarlOrDaria, 11000)] [aliceAndBobOrCarlOrDaria] Map.empty 0 [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria' :: Assertion
testAliceAndBobOrCarlOrDaria' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBobOrCarlOrDaria, 11000)] [aliceAndBobOrCarlOrDaria] Map.empty 0 [carlPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria'' :: Assertion
testAliceAndBobOrCarlOrDaria'' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceAndBobOrCarlOrDaria, 11000)] [aliceAndBobOrCarlOrDaria] Map.empty 0 [dariaPay]
        s = "problem: " ++ show utxoSt'

-- multiple script-locked outputs

testTwoScripts :: Assertion
testTwoScripts =
  assertBool s (isRight utxoSt')
  where utxoSt' = applyTxWithScript
                   [ (aliceOrBob, 10000)
                   , (aliceAndBobOrCarl, 1000)]
                   [ aliceOrBob
                   , aliceAndBobOrCarl] Map.empty 0 [bobPay, carlPay]
        s = "problem: " ++ show utxoSt'

testTwoScripts' :: Assertion
testTwoScripts' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' = applyTxWithScript
                   [ (aliceAndBob, 10000)
                   , (aliceAndBobOrCarl, 1000)]
                   [ aliceAndBob
                   , aliceAndBobOrCarl] Map.empty 0 [bobPay, carlPay]

-- script and skey locked

testScriptAndSKey :: Assertion
testScriptAndSKey =
  assertBool s (isRight utxoSt')
  where utxoSt' = applyTxWithScript
                   [(aliceAndBob, 10000)]
                   [aliceAndBob] Map.empty 1000 [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testScriptAndSKey' :: Assertion
testScriptAndSKey' =
  utxoSt' @?= Left [[MissingVKeyWitnessesUTXOW]]
  where utxoSt' = applyTxWithScript
                   [(aliceOrBob, 10000)]
                   [aliceOrBob] Map.empty 1000 [bobPay]

testScriptAndSKey'' :: Assertion
testScriptAndSKey'' =
  assertBool s (isRight utxoSt')
  where utxoSt' = applyTxWithScript
                   [(aliceOrBob, 10000)]
                   [aliceOrBob] Map.empty 1000 [alicePay]
        s = "problem: " ++ show utxoSt'

testScriptAndSKey''' :: Assertion
testScriptAndSKey''' =
  assertBool s (isRight utxoSt')
  where utxoSt' = applyTxWithScript
                   [(aliceAndBobOrCarl, 10000)]
                   [aliceAndBobOrCarl] Map.empty 1000 [alicePay, carlPay]
        s = "problem: " ++ show utxoSt'

-- Withdrawals

testRwdAliceSignsAlone :: Assertion
testRwdAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Map.singleton (RewardAcnt (ScriptHashObj $ hashScript aliceOnly)) 1000) 0 [alicePay]
        s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone' :: Assertion
testRwdAliceSignsAlone' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly, bobOnly] (Map.singleton (RewardAcnt (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [alicePay]

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly, bobOnly] (Map.singleton (RewardAcnt (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone''' :: Assertion
testRwdAliceSignsAlone''' =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW]]
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Map.singleton (RewardAcnt (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [alicePay, bobPay]
