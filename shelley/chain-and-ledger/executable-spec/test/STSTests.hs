{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module STSTests (stsTests) where

import           Data.Either (isRight)
import qualified Data.Map.Strict as Map (empty, singleton)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import           Examples (CHAINExample (..), alicePay, bobPay, carlPay, dariaPay, ex1, ex2, ex3,
                     ex4, ex5, ex6, ex7, ex8, ex9)
import           MockTypes (CHAIN)
import           MultiSigExamples (aliceAndBob, aliceAndBobOrCarl, aliceAndBobOrCarlAndDaria,
                     aliceAndBobOrCarlOrDaria, aliceOnly, aliceOrBob, applyTxWithScript, bobOnly)

import           BaseTypes (Seed (..))
import           Control.State.Transition (TRC (..), applySTS)
import           Control.State.Transition.Trace (checkTrace, (.-), (.->))
import           Slot (Slot (..))
import           STS.Updn (UPDN)
import           STS.Utxow (PredicateFailure (..))
import           Tx (hashScript)
import           TxData (pattern RewardAcnt, pattern ScriptHashObj)

-- | The UPDN transition should update both the evolving nonce and
-- the candidate nonce during the first two-thirds of the epoch.
-- Note that the number of slots per epoch is hard-coded in the Slot module.
testUPNEarly :: Assertion
testUPNEarly =
  let
    st = applySTS @UPDN (TRC (Nonce 1, (Nonce 2, Nonce 3), Slot.Slot 5))
  in
    st @?= Right (SeedOp (Nonce 2) (Nonce 1), SeedOp (Nonce 3) (Nonce 1))

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

testCHAINExample2 :: Assertion
testCHAINExample2 = testCHAINExample ex2

testCHAINExample3 :: Assertion
testCHAINExample3 = testCHAINExample ex3

testCHAINExample4 :: Assertion
testCHAINExample4 = testCHAINExample ex4

testCHAINExample5 :: Assertion
testCHAINExample5 = testCHAINExample ex5

testCHAINExample6 :: Assertion
testCHAINExample6 = testCHAINExample ex6

testCHAINExample7 :: Assertion
testCHAINExample7 = testCHAINExample ex7

testCHAINExample8 :: Assertion
testCHAINExample8 = testCHAINExample ex8

testCHAINExample9 :: Assertion
testCHAINExample9 = testCHAINExample ex9

stsTests :: TestTree
stsTests = testGroup "STS Tests"
  [ testCase "update nonce early in the epoch" testUPNEarly
  , testCase "update nonce late in the epoch" testUPNLate
  , testCase "CHAIN example 1 - empty block" testCHAINExample1
  , testCase "CHAIN example 2 - register stake key" testCHAINExample2
  , testCase "CHAIN example 3 - delegate stake and create reward update" testCHAINExample3
  , testCase "CHAIN example 4 - new epoch changes" testCHAINExample4
  , testCase "CHAIN example 5 - second reward update" testCHAINExample5
  , testCase "CHAIN example 6 - nonempty pool distr" testCHAINExample6
  , testCase "CHAIN example 7 - decentralized block" testCHAINExample7
  , testCase "CHAIN example 8 - prelude to the first nontrivial rewards" testCHAINExample8
  , testCase "CHAIN example 9 - create the first nontrivial rewards" testCHAINExample9
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
