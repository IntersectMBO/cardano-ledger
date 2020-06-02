{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.STSTests (stsTests) where

import Control.State.Transition.Extended (TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import Data.Either (fromRight, isRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, singleton)
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Credential (pattern ScriptHashObj)
import Shelley.Spec.Ledger.Keys (KeyRole (..), asWitness, hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState
  ( _delegationState,
    _fPParams,
    _pParams,
    _pstate,
    esLState,
    getGKeys,
    nesEs,
  )
import Shelley.Spec.Ledger.STS.Chain (totalAda)
import Shelley.Spec.Ledger.STS.Tick (pattern TickEnv)
import Shelley.Spec.Ledger.STS.Utxow (PredicateFailure (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (hashScript)
import Shelley.Spec.Ledger.TxData (Wdrl (..), pattern RewardAcnt)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CHAIN, KeyHash, NewEpochState, PoolParams, TICK, TickEnv)
import Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample (..),
    alicePay,
    bobPay,
    carlPay,
    dariaPay,
    ex1,
    ex2A,
    ex2B,
    ex2C,
    ex2Cbis,
    ex2Cquater,
    ex2Cter,
    ex2D,
    ex2E,
    ex2F,
    ex2G,
    ex2H,
    ex2I,
    ex2J,
    ex2K,
    ex2L,
    ex3A,
    ex3B,
    ex3C,
    ex4A,
    ex4B,
    ex5AReserves,
    ex5ATreasury,
    ex5BReserves,
    ex5BTreasury,
    ex5CReserves,
    ex5CTreasury,
    ex5DReserves',
    ex5DTreasury',
    ex6A,
    ex6A',
    ex6BExpectedNES,
    ex6BExpectedNES',
    ex6BPoolParams,
    test5DReserves,
    test5DTreasury,
  )
import Test.Shelley.Spec.Ledger.MultiSigExamples
  ( aliceAndBob,
    aliceAndBobOrCarl,
    aliceAndBobOrCarlAndDaria,
    aliceAndBobOrCarlOrDaria,
    aliceOnly,
    aliceOrBob,
    applyTxWithScript,
    bobOnly,
  )
import Test.Shelley.Spec.Ledger.Utils (maxLLSupply, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertFailure, testCase)

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: CHAINExample -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  checkTrace @CHAIN runShelleyBase () $ pure initSt .- block .-> expectedSt
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTS @CHAIN (TRC ((), initSt, block))
  st @?= predicateFailure

-- | Applies the TICK transition to a given chain state,
-- and check that some component of the result is as expected.
testTICKChainState ::
  (Show a, Eq a) =>
  NewEpochState ->
  TickEnv ->
  SlotNo ->
  (NewEpochState -> a) ->
  a ->
  Assertion
testTICKChainState initSt env slot focus expectedSt = do
  let result = runShelleyBase $ applySTS @TICK (TRC (env, initSt, slot))
  case result of
    Right res -> focus res @?= expectedSt
    Left err -> assertFailure $ show err

testPreservationOfAda :: CHAINExample -> Assertion
testPreservationOfAda (CHAINExample _ _ (Right expectedSt)) =
  totalAda expectedSt @?= maxLLSupply
testPreservationOfAda (CHAINExample _ _ (Left predicateFailure)) =
  assertFailure $ "Ada not preserved " ++ show predicateFailure

newEpochToPoolParams ::
  NewEpochState ->
  (Map (KeyHash 'StakePool) PoolParams)
newEpochToPoolParams = _pParams . _pstate . _delegationState . esLState . nesEs

newEpochToFuturePoolParams ::
  NewEpochState ->
  (Map (KeyHash 'StakePool) PoolParams)
newEpochToFuturePoolParams = _fPParams . _pstate . _delegationState . esLState . nesEs

testAdoptEarlyPoolRegistration :: Assertion
testAdoptEarlyPoolRegistration =
  testTICKChainState
    ex6BExpectedNES'
    (TickEnv $ getGKeys ex6BExpectedNES')
    (SlotNo 110)
    (\n -> (newEpochToPoolParams n, newEpochToFuturePoolParams n))
    (ex6BPoolParams, Map.empty)

testAdoptLatePoolRegistration :: Assertion
testAdoptLatePoolRegistration =
  testTICKChainState
    ex6BExpectedNES
    (TickEnv $ getGKeys ex6BExpectedNES)
    (SlotNo 110)
    (\n -> (newEpochToPoolParams n, newEpochToFuturePoolParams n))
    (ex6BPoolParams, Map.empty)

stsTests :: TestTree
stsTests =
  testGroup
    "STS Tests"
    [ testCase "CHAIN example 1 - empty block" $ testCHAINExample ex1,
      testCase "CHAIN example 2A - register stake key" $ testCHAINExample ex2A,
      testCase "CHAIN example 2B - delegate stake and create reward update" $ testCHAINExample ex2B,
      testCase "CHAIN example 2C - new epoch changes" $ testCHAINExample ex2C,
      testCase "CHAIN example 2Cbis - as 2C but no decay" $ testCHAINExample ex2Cbis,
      testCase "CHAIN example 2Cter - as 2C but full refund" $ testCHAINExample ex2Cter,
      testCase "CHAIN example 2Cquater - as 2C but with instant decay" $ testCHAINExample ex2Cquater,
      testCase "CHAIN example 2D - second reward update" $ testCHAINExample ex2D,
      testCase "CHAIN example 2E - nonempty pool distr" $ testCHAINExample ex2E,
      testCase "CHAIN example 2F - decentralized block" $ testCHAINExample ex2F,
      testCase "CHAIN example 2G - prelude to the first nontrivial rewards" $ testCHAINExample ex2G,
      testCase "CHAIN example 2H - create a nontrivial rewards" $ testCHAINExample ex2H,
      testCase "CHAIN example 2I - apply a nontrivial rewards" $ testCHAINExample ex2I,
      testCase "CHAIN example 2J - drain reward account and deregister" $ testCHAINExample ex2J,
      testCase "CHAIN example 2K - stage stake pool retirement" $ testCHAINExample ex2K,
      testCase "CHAIN example 2L - reap stake pool" $ testCHAINExample ex2L,
      testCase "CHAIN example 3A - get 3/7 votes for a pparam update" $ testCHAINExample ex3A,
      testCase "CHAIN example 3B - get 5/7 votes for a pparam update" $ testCHAINExample ex3B,
      testCase "CHAIN example 3C - processes a pparam update" $ testCHAINExample ex3C,
      testCase "CHAIN example 4A - stage genesis key delegation" $ testCHAINExample ex4A,
      testCase "CHAIN example 4B - adopt genesis key delegation" $ testCHAINExample ex4B,
      testCase "CHAIN example 5A - create MIR cert - reserves" $ testCHAINExample ex5AReserves,
      testCase "CHAIN example 5A - create MIR cert - treasury" $ testCHAINExample ex5ATreasury,
      testCase "CHAIN example 5B - FAIL: insufficient core node signatures MIR reserves" $
        testCHAINExample ex5BReserves,
      testCase "CHAIN example 5B - FAIL: insufficient core node signatures MIR treasury" $
        testCHAINExample ex5BTreasury,
      testCase "CHAIN example 5C - FAIL: MIR insufficient reserves" $
        testCHAINExample ex5CReserves,
      testCase "CHAIN example 5C - FAIL: MIR insufficient treasury" $
        testCHAINExample ex5CTreasury,
      testCase "CHAIN example 5D - apply reserves MIR at epoch boundary" test5DReserves,
      testCase "CHAIN example 5D - apply treasury MIR at epoch boundary" test5DTreasury,
      testCase "CHAIN example 6A - Early Pool Re-registration" $ testCHAINExample ex6A,
      testCase "CHAIN example 6A' - Late Pool Re-registration" $ testCHAINExample ex6A',
      testCase "CHAIN example 6B - Adopt Early Pool Re-registration" $ testAdoptEarlyPoolRegistration,
      testCase "CHAIN example 6B' - Adopt Late Pool Re-registration" $ testAdoptLatePoolRegistration,
      testCase "CHAIN example 1 - Preservation of ADA" $ testPreservationOfAda ex1,
      testCase "CHAIN example 2A - Preservation of ADA" $ testPreservationOfAda ex2A,
      testCase "CHAIN example 2B - Preservation of ADA" $ testPreservationOfAda ex2B,
      testCase "CHAIN example 2C - Preservation of ADA" $ testPreservationOfAda ex2C,
      testCase "CHAIN example 2D - Preservation of ADA" $ testPreservationOfAda ex2D,
      testCase "CHAIN example 2E - Preservation of ADA" $ testPreservationOfAda ex2E,
      testCase "CHAIN example 2F - Preservation of ADA" $ testPreservationOfAda ex2F,
      testCase "CHAIN example 2G - Preservation of ADA" $ testPreservationOfAda ex2G,
      testCase "CHAIN example 2H - Preservation of ADA" $ testPreservationOfAda ex2H,
      testCase "CHAIN example 2I - Preservation of ADA" $ testPreservationOfAda ex2I,
      testCase "CHAIN example 2J - Preservation of ADA" $ testPreservationOfAda ex2J,
      testCase "CHAIN example 2K - Preservation of ADA" $ testPreservationOfAda ex2K,
      testCase "CHAIN example 2L - Preservation of ADA" $ testPreservationOfAda ex2L,
      testCase "CHAIN example 3A - Preservation of ADA" $ testPreservationOfAda ex3A,
      testCase "CHAIN example 3B - Preservation of ADA" $ testPreservationOfAda ex3B,
      testCase "CHAIN example 3C - Preservation of ADA" $ testPreservationOfAda ex3C,
      testCase "CHAIN example 4A - Preservation of ADA" $ testPreservationOfAda ex4A,
      testCase "CHAIN example 4B - Preservation of ADA" $ testPreservationOfAda ex4B,
      testCase "CHAIN example 5A Reserves - Preservation of ADA" $
        testPreservationOfAda ex5AReserves,
      testCase "CHAIN example 5A Treasury - Preservation of ADA" $
        testPreservationOfAda ex5ATreasury,
      testCase "CHAIN example 5D Reserves - Preservation of ADA" $
        (totalAda (fromRight (error "CHAIN example 5D") ex5DReserves') @?= maxLLSupply),
      testCase "CHAIN example 5D Treasury - Preservation of ADA" $
        (totalAda (fromRight (error "CHAIN example 5D") ex5DTreasury') @?= maxLLSupply),
      testCase "CHAIN example 6A - Preservation of ADA" $ testPreservationOfAda ex6A,
      testCase "Alice uses SingleSig script" testAliceSignsAlone,
      testCase "FAIL: Alice doesn't sign in multi-sig" testAliceDoesntSign,
      testCase "Everybody signs in multi-sig" testEverybodySigns,
      testCase "FAIL: Wrong script for correct signatures" testWrongScript,
      testCase "Alice || Bob, Alice signs" testAliceOrBob,
      testCase "Alice || Bob, Bob signs" testAliceOrBob',
      testCase "Alice && Bob, both sign" testAliceAndBob,
      testCase "FAIL: Alice && Bob, Alice signs" testAliceAndBob',
      testCase "FAIL: Alice && Bob, Bob signs" testAliceAndBob'',
      testCase "Alice && Bob || Carl, Alice && Bob sign" testAliceAndBobOrCarl,
      testCase "Alice && Bob || Carl, Carl signs" testAliceAndBobOrCarl',
      testCase "Alice && Bob || Carl && Daria, Alice && Bob sign" testAliceAndBobOrCarlAndDaria,
      testCase "Alice && Bob || Carl && Daria, Carl && Daria sign" testAliceAndBobOrCarlAndDaria',
      testCase "Alice && Bob || Carl || Daria, Alice && Bob sign" testAliceAndBobOrCarlOrDaria,
      testCase "Alice && Bob || Carl || Daria, Carl signs" testAliceAndBobOrCarlOrDaria',
      testCase "Alice && Bob || Carl || Daria, Daria signs" testAliceAndBobOrCarlOrDaria'',
      testCase "two scripts: Alice Or Bob / alice And Bob Or Carl" testTwoScripts,
      testCase "FAIL: two scripts: Alice Or Bob / alice And Bob Or Carl" testTwoScripts',
      testCase "script and Key: Alice And Bob and alicePay" testScriptAndSKey,
      testCase "FAIL: script and Key: Alice And Bob and alicePay" testScriptAndSKey',
      testCase "script and Key: Alice Or Bob and alicePay, only Alice" testScriptAndSKey'',
      testCase "script and Key: Alice And Bob Or Carl and alicePay, Alice and Carl sign" testScriptAndSKey''',
      testCase "withdraw from script locked account, same script" testRwdAliceSignsAlone,
      testCase "FAIL: withdraw from script locked account" testRwdAliceSignsAlone',
      testCase "withdraw from script locked account, different script" testRwdAliceSignsAlone'',
      testCase "FAIL: withdraw from script locked account, signed, missing script" testRwdAliceSignsAlone'''
    ]

testAliceSignsAlone :: Assertion
testAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Wdrl Map.empty) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testAliceDoesntSign :: Assertion
testAliceDoesntSign =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript aliceOnly)]]
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Wdrl Map.empty) 0 [asWitness bobPay, asWitness carlPay, asWitness dariaPay]

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay, asWitness carlPay, asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

testWrongScript :: Assertion
testWrongScript =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW (Set.singleton $ hashScript aliceOnly)]]
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOrBob] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceOrBob, 11000)] [aliceOrBob] (Wdrl Map.empty) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testAliceOrBob' :: Assertion
testAliceOrBob' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceOrBob, 11000)] [aliceOrBob] (Wdrl Map.empty) 0 [asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob :: Assertion
testAliceAndBob =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBob, 11000)] [aliceAndBob] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob' :: Assertion
testAliceAndBob' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript aliceAndBob)]]
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBob, 11000)] [aliceAndBob] (Wdrl Map.empty) 0 [asWitness alicePay]

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript aliceAndBob)]]
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBob, 11000)] [aliceAndBob] (Wdrl Map.empty) 0 [asWitness bobPay]

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBobOrCarl, 11000)] [aliceAndBobOrCarl] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarl' :: Assertion
testAliceAndBobOrCarl' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBobOrCarl, 11000)] [aliceAndBobOrCarl] (Wdrl Map.empty) 0 [asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria :: Assertion
testAliceAndBobOrCarlAndDaria =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBobOrCarlAndDaria, 11000)] [aliceAndBobOrCarlAndDaria] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria' :: Assertion
testAliceAndBobOrCarlAndDaria' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBobOrCarlAndDaria, 11000)] [aliceAndBobOrCarlAndDaria] (Wdrl Map.empty) 0 [asWitness carlPay, asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria :: Assertion
testAliceAndBobOrCarlOrDaria =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBobOrCarlOrDaria, 11000)] [aliceAndBobOrCarlOrDaria] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria' :: Assertion
testAliceAndBobOrCarlOrDaria' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBobOrCarlOrDaria, 11000)] [aliceAndBobOrCarlOrDaria] (Wdrl Map.empty) 0 [asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria'' :: Assertion
testAliceAndBobOrCarlOrDaria'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceAndBobOrCarlOrDaria, 11000)] [aliceAndBobOrCarlOrDaria] (Wdrl Map.empty) 0 [asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

-- multiple script-locked outputs

testTwoScripts :: Assertion
testTwoScripts =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [ (aliceOrBob, 10000),
          (aliceAndBobOrCarl, 1000)
        ]
        [ aliceOrBob,
          aliceAndBobOrCarl
        ]
        (Wdrl Map.empty)
        0
        [asWitness bobPay, asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testTwoScripts' :: Assertion
testTwoScripts' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript aliceAndBob)]]
  where
    utxoSt' =
      applyTxWithScript
        [ (aliceAndBob, 10000),
          (aliceAndBobOrCarl, 1000)
        ]
        [ aliceAndBob,
          aliceAndBobOrCarl
        ]
        (Wdrl Map.empty)
        0
        [asWitness bobPay, asWitness carlPay]

-- script and skey locked

testScriptAndSKey :: Assertion
testScriptAndSKey =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBob, 10000)]
        [aliceAndBob]
        (Wdrl Map.empty)
        1000
        [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testScriptAndSKey' :: Assertion
testScriptAndSKey' =
  utxoSt' @?= Left [[MissingVKeyWitnessesUTXOW wits]]
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOrBob, 10000)]
        [aliceOrBob]
        (Wdrl Map.empty)
        1000
        [asWitness bobPay]
    wits = Set.singleton $ asWitness $ hashKey $ vKey alicePay

testScriptAndSKey'' :: Assertion
testScriptAndSKey'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOrBob, 10000)]
        [aliceOrBob]
        (Wdrl Map.empty)
        1000
        [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testScriptAndSKey''' :: Assertion
testScriptAndSKey''' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarl, 10000)]
        [aliceAndBobOrCarl]
        (Wdrl Map.empty)
        1000
        [asWitness alicePay, asWitness carlPay]
    s = "problem: " ++ show utxoSt'

-- Withdrawals

testRwdAliceSignsAlone :: Assertion
testRwdAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript aliceOnly)) 1000) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone' :: Assertion
testRwdAliceSignsAlone' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript bobOnly)]]
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOnly, bobOnly] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [asWitness alicePay]

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOnly, bobOnly] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone''' :: Assertion
testRwdAliceSignsAlone''' =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW (Set.singleton $ hashScript bobOnly)]]
  where
    utxoSt' =
      applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [asWitness alicePay, asWitness bobPay]
