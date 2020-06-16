{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.STSTests (stsTests) where

import Cardano.Crypto.Hash (ShortHash)
import Control.State.Transition.Extended (TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import Data.Either (fromRight, isRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, singleton)
import Data.Proxy
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Credential (pattern ScriptHashObj)
import Shelley.Spec.Ledger.Keys (KeyRole (..), asWitness, hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState
  ( WitHashes (..),
    _delegationState,
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
testCHAINExample :: CHAINExample ShortHash -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  checkTrace @(CHAIN ShortHash) runShelleyBase () $ pure initSt .- block .-> expectedSt
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTS @(CHAIN ShortHash) (TRC ((), initSt, block))
  st @?= predicateFailure

-- | Applies the TICK transition to a given chain state,
-- and check that some component of the result is as expected.
testTICKChainState ::
  (Show a, Eq a) =>
  NewEpochState ShortHash ->
  TickEnv ShortHash ->
  SlotNo ->
  (NewEpochState ShortHash -> a) ->
  a ->
  Assertion
testTICKChainState initSt env slot focus expectedSt = do
  let result = runShelleyBase $ applySTS @(TICK ShortHash) (TRC (env, initSt, slot))
  case result of
    Right res -> focus res @?= expectedSt
    Left err -> assertFailure $ show err

testPreservationOfAda :: CHAINExample ShortHash -> Assertion
testPreservationOfAda (CHAINExample _ _ (Right expectedSt)) =
  totalAda expectedSt @?= maxLLSupply
testPreservationOfAda (CHAINExample _ _ (Left predicateFailure)) =
  assertFailure $ "Ada not preserved " ++ show predicateFailure

newEpochToPoolParams ::
  NewEpochState ShortHash ->
  (Map (KeyHash ShortHash 'StakePool) (PoolParams ShortHash))
newEpochToPoolParams = _pParams . _pstate . _delegationState . esLState . nesEs

newEpochToFuturePoolParams ::
  NewEpochState ShortHash ->
  (Map (KeyHash ShortHash 'StakePool) (PoolParams ShortHash))
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
    [ testCase "CHAIN example 1 - empty block" $ testCHAINExample (ex1 p),
      testCase "CHAIN example 2A - register stake key" $ testCHAINExample (ex2A p),
      testCase "CHAIN example 2B - delegate stake and create reward update" $ testCHAINExample (ex2B p),
      testCase "CHAIN example 2C - new epoch changes" $ testCHAINExample (ex2C p),
      testCase "CHAIN example 2D - second reward update" $ testCHAINExample (ex2D p),
      testCase "CHAIN example 2E - nonempty pool distr" $ testCHAINExample (ex2E p),
      testCase "CHAIN example 2F - decentralized block" $ testCHAINExample (ex2F p),
      testCase "CHAIN example 2G - prelude to the first nontrivial rewards" $ testCHAINExample (ex2G p),
      testCase "CHAIN example 2H - create a nontrivial rewards" $ testCHAINExample (ex2H p),
      testCase "CHAIN example 2I - apply a nontrivial rewards" $ testCHAINExample (ex2I p),
      testCase "CHAIN example 2J - drain reward account and deregister" $ testCHAINExample (ex2J p),
      testCase "CHAIN example 2K - stage stake pool retirement" $ testCHAINExample (ex2K p),
      testCase "CHAIN example 2L - reap stake pool" $ testCHAINExample (ex2L p),
      testCase "CHAIN example 3A - get 3/7 votes for a pparam update" $ testCHAINExample (ex3A p),
      testCase "CHAIN example 3B - get 5/7 votes for a pparam update" $ testCHAINExample (ex3B p),
      testCase "CHAIN example 3C - processes a pparam update" $ testCHAINExample (ex3C p),
      testCase "CHAIN example 4A - stage genesis key delegation" $ testCHAINExample (ex4A p),
      testCase "CHAIN example 4B - adopt genesis key delegation" $ testCHAINExample (ex4B p),
      testCase "CHAIN example 5A - create MIR cert - reserves" $ testCHAINExample (ex5AReserves p),
      testCase "CHAIN example 5A - create MIR cert - treasury" $ testCHAINExample (ex5ATreasury p),
      testCase "CHAIN example 5B - FAIL: insufficient core node signatures MIR reserves" $
        testCHAINExample (ex5BReserves p),
      testCase "CHAIN example 5B - FAIL: insufficient core node signatures MIR treasury" $
        testCHAINExample (ex5BTreasury p),
      testCase "CHAIN example 5C - FAIL: MIR insufficient reserves" $
        testCHAINExample (ex5CReserves p),
      testCase "CHAIN example 5C - FAIL: MIR insufficient treasury" $
        testCHAINExample (ex5CTreasury p),
      testCase "CHAIN example 5D - apply reserves MIR at epoch boundary" (test5DReserves p),
      testCase "CHAIN example 5D - apply treasury MIR at epoch boundary" (test5DTreasury p),
      testCase "CHAIN example 6A - Early Pool Re-registration" $ testCHAINExample (ex6A p),
      testCase "CHAIN example 6A' - Late Pool Re-registration" $ testCHAINExample (ex6A' p),
      testCase "CHAIN example 6B - Adopt Early Pool Re-registration" $ testAdoptEarlyPoolRegistration,
      testCase "CHAIN example 6B' - Adopt Late Pool Re-registration" $ testAdoptLatePoolRegistration,
      testCase "CHAIN example 1 - Preservation of ADA" $ testPreservationOfAda (ex1 p),
      testCase "CHAIN example 2A - Preservation of ADA" $ testPreservationOfAda (ex2A p),
      testCase "CHAIN example 2B - Preservation of ADA" $ testPreservationOfAda (ex2B p),
      testCase "CHAIN example 2C - Preservation of ADA" $ testPreservationOfAda (ex2C p),
      testCase "CHAIN example 2D - Preservation of ADA" $ testPreservationOfAda (ex2D p),
      testCase "CHAIN example 2E - Preservation of ADA" $ testPreservationOfAda (ex2E p),
      testCase "CHAIN example 2F - Preservation of ADA" $ testPreservationOfAda (ex2F p),
      testCase "CHAIN example 2G - Preservation of ADA" $ testPreservationOfAda (ex2G p),
      testCase "CHAIN example 2H - Preservation of ADA" $ testPreservationOfAda (ex2H p),
      testCase "CHAIN example 2I - Preservation of ADA" $ testPreservationOfAda (ex2I p),
      testCase "CHAIN example 2J - Preservation of ADA" $ testPreservationOfAda (ex2J p),
      testCase "CHAIN example 2K - Preservation of ADA" $ testPreservationOfAda (ex2K p),
      testCase "CHAIN example 2L - Preservation of ADA" $ testPreservationOfAda (ex2L p),
      testCase "CHAIN example 3A - Preservation of ADA" $ testPreservationOfAda (ex3A p),
      testCase "CHAIN example 3B - Preservation of ADA" $ testPreservationOfAda (ex3B p),
      testCase "CHAIN example 3C - Preservation of ADA" $ testPreservationOfAda (ex3C p),
      testCase "CHAIN example 4A - Preservation of ADA" $ testPreservationOfAda (ex4A p),
      testCase "CHAIN example 4B - Preservation of ADA" $ testPreservationOfAda (ex4B p),
      testCase "CHAIN example 5A Reserves - Preservation of ADA" $
        testPreservationOfAda (ex5AReserves p),
      testCase "CHAIN example 5A Treasury - Preservation of ADA" $
        testPreservationOfAda (ex5ATreasury p),
      testCase "CHAIN example 5D Reserves - Preservation of ADA" $
        (totalAda (fromRight (error "CHAIN example 5D") (ex5DReserves' p)) @?= maxLLSupply),
      testCase "CHAIN example 5D Treasury - Preservation of ADA" $
        (totalAda (fromRight (error "CHAIN example 5D") (ex5DTreasury' p)) @?= maxLLSupply),
      testCase "CHAIN example 6A - Preservation of ADA" $ testPreservationOfAda (ex6A p),
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
  where
    p :: Proxy ShortHash
    p = Proxy

testAliceSignsAlone :: Assertion
testAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p] (Wdrl Map.empty) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testAliceDoesntSign :: Assertion
testAliceDoesntSign =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceOnly p))]]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p] (Wdrl Map.empty) 0 [asWitness bobPay, asWitness carlPay, asWitness dariaPay]

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay, asWitness carlPay, asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

testWrongScript :: Assertion
testWrongScript =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW (Set.singleton $ hashScript (aliceOnly p))]]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOrBob p] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOrBob p, 11000)] [aliceOrBob p] (Wdrl Map.empty) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testAliceOrBob' :: Assertion
testAliceOrBob' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOrBob p, 11000)] [aliceOrBob p] (Wdrl Map.empty) 0 [asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob :: Assertion
testAliceAndBob =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBob p, 11000)] [aliceAndBob p] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob' :: Assertion
testAliceAndBob' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceAndBob p))]]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBob p, 11000)] [aliceAndBob p] (Wdrl Map.empty) 0 [asWitness alicePay]

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceAndBob p))]]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBob p, 11000)] [aliceAndBob p] (Wdrl Map.empty) 0 [asWitness bobPay]

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarl p, 11000)] [aliceAndBobOrCarl p] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarl' :: Assertion
testAliceAndBobOrCarl' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarl p, 11000)] [aliceAndBobOrCarl p] (Wdrl Map.empty) 0 [asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria :: Assertion
testAliceAndBobOrCarlAndDaria =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarlAndDaria p, 11000)] [aliceAndBobOrCarlAndDaria p] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria' :: Assertion
testAliceAndBobOrCarlAndDaria' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarlAndDaria p, 11000)] [aliceAndBobOrCarlAndDaria p] (Wdrl Map.empty) 0 [asWitness carlPay, asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria :: Assertion
testAliceAndBobOrCarlOrDaria =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarlOrDaria p, 11000)] [aliceAndBobOrCarlOrDaria p] (Wdrl Map.empty) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria' :: Assertion
testAliceAndBobOrCarlOrDaria' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarlOrDaria p, 11000)] [aliceAndBobOrCarlOrDaria p] (Wdrl Map.empty) 0 [asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria'' :: Assertion
testAliceAndBobOrCarlOrDaria'' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarlOrDaria p, 11000)] [aliceAndBobOrCarlOrDaria p] (Wdrl Map.empty) 0 [asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

-- multiple script-locked outputs

testTwoScripts :: Assertion
testTwoScripts =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [ (aliceOrBob p, 10000),
          (aliceAndBobOrCarl p, 1000)
        ]
        [ aliceOrBob p,
          aliceAndBobOrCarl p
        ]
        (Wdrl Map.empty)
        0
        [asWitness bobPay, asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testTwoScripts' :: Assertion
testTwoScripts' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceAndBob p))]]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [ (aliceAndBob p, 10000),
          (aliceAndBobOrCarl p, 1000)
        ]
        [ aliceAndBob p,
          aliceAndBobOrCarl p
        ]
        (Wdrl Map.empty)
        0
        [asWitness bobPay, asWitness carlPay]

-- script and skey locked

testScriptAndSKey :: Assertion
testScriptAndSKey =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBob p, 10000)]
        [aliceAndBob p]
        (Wdrl Map.empty)
        1000
        [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testScriptAndSKey' :: Assertion
testScriptAndSKey' =
  utxoSt'
    @?= Left
      [ [ MissingVKeyWitnessesUTXOW $
            WitHashes
              { addrWitHashes = wits,
                regWitHashes =
                  mempty
              }
        ]
      ]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOrBob p, 10000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        1000
        [asWitness bobPay]
    wits = Set.singleton $ asWitness $ hashKey $ vKey alicePay

testScriptAndSKey'' :: Assertion
testScriptAndSKey'' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOrBob p, 10000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        1000
        [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testScriptAndSKey''' :: Assertion
testScriptAndSKey''' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarl p, 10000)]
        [aliceAndBobOrCarl p]
        (Wdrl Map.empty)
        1000
        [asWitness alicePay, asWitness carlPay]
    s = "problem: " ++ show utxoSt'

-- Withdrawals

testRwdAliceSignsAlone :: Assertion
testRwdAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (aliceOnly p))) 1000) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone' :: Assertion
testRwdAliceSignsAlone' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (bobOnly p))]]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p, bobOnly p] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (bobOnly p))) 1000) 0 [asWitness alicePay]

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p, bobOnly p] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (bobOnly p))) 1000) 0 [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone''' :: Assertion
testRwdAliceSignsAlone''' =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW (Set.singleton $ hashScript (bobOnly p))]]
  where
    p :: Proxy ShortHash
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p] (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (bobOnly p))) 1000) 0 [asWitness alicePay, asWitness bobPay]
