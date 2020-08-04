{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.STSTests (stsTests) where

import Control.State.Transition.Extended (TRC (..))
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)
import Data.Proxy
import Shelley.Spec.Ledger.API
  ( CHAIN,
    DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    TICK,
    TickEnv,
  )
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
  )
import Shelley.Spec.Ledger.LedgerState
  ( getGKeys,
  )
import Shelley.Spec.Ledger.STS.Chain (totalAda)
import Shelley.Spec.Ledger.STS.Tick (pattern TickEnv)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.TxData
  ( PoolParams,
  )
import Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( testBootstrapNotSpending,
    testBootstrapSpending,
  )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( C,
  )
import Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample (..),
    ex3A,
    ex3B,
    ex3C,
    ex3D,
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
import Test.Shelley.Spec.Ledger.Examples.EmptyBlock (exEmptyBlock)
import Test.Shelley.Spec.Ledger.Examples.PoolLifetime
  ( poolLifetime1,
    poolLifetime10,
    poolLifetime11,
    poolLifetime12,
    poolLifetime2,
    poolLifetime3,
    poolLifetime4,
    poolLifetime5,
    poolLifetime6,
    poolLifetime7,
    poolLifetime8,
    poolLifetime9,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( applySTSTest,
    maxLLSupply,
    runShelleyBase,
    testSTS,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: CHAINExample C -> Assertion
testCHAINExample (CHAINExample initSt block expectedSt) =
  testSTS @(CHAIN C) () initSt block expectedSt

-- | Applies the TICK transition to a given chain state,
-- and check that some component of the result is as expected.
testTICKChainState ::
  (Show a, Eq a) =>
  NewEpochState C ->
  TickEnv C ->
  SlotNo ->
  (NewEpochState C -> a) ->
  a ->
  Assertion
testTICKChainState initSt env slot focus expectedSt = do
  let result = runShelleyBase $ applySTSTest @(TICK C) (TRC (env, initSt, slot))
  case result of
    Right res -> focus res @?= expectedSt
    Left err -> assertFailure $ show err

testPreservationOfAda :: CHAINExample C -> Assertion
testPreservationOfAda (CHAINExample _ _ (Right expectedSt)) =
  totalAda expectedSt @?= maxLLSupply
testPreservationOfAda (CHAINExample _ _ (Left predicateFailure)) =
  assertFailure $ "Ada not preserved " ++ show predicateFailure

newEpochToPoolParams ::
  NewEpochState C ->
  (Map (KeyHash 'StakePool C) (PoolParams C))
newEpochToPoolParams = _pParams . _pstate . _delegationState . esLState . nesEs

newEpochToFuturePoolParams ::
  NewEpochState C ->
  (Map (KeyHash 'StakePool C) (PoolParams C))
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
    [ testCase "CHAIN example - empty block" $ testCHAINExample exEmptyBlock,
      testCase "CHAIN example - pool lifetime - register stake key" $ testCHAINExample poolLifetime1,
      testCase "CHAIN example - pool lifetime - delegate stake and create reward update" $ testCHAINExample poolLifetime2,
      testCase "CHAIN example - pool lifetime - new epoch changes" $ testCHAINExample poolLifetime3,
      testCase "CHAIN example - pool lifetime - second reward update" $ testCHAINExample poolLifetime4,
      testCase "CHAIN example - pool lifetime - nonempty pool distr" $ testCHAINExample poolLifetime5,
      testCase "CHAIN example - pool lifetime - decentralized block" $ testCHAINExample poolLifetime6,
      testCase "CHAIN example - pool lifetime - prelude to the first nontrivial rewards" $ testCHAINExample poolLifetime7,
      testCase "CHAIN example - pool lifetime - create a nontrivial rewards" $ testCHAINExample poolLifetime8,
      testCase "CHAIN example - pool lifetime - apply a nontrivial rewards" $ testCHAINExample poolLifetime9,
      testCase "CHAIN example - pool lifetime - drain reward account and deregister" $ testCHAINExample poolLifetime10,
      testCase "CHAIN example - pool lifetime - stage stake pool retirement" $ testCHAINExample poolLifetime11,
      testCase "CHAIN example - pool lifetime - reap stake pool" $ testCHAINExample poolLifetime12,
      testCase "CHAIN example 3A - get 3/7 votes for a pparam update" $ testCHAINExample (ex3A p),
      testCase "CHAIN example 3B - get 5/7 votes for a pparam update" $ testCHAINExample (ex3B p),
      testCase "CHAIN example 3C - votes for the next epoch" $ testCHAINExample (ex3C p),
      testCase "CHAIN example 3D - processes a pparam update" $ testCHAINExample (ex3D p),
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
      testCase "CHAIN example - empty block - Preservation of ADA" $ testPreservationOfAda exEmptyBlock,
      testCase "CHAIN example - pool lifetime 1 - Preservation of ADA" $ testPreservationOfAda (poolLifetime1),
      testCase "CHAIN example - pool lifetime 2 - Preservation of ADA" $ testPreservationOfAda poolLifetime2,
      testCase "CHAIN example - pool lifetime 3 - Preservation of ADA" $ testPreservationOfAda poolLifetime3,
      testCase "CHAIN example - pool lifetime 4 - Preservation of ADA" $ testPreservationOfAda poolLifetime4,
      testCase "CHAIN example - pool lifetime 5 - Preservation of ADA" $ testPreservationOfAda poolLifetime5,
      testCase "CHAIN example - pool lifetime 6 - Preservation of ADA" $ testPreservationOfAda poolLifetime6,
      testCase "CHAIN example - pool lifetime 7 - Preservation of ADA" $ testPreservationOfAda poolLifetime7,
      testCase "CHAIN example - pool lifetime 8 - Preservation of ADA" $ testPreservationOfAda poolLifetime8,
      testCase "CHAIN example - pool lifetime 9 - Preservation of ADA" $ testPreservationOfAda poolLifetime9,
      testCase "CHAIN example - pool lifetime 10 - Preservation of ADA" $ testPreservationOfAda poolLifetime10,
      testCase "CHAIN example - pool lifetime 11 - Preservation of ADA" $ testPreservationOfAda poolLifetime11,
      testCase "CHAIN example - pool lifetime 12 - Preservation of ADA" $ testPreservationOfAda poolLifetime12,
      testCase "CHAIN example 3A - Preservation of ADA" $ testPreservationOfAda (ex3A p),
      testCase "CHAIN example 3B - Preservation of ADA" $ testPreservationOfAda (ex3B p),
      testCase "CHAIN example 3C - Preservation of ADA" $ testPreservationOfAda (ex3C p),
      testCase "CHAIN example 3D - Preservation of ADA" $ testPreservationOfAda (ex3D p),
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
      {-
       - TODO re-enable after the script embargo has been lifted
       -
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
      testCase "FAIL: withdraw from script locked account, signed, missing script" testRwdAliceSignsAlone''',
      -}
      testCase "spend from a bootstrap address" testBootstrapSpending,
      testCase "don't spend from a bootstrap address" testBootstrapNotSpending
    ]
  where
    p :: Proxy C
    p = Proxy

{-
 - TODO re-enable after the script embargo has been lifted
 -
testAliceSignsAlone :: Assertion
testAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p] (Wdrl Map.empty) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testAliceDoesntSign :: Assertion
testAliceDoesntSign =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceOnly p))]]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOnly p, 11000)] [aliceOnly p] (Wdrl Map.empty) 0 [asWitness bobPay, asWitness carlPay, asWitness dariaPay]

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, 11000)]
        [aliceOnly p]
        (Wdrl Map.empty)
        0
        [ asWitness alicePay,
          asWitness bobPay,
          asWitness carlPay,
          asWitness dariaPay
        ]
    s = "problem: " ++ show utxoSt'

testWrongScript :: Assertion
testWrongScript =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW (Set.singleton $ hashScript (aliceOnly p))]]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, 11000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        0
        [asWitness alicePay, asWitness bobPay]

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOrBob p, 11000)] [aliceOrBob p] (Wdrl Map.empty) 0 [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testAliceOrBob' :: Assertion
testAliceOrBob' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceOrBob p, 11000)] [aliceOrBob p] (Wdrl Map.empty) 0 [asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob :: Assertion
testAliceAndBob =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBob p, 11000)]
        [aliceAndBob p]
        (Wdrl Map.empty)
        0
        [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob' :: Assertion
testAliceAndBob' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceAndBob p))]]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBob p, 11000)] [aliceAndBob p] (Wdrl Map.empty) 0 [asWitness alicePay]

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceAndBob p))]]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBob p, 11000)] [aliceAndBob p] (Wdrl Map.empty) 0 [asWitness bobPay]

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarl p, 11000)]
        [aliceAndBobOrCarl p]
        (Wdrl Map.empty)
        0
        [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarl' :: Assertion
testAliceAndBobOrCarl' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript p [(aliceAndBobOrCarl p, 11000)] [aliceAndBobOrCarl p] (Wdrl Map.empty) 0 [asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria :: Assertion
testAliceAndBobOrCarlAndDaria =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarlAndDaria p, 11000)]
        [aliceAndBobOrCarlAndDaria p]
        (Wdrl Map.empty)
        0
        [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria' :: Assertion
testAliceAndBobOrCarlAndDaria' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarlAndDaria p, 11000)]
        [aliceAndBobOrCarlAndDaria p]
        (Wdrl Map.empty)
        0
        [asWitness carlPay, asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria :: Assertion
testAliceAndBobOrCarlOrDaria =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarlOrDaria p, 11000)]
        [aliceAndBobOrCarlOrDaria p]
        (Wdrl Map.empty)
        0
        [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria' :: Assertion
testAliceAndBobOrCarlOrDaria' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarlOrDaria p, 11000)]
        [aliceAndBobOrCarlOrDaria p]
        (Wdrl Map.empty)
        0
        [asWitness carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria'' :: Assertion
testAliceAndBobOrCarlOrDaria'' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarlOrDaria p, 11000)]
        [aliceAndBobOrCarlOrDaria p]
        (Wdrl Map.empty)
        0
        [asWitness dariaPay]
    s = "problem: " ++ show utxoSt'

-- multiple script-locked outputs

testTwoScripts :: Assertion
testTwoScripts =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
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
    p :: Proxy C
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
    p :: Proxy C
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
            WitHashes wits
        ]
      ]
  where
    p :: Proxy C
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
    p :: Proxy C
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
    p :: Proxy C
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
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, 11000)]
        [aliceOnly p]
        (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (aliceOnly p))) 1000)
        0
        [asWitness alicePay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone' :: Assertion
testRwdAliceSignsAlone' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (bobOnly p))]]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, 11000)]
        [aliceOnly p, bobOnly p]
        (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (bobOnly p))) 1000)
        0
        [asWitness alicePay]

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, 11000)]
        [aliceOnly p, bobOnly p]
        (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (bobOnly p))) 1000)
        0
        [asWitness alicePay, asWitness bobPay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone''' :: Assertion
testRwdAliceSignsAlone''' =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW (Set.singleton $ hashScript (bobOnly p))]]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, 11000)]
        [aliceOnly p]
        (Wdrl $ Map.singleton (RewardAcnt Testnet (ScriptHashObj $ hashScript (bobOnly p))) 1000)
        0
        [asWitness alicePay, asWitness bobPay]
-}
