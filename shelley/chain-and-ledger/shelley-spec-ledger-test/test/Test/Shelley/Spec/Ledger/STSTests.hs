{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.STSTests
  ( chainExamples,
    multisigExamples,
  )
where

import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (pattern ScriptHashObj)
import Shelley.Spec.Ledger.Keys (asWitness, hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState (WitHashes (..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (..))
import Shelley.Spec.Ledger.Tx (hashScript)
import Shelley.Spec.Ledger.TxData (RewardAcnt (..), Wdrl (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Examples (testCHAINExample)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Examples.EmptyBlock (exEmptyBlock)
import Test.Shelley.Spec.Ledger.Examples.GenesisDelegation (genesisDelegExample)
import Test.Shelley.Spec.Ledger.Examples.Mir (mirExample)
import Test.Shelley.Spec.Ledger.Examples.PoolLifetime (poolLifetimeExample)
import Test.Shelley.Spec.Ledger.Examples.PoolReReg (poolReRegExample)
import Test.Shelley.Spec.Ledger.Examples.Updates (updatesExample)
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

chainExamples :: TestTree
chainExamples =
  testGroup
    "CHAIN examples"
    [ testCase "empty block" $ testCHAINExample exEmptyBlock,
      poolLifetimeExample,
      poolReRegExample,
      updatesExample,
      genesisDelegExample,
      mirExample
    ]

multisigExamples :: TestTree
multisigExamples =
  testGroup
    "MultiSig Examples"
    [ testCase "Alice uses SingleSig script" testAliceSignsAlone,
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
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, Coin 11000)]
        [aliceOnly p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay]
    s = "problem: " ++ show utxoSt'

testAliceDoesntSign :: Assertion
testAliceDoesntSign =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW (Set.singleton $ hashScript (aliceOnly p))]]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, Coin 11000)]
        [aliceOnly p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay, asWitness Cast.dariaPay]

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, Coin 11000)]
        [aliceOnly p]
        (Wdrl Map.empty)
        (Coin 0)
        [ asWitness Cast.alicePay,
          asWitness Cast.bobPay,
          asWitness Cast.carlPay,
          asWitness Cast.dariaPay
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
        [(aliceOnly p, Coin 11000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOrBob p, Coin 11000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay]
    s = "problem: " ++ show utxoSt'

testAliceOrBob' :: Assertion
testAliceOrBob' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOrBob p, Coin 11000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay]
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
        [(aliceAndBob p, Coin 11000)]
        [aliceAndBob p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob' :: Assertion
testAliceAndBob' =
  utxoSt'
    @?= Left
      [ [ ScriptWitnessNotValidatingUTXOW
            (Set.singleton $ hashScript (aliceAndBob p))
        ]
      ]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBob p, Coin 11000)]
        [aliceAndBob p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay]

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt'
    @?= Left
      [ [ ScriptWitnessNotValidatingUTXOW
            (Set.singleton $ hashScript (aliceAndBob p))
        ]
      ]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBob p, Coin 11000)]
        [aliceAndBob p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay]

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarl p, Coin 11000)]
        [aliceAndBobOrCarl p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarl' :: Assertion
testAliceAndBobOrCarl' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceAndBobOrCarl p, Coin 11000)]
        [aliceAndBobOrCarl p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.carlPay]
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
        [(aliceAndBobOrCarlAndDaria p, Coin 11000)]
        [aliceAndBobOrCarlAndDaria p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
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
        [(aliceAndBobOrCarlAndDaria p, Coin 11000)]
        [aliceAndBobOrCarlAndDaria p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.carlPay, asWitness Cast.dariaPay]
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
        [(aliceAndBobOrCarlOrDaria p, Coin 11000)]
        [aliceAndBobOrCarlOrDaria p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
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
        [(aliceAndBobOrCarlOrDaria p, Coin 11000)]
        [aliceAndBobOrCarlOrDaria p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.carlPay]
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
        [(aliceAndBobOrCarlOrDaria p, Coin 11000)]
        [aliceAndBobOrCarlOrDaria p]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.dariaPay]
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
        [ (aliceOrBob p, Coin 10000),
          (aliceAndBobOrCarl p, Coin 1000)
        ]
        [ aliceOrBob p,
          aliceAndBobOrCarl p
        ]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay]
    s = "problem: " ++ show utxoSt'

testTwoScripts' :: Assertion
testTwoScripts' =
  utxoSt'
    @?= Left
      [ [ ScriptWitnessNotValidatingUTXOW
            (Set.singleton $ hashScript (aliceAndBob p))
        ]
      ]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [ (aliceAndBob p, Coin 10000),
          (aliceAndBobOrCarl p, Coin 1000)
        ]
        [ aliceAndBob p,
          aliceAndBobOrCarl p
        ]
        (Wdrl Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay]

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
        [(aliceAndBob p, Coin 10000)]
        [aliceAndBob p]
        (Wdrl Map.empty)
        (Coin 1000)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
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
        [(aliceOrBob p, Coin 10000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        (Coin 1000)
        [asWitness Cast.bobPay]
    wits = Set.singleton $ asWitness $ hashKey $ vKey Cast.alicePay

testScriptAndSKey'' :: Assertion
testScriptAndSKey'' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOrBob p, Coin 10000)]
        [aliceOrBob p]
        (Wdrl Map.empty)
        (Coin 1000)
        [asWitness Cast.alicePay]
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
        [(aliceAndBobOrCarl p, Coin 10000)]
        [aliceAndBobOrCarl p]
        (Wdrl Map.empty)
        (Coin 1000)
        [asWitness Cast.alicePay, asWitness Cast.carlPay]
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
        [(aliceOnly p, Coin 11000)]
        [aliceOnly p]
        ( Wdrl $
            Map.singleton
              ( RewardAcnt
                  Testnet
                  (ScriptHashObj $ hashScript (aliceOnly p))
              )
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone' :: Assertion
testRwdAliceSignsAlone' =
  utxoSt'
    @?= Left
      [ [ ScriptWitnessNotValidatingUTXOW
            (Set.singleton $ hashScript (bobOnly p))
        ]
      ]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, Coin 11000)]
        [aliceOnly p, bobOnly p]
        ( Wdrl $
            Map.singleton
              ( RewardAcnt
                  Testnet
                  ( ScriptHashObj $
                      hashScript (bobOnly p)
                  )
              )
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay]

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, Coin 11000)]
        [aliceOnly p, bobOnly p]
        ( Wdrl $
            Map.singleton
              ( RewardAcnt
                  Testnet
                  ( ScriptHashObj $
                      hashScript (bobOnly p)
                  )
              )
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone''' :: Assertion
testRwdAliceSignsAlone''' =
  utxoSt'
    @?= Left
      [ [ MissingScriptWitnessesUTXOW
            ( Set.singleton $
                hashScript (bobOnly p)
            )
        ]
      ]
  where
    p :: Proxy C
    p = Proxy
    utxoSt' =
      applyTxWithScript
        p
        [(aliceOnly p, Coin 11000)]
        [aliceOnly p]
        ( Wdrl $
            Map.singleton
              (RewardAcnt Testnet (ScriptHashObj $ hashScript (bobOnly p)))
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
