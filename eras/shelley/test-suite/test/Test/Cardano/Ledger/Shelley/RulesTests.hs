{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.RulesTests (
  chainExamples,
  multisigExamples,
) where

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Credential (pattern ScriptHashObj)
import Cardano.Ledger.Keys (asWitness, hashKey)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxBody (RewardAccount (..), Withdrawals (..))
import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import qualified Data.Set.NonEmpty as NES
import Test.Cardano.Ledger.Core.KeyPair (vKey)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import Test.Cardano.Ledger.Shelley.Examples.Chain (testCHAINExample)
import Test.Cardano.Ledger.Shelley.Examples.EmptyBlock (exEmptyBlock)
import Test.Cardano.Ledger.Shelley.Examples.GenesisDelegation (genesisDelegExample)
import Test.Cardano.Ledger.Shelley.Examples.Mir (mirExample)
import Test.Cardano.Ledger.Shelley.Examples.MirTransfer (testMIRTransfer)
import Test.Cardano.Ledger.Shelley.Examples.NetworkID (testPoolNetworkId)
import Test.Cardano.Ledger.Shelley.Examples.PoolLifetime (poolLifetimeExample)
import Test.Cardano.Ledger.Shelley.Examples.PoolReReg (poolReRegExample)
import Test.Cardano.Ledger.Shelley.Examples.TwoPools (twoPoolsExample)
import Test.Cardano.Ledger.Shelley.Examples.Updates (updatesExample)
import Test.Cardano.Ledger.Shelley.MultiSigExamples (
  aliceAndBob,
  aliceAndBobOrCarl,
  aliceAndBobOrCarlAndDaria,
  aliceAndBobOrCarlOrDaria,
  aliceOnly,
  aliceOrBob,
  applyTxWithScript,
  bobOnly,
 )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

chainExamples :: TestTree
chainExamples =
  testGroup
    "CHAIN examples"
    [ testCase "empty block" $ testCHAINExample exEmptyBlock
    , poolLifetimeExample
    , twoPoolsExample
    , poolReRegExample
    , updatesExample
    , genesisDelegExample
    , mirExample
    , testMIRTransfer
    , testPoolNetworkId
    ]

multisigExamples :: TestTree
multisigExamples =
  testGroup
    "MultiSig Examples"
    [ testCase "Alice uses SingleSig script" testAliceSignsAlone
    , testCase "FAIL: Alice doesn't sign in multi-sig" testAliceDoesntSign
    , testCase "Everybody signs in multi-sig" testEverybodySigns
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
    , testCase
        "script and Key: Alice And Bob Or Carl and alicePay, Alice and Carl sign"
        testScriptAndSKey'''
    , testCase "withdraw from script locked account, same script" testRwdAliceSignsAlone
    , testCase "FAIL: withdraw from script locked account" testRwdAliceSignsAlone'
    , testCase "withdraw from script locked account, different script" testRwdAliceSignsAlone''
    , testCase
        "FAIL: withdraw from script locked account, signed, missing script"
        testRwdAliceSignsAlone'''
    ]

testAliceSignsAlone :: Assertion
testAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay]
    s = "problem: " ++ show utxoSt'

testAliceDoesntSign :: Assertion
testAliceDoesntSign =
  utxoSt' @?= (Left . pure . ScriptWitnessNotValidatingUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay, asWitness Cast.dariaPay]
    wits = NES.singleton $ hashScript @ShelleyEra aliceOnly

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        (Withdrawals Map.empty)
        (Coin 0)
        [ asWitness Cast.alicePay
        , asWitness Cast.bobPay
        , asWitness Cast.carlPay
        , asWitness Cast.dariaPay
        ]
    s = "problem: " ++ show utxoSt'

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOrBob, Coin 11000)]
        [aliceOrBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay]
    s = "problem: " ++ show utxoSt'

testAliceOrBob' :: Assertion
testAliceOrBob' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOrBob, Coin 11000)]
        [aliceOrBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob :: Assertion
testAliceAndBob =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBob, Coin 11000)]
        [aliceAndBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBob' :: Assertion
testAliceAndBob' =
  utxoSt' @?= (Left . pure . ScriptWitnessNotValidatingUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBob, Coin 11000)]
        [aliceAndBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay]
    wits = NES.singleton $ hashScript @ShelleyEra aliceAndBob

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt' @?= (Left . pure . ScriptWitnessNotValidatingUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBob, Coin 11000)]
        [aliceAndBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay]
    wits = NES.singleton $ hashScript @ShelleyEra aliceAndBob

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarl, Coin 11000)]
        [aliceAndBobOrCarl]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarl' :: Assertion
testAliceAndBobOrCarl' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarl, Coin 11000)]
        [aliceAndBobOrCarl]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria :: Assertion
testAliceAndBobOrCarlAndDaria =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarlAndDaria, Coin 11000)]
        [aliceAndBobOrCarlAndDaria]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria' :: Assertion
testAliceAndBobOrCarlAndDaria' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarlAndDaria, Coin 11000)]
        [aliceAndBobOrCarlAndDaria]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.carlPay, asWitness Cast.dariaPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria :: Assertion
testAliceAndBobOrCarlOrDaria =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarlOrDaria, Coin 11000)]
        [aliceAndBobOrCarlOrDaria]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria' :: Assertion
testAliceAndBobOrCarlOrDaria' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarlOrDaria, Coin 11000)]
        [aliceAndBobOrCarlOrDaria]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.carlPay]
    s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria'' :: Assertion
testAliceAndBobOrCarlOrDaria'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarlOrDaria, Coin 11000)]
        [aliceAndBobOrCarlOrDaria]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.dariaPay]
    s = "problem: " ++ show utxoSt'

-- multiple script-locked outputs

testTwoScripts :: Assertion
testTwoScripts =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [ (aliceOrBob, Coin 10000)
        , (aliceAndBobOrCarl, Coin 1000)
        ]
        [ aliceOrBob
        , aliceAndBobOrCarl
        ]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay]
    s = "problem: " ++ show utxoSt'

testTwoScripts' :: Assertion
testTwoScripts' =
  utxoSt' @?= (Left . pure . ScriptWitnessNotValidatingUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        [ (aliceAndBob, Coin 10000)
        , (aliceAndBobOrCarl, Coin 1000)
        ]
        [ aliceAndBob
        , aliceAndBobOrCarl
        ]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay]
    wits = NES.singleton $ hashScript @ShelleyEra aliceAndBob

-- script and skey locked

testScriptAndSKey :: Assertion
testScriptAndSKey =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBob, Coin 10000)]
        [aliceAndBob]
        (Withdrawals Map.empty)
        (Coin 1000)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testScriptAndSKey' :: Assertion
testScriptAndSKey' =
  utxoSt' @?= (Left . pure . MissingVKeyWitnessesUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOrBob, Coin 10000)]
        [aliceOrBob]
        (Withdrawals Map.empty)
        (Coin 1000)
        [asWitness Cast.bobPay]
    wits = NES.singleton $ asWitness $ hashKey $ vKey Cast.alicePay

testScriptAndSKey'' :: Assertion
testScriptAndSKey'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOrBob, Coin 10000)]
        [aliceOrBob]
        (Withdrawals Map.empty)
        (Coin 1000)
        [asWitness Cast.alicePay]
    s = "problem: " ++ show utxoSt'

testScriptAndSKey''' :: Assertion
testScriptAndSKey''' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceAndBobOrCarl, Coin 10000)]
        [aliceAndBobOrCarl]
        (Withdrawals Map.empty)
        (Coin 1000)
        [asWitness Cast.alicePay, asWitness Cast.carlPay]
    s = "problem: " ++ show utxoSt'

-- Withdrawals

testRwdAliceSignsAlone :: Assertion
testRwdAliceSignsAlone =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        ( Withdrawals $
            Map.singleton
              ( RewardAccount
                  Testnet
                  (ScriptHashObj $ hashScript @ShelleyEra aliceOnly)
              )
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone' :: Assertion
testRwdAliceSignsAlone' =
  utxoSt' @?= (Left . pure . ScriptWitnessNotValidatingUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOnly, Coin 11000)]
        [aliceOnly, bobOnly]
        ( Withdrawals $
            Map.singleton
              ( RewardAccount
                  Testnet
                  ( ScriptHashObj $
                      hashScript @ShelleyEra bobOnly
                  )
              )
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay]
    wits = NES.singleton $ hashScript @ShelleyEra bobOnly

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOnly, Coin 11000)]
        [aliceOnly, bobOnly]
        ( Withdrawals $
            Map.singleton
              ( RewardAccount
                  Testnet
                  ( ScriptHashObj $
                      hashScript @ShelleyEra bobOnly
                  )
              )
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone''' :: Assertion
testRwdAliceSignsAlone''' =
  utxoSt' @?= (Left . pure . MissingScriptWitnessesUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        ( Withdrawals $
            Map.singleton
              (RewardAccount Testnet (ScriptHashObj $ hashScript @ShelleyEra bobOnly))
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    wits = NES.singleton $ hashScript @ShelleyEra bobOnly
