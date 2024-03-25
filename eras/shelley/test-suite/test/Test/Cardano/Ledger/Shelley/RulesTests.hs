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
  testTickF,
)
where

import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Credential (pattern ScriptHashObj)
import Cardano.Ledger.Keys (asWitness, hashKey)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API (ShelleyTICK, ShelleyTICKF)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  totalObligation,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..), RewardUpdate (..))
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxBody (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Protocol.TPraos.API (GetLedgerView (..))
import Control.State.Transition.Extended (TRC (..))
import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Core.KeyPair (vKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, C_Crypto)
import Test.Cardano.Ledger.Shelley.Examples (testCHAINExample)
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
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
import Test.Cardano.Ledger.Shelley.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, discard, testProperty, (===))

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
        @C_Crypto
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
        @C_Crypto
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay, asWitness Cast.dariaPay]
    wits = Set.singleton $ hashScript @C aliceOnly

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
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

testWrongScript :: Assertion
testWrongScript =
  utxoSt' @?= Left (pure extraneous <> pure missing)
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
        [(aliceOnly, Coin 11000)]
        [aliceOrBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    asHashes = Set.singleton . hashScript @C
    extraneous = ExtraneousScriptWitnessesUTXOW $ asHashes aliceOrBob
    missing = MissingScriptWitnessesUTXOW $ asHashes aliceOnly

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
        [(aliceAndBob, Coin 11000)]
        [aliceAndBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.alicePay]
    wits = Set.singleton $ hashScript @C aliceAndBob

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt' @?= (Left . pure . ScriptWitnessNotValidatingUTXOW) wits
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
        [(aliceAndBob, Coin 11000)]
        [aliceAndBob]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay]
    wits = Set.singleton $ hashScript @C aliceAndBob

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
        [ (aliceAndBob, Coin 10000)
        , (aliceAndBobOrCarl, Coin 1000)
        ]
        [ aliceAndBob
        , aliceAndBobOrCarl
        ]
        (Withdrawals Map.empty)
        (Coin 0)
        [asWitness Cast.bobPay, asWitness Cast.carlPay]
    wits = Set.singleton $ hashScript @C aliceAndBob

-- script and skey locked

testScriptAndSKey :: Assertion
testScriptAndSKey =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
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
        @C_Crypto
        [(aliceOrBob, Coin 10000)]
        [aliceOrBob]
        (Withdrawals Map.empty)
        (Coin 1000)
        [asWitness Cast.bobPay]
    wits = Set.singleton $ asWitness $ hashKey $ vKey Cast.alicePay

testScriptAndSKey'' :: Assertion
testScriptAndSKey'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
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
        @C_Crypto
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
        @C_Crypto
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        ( Withdrawals $
            Map.singleton
              ( RewardAccount
                  Testnet
                  (ScriptHashObj $ hashScript @C aliceOnly)
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
        @C_Crypto
        [(aliceOnly, Coin 11000)]
        [aliceOnly, bobOnly]
        ( Withdrawals $
            Map.singleton
              ( RewardAccount
                  Testnet
                  ( ScriptHashObj $
                      hashScript @C bobOnly
                  )
              )
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay]
    wits = Set.singleton $ hashScript @C bobOnly

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where
    utxoSt' =
      applyTxWithScript
        @C_Crypto
        [(aliceOnly, Coin 11000)]
        [aliceOnly, bobOnly]
        ( Withdrawals $
            Map.singleton
              ( RewardAccount
                  Testnet
                  ( ScriptHashObj $
                      hashScript @C bobOnly
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
        @C_Crypto
        [(aliceOnly, Coin 11000)]
        [aliceOnly]
        ( Withdrawals $
            Map.singleton
              (RewardAccount Testnet (ScriptHashObj $ hashScript @C bobOnly))
              (Coin 1000)
        )
        (Coin 0)
        [asWitness Cast.alicePay, asWitness Cast.bobPay]
    wits = Set.singleton $ hashScript @C bobOnly

-- | The reward aggregation bug described in the Shelley ledger spec in
-- section 17.4 (in the Errata) resulted in needing to use 'aggregatedRewards' to change
-- the behavior of how rewards are collected starting at protocol version 3.
-- Instead of collecting a `Coin` for each stake credential, we collect 'Set Reward'.
-- In major protocol version 2, it is impossible for this set to be empty, but sadly this
-- property is not enforced in the types. For this reason, the property test
-- 'propTickfPerservesLedgerView' removes these empty sets from an otherwise arbitrary
-- 'NewEpochState'.
filterEmptyRewards :: NewEpochState Shelley -> NewEpochState Shelley
filterEmptyRewards (NewEpochState el bprev bcur es ru pd stash) =
  NewEpochState el bprev bcur es ru' pd stash
  where
    removeEmptyRewards = Map.filter $ not . Set.null
    ru' = case ru of
      SNothing -> SNothing
      SJust (Pulsing _ _) -> SNothing
      SJust (Complete rewardUpdate) ->
        SJust . Complete $ rewardUpdate {rs = removeEmptyRewards (rs rewardUpdate)}

setDepositsToObligation :: NewEpochState Shelley -> NewEpochState Shelley
setDepositsToObligation nes = nes {nesEs = es {esLState = ls {lsUTxOState = utxoState}}}
  where
    es = nesEs nes
    ls = esLState es
    utxoState =
      (lsUTxOState ls)
        { utxosDeposited =
            totalObligation
              (lsCertState ls)
              (utxoState ^. utxosGovStateL)
        }

-- | This property test checks the correctness of the TICKF transation.
-- TICKF is used by the consensus layer to get a ledger view in a computationally
-- cheaper way than using the TICK rule.
-- Therefore TICKF and TICK need to compute the same ledger view.
propTickfPerservesLedgerView :: NewEpochState Shelley -> Property
propTickfPerservesLedgerView nes =
  let (EpochNo e) = nesEL nes
      slot = slotFromEpoch (EpochNo $ e + 1)
      nes' = setDepositsToObligation (filterEmptyRewards nes)
      tickNes = runShelleyBase $ applySTSTest @(ShelleyTICK Shelley) (TRC ((), nes', slot))
      tickFNes = runShelleyBase $ applySTSTest @(ShelleyTICKF Shelley) (TRC ((), nes', slot))
   in fromMaybe discard $ do
        Right tickNes' <- pure tickNes
        Right tickFNes' <- pure tickFNes
        pure $ currentLedgerView tickNes' === currentLedgerView tickFNes'

testTickF :: TestTree
testTickF = testProperty "TICKF properties" propTickfPerservesLedgerView
