{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module STSTests (stsTests) where

import           Data.Either (isRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList, singleton)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Crypto.Random (drgNewTest, withDRG)
import           Examples (CHAINExample (..), ex1, ex2, ex3, ex4)
import           MockTypes (Addr, CHAIN, KeyPair, LedgerState, MultiSig, SKey, ScriptHash, Tx,
                     TxBody, TxId, TxIn, UTXOW, UTxOState, VKey, Wdrl)

import           BaseTypes (Seed (..))
import           Coin (Coin (..))
import           Control.State.Transition (PredicateFailure, TRC (..), applySTS)
import           Keys (pattern Dms, pattern KeyPair, pattern SKey, pattern VKey, hashKey, vKey)
import           LedgerState (genesisId, genesisState, _utxoState)
import           PParams (emptyPParams)
import           Slot (Slot (..))
import           STS.Updn (UPDN)
import           STS.Utxow (PredicateFailure (..))
import           Tx (hashScript)
import           TxData (pattern AddrBase, pattern KeyHashObj, pattern RequireAllOf,
                     pattern RequireAnyOf, pattern RequireMOf, pattern RequireSignature,
                     pattern RewardAcnt, pattern ScriptHashObj, pattern StakeKeys,
                     pattern StakePools, pattern Tx, pattern TxBody, pattern TxIn, pattern TxOut,
                     _body)
import           Updates (emptyUpdate)
import           UTxO (makeWitnessesVKey, txid)


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
    applySTS @CHAIN (TRC (slotNow, initSt, block)) @?= Right expectedSt

testCHAINExample1 :: Assertion
testCHAINExample1 = testCHAINExample ex1

testCHAINExample2 :: Assertion
testCHAINExample2 = testCHAINExample ex2

testCHAINExample3 :: Assertion
testCHAINExample3 = testCHAINExample ex3

testCHAINExample4 :: Assertion
testCHAINExample4 = testCHAINExample ex4

stsTests :: TestTree
stsTests = testGroup "STS Tests"
  [ testCase "update nonce early in the epoch" testUPNEarly
  , testCase "update nonce late in the epoch" testUPNLate
  , testCase "CHAIN example 1 - empty block" testCHAINExample1
  , testCase "CHAIN example 2 - register stake key" testCHAINExample2
  , testCase "CHAIN example 3 - create reward update" testCHAINExample3
  , testCase "CHAIN example 4 - new epoch changes" testCHAINExample4
  , testCase "apply Transaction to genesis UTxO" testInitialUTXO
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

-- Multi-Signature tests

mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKey)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKey $ deriveVerKeyDSIGN sk)

alicePay :: KeyPair
alicePay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceStake :: KeyPair
aliceStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

mkAddr :: (KeyPair, KeyPair) -> Addr
mkAddr (payKey, stakeKey) =
  AddrBase (KeyHashObj . hashKey $ vKey payKey) (KeyHashObj . hashKey $ vKey stakeKey)

aliceAddr :: Addr
aliceAddr = mkAddr (alicePay, aliceStake)


bobPay :: KeyPair
bobPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

bobStake :: KeyPair
bobStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

bobAddr :: Addr
bobAddr = mkAddr (bobPay, bobStake)


carlPay :: KeyPair
carlPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

carlStake :: KeyPair
carlStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (5, 5, 5, 5, 5)

carlAddr :: Addr
carlAddr = mkAddr (carlPay, carlStake)


dariaPay :: KeyPair
dariaPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

dariaStake :: KeyPair
dariaStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

dariaAddr :: Addr
dariaAddr = mkAddr (dariaPay, dariaStake)


-- Multi-signature scripts
singleKeyOnly :: Addr -> MultiSig
singleKeyOnly (AddrBase (KeyHashObj pk) _ ) = RequireSignature pk
singleKeyOnly _ = error "use VKey address"

aliceOnly :: MultiSig
aliceOnly = singleKeyOnly aliceAddr

aliceOrBob :: MultiSig
aliceOrBob = RequireAnyOf [aliceOnly, singleKeyOnly bobAddr]

aliceAndBob :: MultiSig
aliceAndBob = RequireAllOf [aliceOnly, singleKeyOnly bobAddr]

aliceAndBobOrCarl :: MultiSig
aliceAndBobOrCarl = RequireMOf 1 [aliceAndBob, singleKeyOnly carlAddr]

aliceAndBobOrCarlAndDaria :: MultiSig
aliceAndBobOrCarlAndDaria =
  RequireAnyOf [aliceAndBob,
                RequireAllOf [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]]

aliceAndBobOrCarlOrDaria :: MultiSig
aliceAndBobOrCarlOrDaria =
  RequireMOf 1 [aliceAndBob,
                RequireAnyOf [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]]


initTxBody :: [(Addr, Coin)] -> TxBody
initTxBody addrs = TxBody
        (Set.fromList [TxIn genesisId 0, TxIn genesisId 1])
        (map (uncurry TxOut) addrs)
        []
        Map.empty
        (Coin 0)
        (Slot 0)
        emptyUpdate

scriptTxBody :: [TxIn] -> Addr -> Wdrl -> Coin -> TxBody
scriptTxBody inp addr wdrl c =
  TxBody
    (Set.fromList inp)
    [TxOut addr c]
    []
    wdrl
    (Coin 0)
    (Slot 10)
    emptyUpdate

makeTx :: TxBody -> [KeyPair] -> Map ScriptHash MultiSig -> Tx
makeTx txBody keyPairs =
  Tx txBody (makeWitnessesVKey txBody keyPairs)

aliceInitCoin :: Coin
aliceInitCoin = 10000

bobInitCoin :: Coin
bobInitCoin = 1000

genesis :: LedgerState
genesis = genesisState
           [ TxOut aliceAddr aliceInitCoin
           , TxOut bobAddr bobInitCoin]

-- | Create an initial UTxO state where Alice has 'aliceInitCoin' and Bob
-- 'bobInitCoin' to spend. Then create and apply a transaction which, if
-- 'aliceKeep' is greater than 0, gives that amount to Alice and creates outputs
-- locked by a script for each pair of script, coin value in 'msigs'.
initialUTxOState
  :: Coin
  -> [(MultiSig, Coin)]
  -> (TxId, Either [[PredicateFailure UTXOW]] UTxOState)
initialUTxOState aliceKeep msigs =
  let addresses =
        [(aliceAddr, aliceKeep) | aliceKeep > 0] ++
        map (\(msig, c) ->
               (AddrBase
                (ScriptHashObj $ hashScript msig)
                (ScriptHashObj $ hashScript msig), c)) msigs
  in
  let tx = makeTx (initTxBody addresses)
                  [alicePay, bobPay]
                  Map.empty in
  (txid $ _body tx, applySTS @UTXOW (TRC( (Slot 0
                                           , emptyPParams
                                           , StakeKeys Map.empty
                                           , StakePools Map.empty
                                           , Dms Map.empty)
                                         , _utxoState genesis
                                         , tx)))

testInitialUTXO :: Assertion
testInitialUTXO =
  assertBool s (isRight utxoSt')
  where (_, utxoSt') = initialUTxOState 0 [(aliceOnly, 11000)]
        s = "problem: " ++ show utxoSt'


-- | Start from genesis, consume Alice's and Bob's coins, create an output
-- spendable by Alice if 'aliceKeep' is greater than 0. For each pair of script
-- and coin value in 'lockScripts' create an output of that value locked by the
-- script. Sign the transaction with keys in 'signers'. Then create an
-- transaction that uses the scripts in 'unlockScripts' (and the output for
-- 'aliceKeep' in the case of it being non-zero) to spend all funds back to
-- Alice. Return resulting UTxO state or collected errors
applyTxWithScript
  :: [(MultiSig, Coin)]
  -> [MultiSig]
  -> Wdrl
  -> Coin
  -> [KeyPair]
  -> Either [[PredicateFailure UTXOW]] UTxOState
applyTxWithScript lockScripts unlockScripts wdrl aliceKeep signers = utxoSt'
  where (txId, initUtxo) = initialUTxOState aliceKeep lockScripts
        utxoSt = case initUtxo of
                   Right utxoSt'' -> utxoSt''
                   _                      -> error ("must fail test before"
                                                   ++ show initUtxo)
        txbody = scriptTxBody inputs aliceAddr wdrl (aliceInitCoin + bobInitCoin + sum wdrl)
        inputs = [TxIn txId (fromIntegral n) | n <-
                     [0..length lockScripts - (if aliceKeep > 0 then 0 else 1)]]
                                 -- alice? + scripts
        tx = makeTx
              txbody
              signers
              (Map.fromList $ map (\scr -> (hashScript scr, scr)) unlockScripts)
        utxoSt' = applySTS @UTXOW (TRC( (Slot 0
                                        , emptyPParams
                                        , StakeKeys Map.empty
                                        , StakePools Map.empty
                                        , Dms Map.empty)
                                      , utxoSt
                                      , tx))

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
        bobOnly = singleKeyOnly bobAddr

testRwdAliceSignsAlone'' :: Assertion
testRwdAliceSignsAlone'' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly, bobOnly] (Map.singleton (RewardAcnt (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [alicePay, bobPay]
        bobOnly = singleKeyOnly bobAddr
        s = "problem: " ++ show utxoSt'

testRwdAliceSignsAlone''' :: Assertion
testRwdAliceSignsAlone''' =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW]]
  where utxoSt' =
          applyTxWithScript [(aliceOnly, 11000)] [aliceOnly] (Map.singleton (RewardAcnt (ScriptHashObj $ hashScript bobOnly)) 1000) 0 [alicePay, bobPay]
        bobOnly = singleKeyOnly bobAddr
