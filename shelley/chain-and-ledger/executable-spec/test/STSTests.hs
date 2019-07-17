{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module STSTests (stsTests) where

import           Data.Either (isLeft, isRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, singleton)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import qualified Data.Set as Set
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=), assertBool)

import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Cardano.Crypto.DSIGN (genKeyDSIGN, deriveVerKeyDSIGN)
import           Crypto.Random (drgNewTest, withDRG)
import           MockTypes (CHAIN, SKeyES, VKeyES, KeyPair, VKey, SKey, Addr,
                            MultiSig, Tx, TxBody, LedgerState,
                            ScriptHash, UTXOW, UTxOState, TxId, TxIn)

import           BaseTypes (Seed (..), mkUnitInterval)
import           Coin
import           BlockChain (pattern BHBody, pattern BHeader, pattern Block, pattern Proof, bhbHash)
import           Control.State.Transition (TRC (..), applySTS, PredicateFailure)
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..))
import           Keys (pattern KeyPair, pattern SKeyES, pattern VKeyES, sKey, sign, signKES, vKey, pattern SKey, pattern VKey, hashKey, pattern Dms)
import           LedgerState (pattern NewEpochState, emptyEpochState, _utxoState,
                              genesisState, genesisId)
import           OCert (KESPeriod (..), pattern OCert)
import           Slot (Epoch (..), Slot (..))
import           STS.Updn (UPDN)
import           STS.Utxow (PredicateFailure(..))
import           Tx (hashScript)
import           TxData (pattern AddrVKey, pattern AddrScr, pattern SingleSig,
                         pattern MultiSig, pattern TxBody, pattern TxOut,
                         pattern TxIn, pattern Tx, pattern StakeKeys,
                         pattern StakePools, _body)
import           UTxO (makeWitnessesVKey, txid)
import           PParams(emptyPParams)
import           Updates(emptyUpdate)

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

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKeyES, VKeyES)
mkKESKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyKES 90
  return (SKeyES sk, VKeyES $ deriveVerKeyKES sk)

-- | This is a very simple test demonstrating that we have everything in place
-- in order to run the CHAIN STS transition.
-- TODO replace this test with one that does more than just apply the rule.
testApplyChain :: Assertion
testApplyChain =
  let
    initChainSt =
      ( NewEpochState
          (Epoch 0)
          (Nonce 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          emptyEpochState
          Nothing
          (PoolDistr Map.empty)
          Map.empty
      , Nonce 0
      , Nonce 0
      , Nothing
      , Slot 0
      )
    kp = KeyPair 1 1
    half = fromMaybe (error "could not construct unit interval") $ mkUnitInterval 0.5
    (sKeyES, vKeyES) = mkKESKeyPair (0, 0, 0, 0, 0)
    bhb = BHBody
            Nothing
            (vKey kp)
            (Slot 0)
            (Nonce 0)
            (Proof (vKey kp) (Nonce 0))
            half
            (Proof (vKey kp) half)
            (sign (sKey kp) [])
            100
            (bhbHash [])
            (OCert
              vKeyES
              (vKey kp)
              0
              (KESPeriod 0)
              (sign (sKey kp) (vKeyES, 0, KESPeriod 0))
            )
    block = Block (BHeader bhb (Keys.signKES sKeyES bhb 0)) []
    newSt = applySTS @CHAIN (TRC (Slot 0, initChainSt, block))
  in
    isLeft newSt @?= True

stsTests :: TestTree
stsTests = testGroup "STS Tests"
  [ testCase "update nonce early in the epoch" testUPNEarly
  , testCase "update nonce late in the epoch" testUPNLate
  , testCase "apply CHAIN transition" testApplyChain
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

aliceAddr :: Addr
aliceAddr = AddrVKey (hashKey (vKey alicePay)) (hashKey (vKey aliceStake))


bobPay :: KeyPair
bobPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

bobStake :: KeyPair
bobStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

bobAddr :: Addr
bobAddr = AddrVKey (hashKey (vKey bobPay)) (hashKey (vKey bobStake))


carlPay :: KeyPair
carlPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

carlStake :: KeyPair
carlStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (5, 5, 5, 5, 5)

carlAddr :: Addr
carlAddr = AddrVKey (hashKey (vKey carlPay)) (hashKey (vKey carlStake))


dariaPay :: KeyPair
dariaPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

dariaStake :: KeyPair
dariaStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

dariaAddr :: Addr
dariaAddr = AddrVKey (hashKey (vKey dariaPay)) (hashKey (vKey dariaStake))

-- Multi-signature scripts
singleKeyOnly :: Addr -> MultiSig
singleKeyOnly (AddrVKey pk _ ) = SingleSig pk
singleKeyOnly _ = error "use VKey address"

aliceOnly :: MultiSig
aliceOnly = singleKeyOnly aliceAddr

aliceOrBob :: MultiSig
aliceOrBob = MultiSig 1 [aliceOnly, singleKeyOnly bobAddr]

aliceAndBob :: MultiSig
aliceAndBob = MultiSig 2 [aliceOnly, singleKeyOnly bobAddr]

aliceAndBobOrCarl :: MultiSig
aliceAndBobOrCarl = MultiSig 1 [aliceAndBob, singleKeyOnly carlAddr]

aliceAndBobOrCarlAndDaria :: MultiSig
aliceAndBobOrCarlAndDaria =
  MultiSig 1 [aliceAndBob, MultiSig 2 [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]]

aliceAndBobOrCarlOrDaria :: MultiSig
aliceAndBobOrCarlOrDaria =
  MultiSig 1 [aliceAndBob, MultiSig 1 [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]]


initTxBody :: Addr -> Coin -> Coin -> Coin -> Coin -> TxBody
initTxBody addr keepAlice keepBob fromAlice fromBob  = TxBody
        (Set.fromList [TxIn genesisId 0, TxIn genesisId 1])
        [ TxOut addr (fromAlice + fromBob)
        , TxOut aliceAddr keepAlice
        , TxOut bobAddr keepBob]
        []
        Map.empty
        (Coin 0)
        (Slot 0)
        emptyUpdate

scriptTxBody :: TxIn -> Addr -> Coin -> TxBody
scriptTxBody inp addr c =
  TxBody
    (Set.fromList [inp])
    [TxOut addr c]
    []
    Map.empty
    (Coin 0)
    (Slot 10)
    emptyUpdate

makeTx :: TxBody -> [KeyPair] -> Map ScriptHash MultiSig -> Tx
makeTx txBody keyPairs scriptWitnesses =
  Tx txBody (makeWitnessesVKey txBody keyPairs) scriptWitnesses

aliceInitCoin :: Coin
aliceInitCoin = 10000

bobInitCoin :: Coin
bobInitCoin = 1000

genesis :: LedgerState
genesis = genesisState
           emptyPParams
           [ TxOut aliceAddr aliceInitCoin
           , TxOut bobAddr bobInitCoin]

initialUTxOState :: MultiSig -> (TxId, Either [[PredicateFailure UTXOW]] UTxOState)
initialUTxOState msig =
  let scriptAddress = AddrScr (hashScript msig) (hashScript msig) in
  let tx = makeTx (initTxBody scriptAddress 0 0 aliceInitCoin bobInitCoin)
                  [alicePay, bobPay]
                  (Map.singleton (hashScript msig) msig) in
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
  where (_, utxoSt') = initialUTxOState aliceOnly
        s = "problem: " ++ show utxoSt'


-- | Start from genesis, consume Alice's and Bob's coins, create an output
-- locked by 'lockedScript', sign the transaction with keys in 'signers'. Then
-- create an transaction that uses 'unlockScript' to spend all funds back to
-- Alice. Return resulting UTxO state or collected errors
applyTxWithScript
  :: MultiSig
  -> MultiSig
  -> [KeyPair]
  -> Either [[PredicateFailure UTXOW]] UTxOState
applyTxWithScript lockScript unlockScript signers = utxoSt'
  where (txId, initUtxo) = initialUTxOState lockScript
        utxoSt = case initUtxo of
                   Right utxoSt'' -> utxoSt''
                   _                      -> error "must fail test before"
        txbody = scriptTxBody (TxIn txId 0) aliceAddr (aliceInitCoin + bobInitCoin)
        tx = makeTx
              txbody
              signers
              (Map.singleton (hashScript unlockScript) unlockScript)
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
          applyTxWithScript aliceOnly aliceOnly [alicePay]
        s = "problem: " ++ show utxoSt'

testAliceDoesntSign :: Assertion
testAliceDoesntSign =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' =
          applyTxWithScript aliceOnly aliceOnly [bobPay, carlPay, dariaPay]

testEverybodySigns :: Assertion
testEverybodySigns =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceOnly aliceOnly [alicePay, bobPay, carlPay, dariaPay]
        s = "problem: " ++ show utxoSt'

testWrongScript :: Assertion
testWrongScript =
  utxoSt' @?= Left [[MissingScriptWitnessesUTXOW]]
  where utxoSt' =
          applyTxWithScript aliceOnly aliceOrBob [alicePay, bobPay]

testAliceOrBob :: Assertion
testAliceOrBob =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceOrBob aliceOrBob [alicePay]
        s = "problem: " ++ show utxoSt'

testAliceOrBob' :: Assertion
testAliceOrBob' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceOrBob aliceOrBob [bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBob :: Assertion
testAliceAndBob =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBob aliceAndBob [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBob' :: Assertion
testAliceAndBob' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' =
          applyTxWithScript aliceAndBob aliceAndBob [alicePay]

testAliceAndBob'' :: Assertion
testAliceAndBob'' =
  utxoSt' @?= Left [[ScriptWitnessNotValidatingUTXOW]]
  where utxoSt' =
          applyTxWithScript aliceAndBob aliceAndBob [bobPay]

testAliceAndBobOrCarl :: Assertion
testAliceAndBobOrCarl =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBobOrCarl aliceAndBobOrCarl [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarl' :: Assertion
testAliceAndBobOrCarl' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBobOrCarl aliceAndBobOrCarl [carlPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria :: Assertion
testAliceAndBobOrCarlAndDaria =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBobOrCarlAndDaria aliceAndBobOrCarlAndDaria [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlAndDaria' :: Assertion
testAliceAndBobOrCarlAndDaria' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBobOrCarlAndDaria aliceAndBobOrCarlAndDaria [carlPay, dariaPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria :: Assertion
testAliceAndBobOrCarlOrDaria =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBobOrCarlOrDaria aliceAndBobOrCarlOrDaria [alicePay, bobPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria' :: Assertion
testAliceAndBobOrCarlOrDaria' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBobOrCarlOrDaria aliceAndBobOrCarlOrDaria [carlPay]
        s = "problem: " ++ show utxoSt'

testAliceAndBobOrCarlOrDaria'' :: Assertion
testAliceAndBobOrCarlOrDaria'' =
  assertBool s (isRight utxoSt')
  where utxoSt' =
          applyTxWithScript aliceAndBobOrCarlOrDaria aliceAndBobOrCarlOrDaria [dariaPay]
        s = "problem: " ++ show utxoSt'
