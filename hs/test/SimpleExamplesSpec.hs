module SimpleExamplesSpec where

import           Control.Monad (foldM)
import           Crypto.Hash   (hash)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Test.Hspec

import           LedgerState   (Ledger
                               , LedgerState(..)
                               , DelegationState(..)
                               , LedgerEntry(..)
                               , ValidationError(..)
                               , asStateTransition
                               , emptyDelegation
                               , genesisId
                               , genesisState)
import           UTxO
import           Keys
import           Coin

import           Delegation.Certificates (Cert(..))

alicePay :: KeyPair
alicePay = keyPair (Owner 1)

aliceStake :: KeyPair
aliceStake = keyPair (Owner 2)

aliceAddr :: Addr
aliceAddr = AddrTxin (hash (vKey alicePay)) (hash (vKey aliceStake))

bobPay :: KeyPair
bobPay = keyPair (Owner 3)

bobStake :: KeyPair
bobStake = keyPair (Owner 4)

bobAddr :: Addr
bobAddr = AddrTxin (hash (vKey bobPay)) (hash (vKey bobStake))

genesis :: LedgerState
genesis = genesisState
            [ TxOut aliceAddr (Coin 10)
            , TxOut bobAddr (Coin 1) ]

stakePoolKey1 :: KeyPair
stakePoolKey1 = keyPair (Owner 5)

stakePoolKey2 :: KeyPair
stakePoolKey2 = keyPair (Owner 6)

ledgerState :: Ledger -> Either [ValidationError] LedgerState
ledgerState = foldM asStateTransition genesis



testLedgerValidTransactions ::
  Either [ValidationError] LedgerState -> Map.Map TxIn TxOut -> SpecWith ()
testLedgerValidTransactions ls1 utxo =
  it "Valid Ledger - Alice gives Bob 3 of her 10 coins" $ do
  ls1 `shouldBe` Right (LedgerState
                         (UTxO utxo)
                         LedgerState.emptyDelegation 0)

testValidStakeKeyRegistration ::
  LedgerEntry -> [LedgerEntry] -> Map.Map TxIn TxOut -> SpecWith ()
testValidStakeKeyRegistration tx1 sd1 utxo =
  it "Valid stake key registration." $ do
  let
    ls2 = ledgerState $ tx1:sd1
  ls2 `shouldBe` Right (LedgerState
                        (UTxO utxo)
                        LedgerState.emptyDelegation
                        {
                          getAccounts =
                            Map.fromList [ (hashKey $ vKey aliceStake, Coin 0)
                                         , (hashKey $ vKey bobStake, Coin 0)
                                         , (hashKey $ vKey stakePoolKey1, Coin 0)]
                        , getStKeys =
                            Set.fromList [ hashKey $ vKey aliceStake
                                         , hashKey $ vKey bobStake
                                         , hashKey $ vKey stakePoolKey1]
                        } 0)

testsValidLedger :: SpecWith ()
testsValidLedger = describe "Tests with valid transactions in ledger." $ do
    let
      tx1Body = Tx
                (Set.fromList [TxIn genesisId 0])
                [ TxOut aliceAddr (Coin 7)
                , TxOut bobAddr (Coin 3) ]
                Set.empty
      aliceTx1Wit = makeWitness alicePay tx1Body
      tx1 = TransactionData $ TxWits tx1Body (Set.fromList [aliceTx1Wit])
      tx1id = txid tx1Body
      utxo = Map.fromList
             [ (TxIn genesisId 1, TxOut bobAddr (Coin 1))
             , (TxIn tx1id 0, TxOut aliceAddr (Coin 7))
             , (TxIn tx1id 1, TxOut bobAddr (Coin 3)) ]
      ls1 = ledgerState [tx1]

    testLedgerValidTransactions ls1 utxo

    describe "Tests for stake delegation." $ do
      let
        certAlice = RegKey $ vKey aliceStake
        certBob = RegKey $ vKey bobStake
        certPool1 = RegKey $ vKey stakePoolKey1
        sd1 = [ DelegationData certAlice
              , DelegationData certBob
              , DelegationData certPool1]
      testValidStakeKeyRegistration tx1 sd1 utxo

testSpendNonexistentInput :: SpecWith ()
testSpendNonexistentInput =
  it "Invalid Ledger - Alice tries to spend a nonexistent input" $ do
  let
    tx1Body = Tx
              (Set.fromList [TxIn genesisId 42])
              [ TxOut aliceAddr (Coin 0) ]
              Set.empty
    aliceTx1Wit = makeWitness alicePay tx1Body
    tx1 = TransactionData $ TxWits tx1Body (Set.fromList [aliceTx1Wit])
  ledgerState [tx1] `shouldBe` Left [BadInputs, InsuffientWitnesses]
  -- Note that BadInputs implies InsuffientWitnesses

testWitnessNotIncluded :: SpecWith ()
testWitnessNotIncluded =
  it "Invalid Ledger - Alice does not include a witness" $ do
  let
    tx1Body = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 7)
              , TxOut bobAddr (Coin 3) ]
              Set.empty
    tx1 = TransactionData $ TxWits tx1Body Set.empty
  ledgerState [tx1] `shouldBe` Left [InsuffientWitnesses]

testSpendNotOwnedUTxO :: SpecWith ()
testSpendNotOwnedUTxO =
  it "Invalid Ledger - Alice tries to spend Bob's UTxO" $ do
  let
    tx1Body = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 1)]
              Set.empty
    aliceTx1Wit = makeWitness alicePay tx1Body
    tx1 = TransactionData $ TxWits tx1Body (Set.fromList [aliceTx1Wit])
  ledgerState [tx1] `shouldBe` Left [InsuffientWitnesses]

testInvalidTransaction :: SpecWith ()
testInvalidTransaction =
  it "Invalid Ledger - Alice provides witness of wrong UTxO" $ do
  let
    tx1Body = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 1)]
              Set.empty
    tx2Body = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 10)]
              Set.empty
    aliceTx1Wit = makeWitness alicePay tx2Body
    tx1 = TransactionData $ TxWits tx1Body (Set.fromList [aliceTx1Wit])
  ledgerState [tx1] `shouldBe` Left [InsuffientWitnesses]

testsInvalidLedger :: SpecWith ()
testsInvalidLedger = describe "Tests with invalid transactions in ledger" $ do
  testSpendNonexistentInput
  testWitnessNotIncluded
  testSpendNotOwnedUTxO
  testInvalidTransaction

spec :: Spec
spec = do
  testsValidLedger
  testsInvalidLedger
