import           Control.Monad           (foldM)
import qualified Data.Map                as Map
import           Data.Ratio
import qualified Data.Set                as Set

import           Test.Tasty
import           Test.Tasty.HUnit

import           Coin
import           Keys
import           LedgerState             (DelegationState (..), Ledger,
                                          LedgerEntry (..), LedgerState (..),
                                          ValidationError (..),
                                          asStateTransition, emptyDelegation,
                                          genesisId, genesisState)
import           UTxO

import           Delegation.Certificates (Cert (..))
import           Delegation.StakePool    (Delegation (..), StakePool (..))

alicePay :: KeyPair
alicePay = keyPair (Owner 1)

aliceStake :: KeyPair
aliceStake = keyPair (Owner 2)

aliceAddr :: Addr
aliceAddr = AddrTxin (hashKey (vKey alicePay)) (hashKey (vKey aliceStake))

bobPay :: KeyPair
bobPay = keyPair (Owner 3)

bobStake :: KeyPair
bobStake = keyPair (Owner 4)

bobAddr :: Addr
bobAddr = AddrTxin (hashKey (vKey bobPay)) (hashKey (vKey bobStake))

genesis :: LedgerState
genesis = genesisState
            [ TxOut aliceAddr (Coin 10)
            , TxOut bobAddr (Coin 1) ]

stakePoolKey1 :: KeyPair
stakePoolKey1 = keyPair (Owner 5)

ledgerState :: Ledger -> Either [ValidationError] LedgerState
ledgerState = foldM asStateTransition genesis


testLedgerValidTransactions ::
  Either [ValidationError] LedgerState -> Map.Map TxIn TxOut -> Assertion
testLedgerValidTransactions ls utxo =
    ls @?= Right (LedgerState
                     (UTxO utxo)
                     LedgerState.emptyDelegation 0)

testValidStakeKeyRegistration ::
  [LedgerEntry] -> Map.Map TxIn TxOut -> DelegationState -> Assertion
testValidStakeKeyRegistration sd utxo stakeKeyRegistration =
  let
    ls2 = ledgerState sd
  in ls2 @?= Right (LedgerState
                     (UTxO utxo)
                     stakeKeyRegistration
                     0)

testValidDelegation ::
  [LedgerEntry] -> Map.Map TxIn TxOut -> DelegationState -> StakePool -> Assertion
testValidDelegation sd utxo stakeKeyRegistration sp =
  let
    stakeDelegation = Delegate (Delegation (vKey aliceStake) (vKey stakePoolKey1))
    ls2 = ledgerState $ sd ++ [ DelegationData poolRegistration
                               , DelegationData stakeDelegation]
    poolRegistration = RegPool sp

  in ls2 @?= Right (LedgerState
                     (UTxO utxo)
                     stakeKeyRegistration
                     {
                       getDelegations =
                         Map.fromList [(hashKey $ vKey aliceStake,
                                        hashKey $ vKey stakePoolKey1)]
                     , getStPools = Set.fromList [sp]
                     }
                     0)

tx1Body :: Tx
tx1Body = Tx
          (Set.fromList [TxIn genesisId 0])
          [ TxOut aliceAddr (Coin 7)
          , TxOut bobAddr (Coin 3) ]
          Set.empty

aliceTx1Wit :: Wit
aliceTx1Wit = makeWitness alicePay tx1Body

tx1 :: LedgerEntry
tx1 = TransactionData $ TxWits tx1Body (Set.fromList [aliceTx1Wit])

tx1id :: TxId
tx1id = txid tx1Body

utxo1 :: Map.Map TxIn TxOut
utxo1 = Map.fromList
       [ (TxIn genesisId 1, TxOut bobAddr (Coin 1))
       , (TxIn tx1id 0, TxOut aliceAddr (Coin 7))
       , (TxIn tx1id 1, TxOut bobAddr (Coin 3)) ]

ls1 :: Either [ValidationError] LedgerState
ls1 = ledgerState [tx1]

certAlice :: Cert
certAlice = RegKey $ vKey aliceStake
certBob :: Cert
certBob = RegKey $ vKey bobStake
certPool1 :: Cert
certPool1 = RegKey $ vKey stakePoolKey1

sd1 :: [LedgerEntry]
sd1 = [ DelegationData certAlice
      , DelegationData certBob
      , DelegationData certPool1]

stakeKeyRegistration1 :: DelegationState
stakeKeyRegistration1 = LedgerState.emptyDelegation
  {
    getAccounts =
      Map.fromList [ (hashKey $ vKey aliceStake, Coin 0)
                   , (hashKey $ vKey bobStake, Coin 0)
                   , (hashKey $ vKey stakePoolKey1, Coin 0)]
  , getStKeys =
      Set.fromList [ hashKey $ vKey aliceStake
                   , hashKey $ vKey bobStake
                   , hashKey $ vKey stakePoolKey1]
  }

stakePool :: StakePool
stakePool = StakePool
            {
              poolPubKey = vKey stakePoolKey1
            , poolPledges = Map.empty
            , poolCost = Coin 0      -- TODO: what is a sensible value?
            , poolMargin = 0 % 1     --          or here?
            , poolAltAcnt = Nothing  --          or here?
            }

testsValidLedger :: TestTree
testsValidLedger =
  testGroup "Tests with valid transactions in ledger."
    [ testCase "Valid Ledger - Alice gives Bob 3 of her 10 coins" $
        testLedgerValidTransactions ls1 utxo1
      , testGroup "Tests for stake delegation."
          [ testCase "Valid stake key registration." $
              testValidStakeKeyRegistration (tx1:sd1) utxo1 stakeKeyRegistration1
          , testCase "Valid stake delegation from Alice to stake pool." $
              testValidDelegation (tx1:sd1) utxo1 stakeKeyRegistration1 stakePool]
    ]

testSpendNonexistentInput :: Assertion
testSpendNonexistentInput =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 42])
              [ TxOut aliceAddr (Coin 0) ]
              Set.empty
    aliceWit = makeWitness alicePay txbody
    tx = TransactionData $ TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?= Left [BadInputs, InsuffientWitnesses]
  -- Note that BadInputs implies InsuffientWitnesses

testWitnessNotIncluded :: Assertion
testWitnessNotIncluded =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 7)
              , TxOut bobAddr (Coin 3) ]
              Set.empty
    tx = TransactionData $ TxWits txbody Set.empty
  in ledgerState [tx] @?= Left [InsuffientWitnesses]

testSpendNotOwnedUTxO :: Assertion
testSpendNotOwnedUTxO =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 1)]
              Set.empty
    aliceWit = makeWitness alicePay txbody
    tx = TransactionData $ TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?= Left [InsuffientWitnesses]

testInvalidTransaction :: Assertion
testInvalidTransaction =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 1)]
              Set.empty
    tx2Body = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 10)]
              Set.empty
    aliceWit = makeWitness alicePay tx2Body
    tx = TransactionData $ TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?= Left [InsuffientWitnesses]

testsInvalidLedger :: TestTree
testsInvalidLedger = testGroup "Tests with invalid transactions in ledger"
  [ testCase "Invalid Ledger - Alice tries to spend a nonexistent input" testSpendNonexistentInput
  , testCase "Invalid Ledger - Alice does not include a witness" testWitnessNotIncluded
  , testCase "Invalid Ledger - Alice tries to spend Bob's UTxO" testSpendNotOwnedUTxO
  , testCase "Invalid Ledger - Alice provides witness of wrong UTxO" testInvalidTransaction
  ]

tests :: TestTree
tests = testGroup "Ledger with Delegation" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testsValidLedger, testsInvalidLedger ]

main :: IO ()
main = defaultMain tests
