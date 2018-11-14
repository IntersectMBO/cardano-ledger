{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad           (foldM)
import qualified Data.Map                as Map
import           Data.Ratio
import qualified Data.Set                as Set
import           Data.Text               (pack)

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hedgehog.Coverage
import           Test.Tasty.HUnit

import           Hedgehog

import           Generator

import           Coin
import           Slot
import           Keys
import           LedgerState             (DelegationState (..), Ledger,
                                          LedgerEntry (..), LedgerState (..),
                                          ValidationError (..),
                                          LedgerValidation(..),
                                          asStateTransition, emptyDelegation,
                                          mkRwdAcnt, genesisId, genesisState)
import           UTxO

import           Delegation.Certificates (DCert (..))
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
ledgerState = foldM (asStateTransition (Slot 0)) genesis


testLedgerValidTransactions ::
  Either [ValidationError] LedgerState -> Map.Map TxIn TxOut -> Assertion
testLedgerValidTransactions ls utxo =
    ls @?= Right (LedgerState
                     (UTxO utxo)
                     LedgerState.emptyDelegation (Epoch 0))

testValidStakeKeyRegistration ::
  [LedgerEntry] -> Map.Map TxIn TxOut -> DelegationState -> Assertion
testValidStakeKeyRegistration sd utxo stakeKeyRegistration =
  let
    ls2 = ledgerState sd
  in ls2 @?= Right (LedgerState
                     (UTxO utxo)
                     stakeKeyRegistration
                     (Epoch 0))

testValidDelegation ::
  [LedgerEntry] -> Map.Map TxIn TxOut -> DelegationState -> StakePool -> Assertion
testValidDelegation sd utxo stakeKeyRegistration sp =
  let
    stakeDelegation = Delegate (Delegation (vKey aliceStake) (vKey stakePoolKey1))
    ls2 = ledgerState $ sd ++ [ DelegationData poolRegistration
                               , DelegationData stakeDelegation]
    poolRegistration = RegPool sp
    poolhk = hashKey $ vKey stakePoolKey1

  in ls2 @?= Right (LedgerState
                     (UTxO utxo)
                     stakeKeyRegistration
                     {
                       getDelegations =
                         Map.fromList [(hashKey $ vKey aliceStake, poolhk)]
                     , getStPools = Map.fromList [(poolhk, (sp, Slot 0))]
                     }
                     (Epoch 0))

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

certAlice :: DCert
certAlice = RegKey $ vKey aliceStake
certBob :: DCert
certBob = RegKey $ vKey bobStake
certPool1 :: DCert
certPool1 = RegKey $ vKey stakePoolKey1

sd1 :: [LedgerEntry]
sd1 = [ DelegationData certAlice
      , DelegationData certBob
      , DelegationData certPool1]

stakeKeyRegistration1 :: DelegationState
stakeKeyRegistration1 = LedgerState.emptyDelegation
  {
    getAccounts =
      Map.fromList [ (mkRwdAcnt aliceStake, Coin 0)
                   , (mkRwdAcnt bobStake, Coin 0)
                   , (mkRwdAcnt stakePoolKey1, Coin 0)]
  , getStKeys =
      Map.fromList [ (hashKey $ vKey aliceStake, Slot 0)
                   , (hashKey $ vKey bobStake, Slot 0)
                   , (hashKey $ vKey stakePoolKey1, Slot 0)]
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

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testsValidLedger, testsInvalidLedger ]

-- | Take 'addr |-> c' pair from 'TxOut' and insert into map or add 'c' to value
-- already present. Used to fold over 'UTxO' to accumulate funds per address.
insertOrUpdate :: TxOut -> Map.Map Addr Coin -> Map.Map Addr Coin
insertOrUpdate (TxOut a c) m =
    Map.insert a (if Map.member a m
                  then c <> (m Map.! a)
                  else c) m

-- | Return True if at least half of the keys have non-trivial coin values to
-- spent, i.e., at least 2 coins per 50% of addresses.
isNotDustDist :: UTxO -> UTxO -> Bool
isNotDustDist initUtxo utxo =
    utxoSize initUtxo <=
           2 * (Map.size $ Map.filter (> Coin 1) coinMap)
        where coinMap = Map.foldr insertOrUpdate Map.empty (utxoMap utxo)

-- | This property states that a non-empty UTxO set in the genesis state has a
-- non-zero balance.
propPositiveBalance:: Property
propPositiveBalance =
    property $ do
      initialState <- forAll genNonemptyGenesisState
      utxoSize (getUtxo initialState) /== 0
      Coin 0 /== balance (getUtxo initialState)

-- | This property states that the balance of the initial genesis state equals
-- the balance of the end ledger state plus the collected fees.
propPreserveBalanceInitTx :: Cover
propPreserveBalanceInitTx =
    withCoverage $ do
      (_, steps, fees, ls, next)  <- forAll genNonEmptyAndAdvanceTx
      classify (steps > 1) "non-trivial number of steps"
      case next of
        Left _    -> failure
        Right ls' -> do
              classify (isNotDustDist (getUtxo ls) (getUtxo ls'))
                           "non-trivial wealth dist"
              balance (getUtxo ls) === balance (getUtxo ls') <> fees

-- | Property 7.2 (Preserve Balance Restricted to TxIns in Balance of TxOuts)
propBalanceTxInTxOut :: Cover
propBalanceTxInTxOut = withCoverage $ do
  (l, steps, fee, entry, l')  <- forAll genValidStateTx
  let tx                       = getTxOfEntry entry
  let inps                     = txins tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  (balance $ inps <| (getUtxo l)) === ((balance $ txouts tx) <> fee)

-- | Property 7.3 (Preserve Outputs of Transaction)
propPreserveOutputs :: Cover
propPreserveOutputs = withCoverage $ do
  (l, steps, _, entry, l') <- forAll genValidStateTx
  let tx                    = getTxOfEntry entry
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  True === Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ getUtxo l')

-- | Property 7.4 (Eliminate Inputs of Transaction)
propEliminateInputs :: Cover
propEliminateInputs = withCoverage $ do
  (l, steps, _, entry, l') <- forAll genValidStateTx
  let tx                    = getTxOfEntry entry
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  -- no element of 'txins tx' is a key in the 'UTxO' of l'
  Map.empty === Map.restrictKeys (utxoMap $ getUtxo l') (txins tx)

-- | Property 7.5 (Completeness and Collision-Freeness of new TxIds)
propUniqueTxIds :: Cover
propUniqueTxIds = withCoverage $ do
  (l, steps, _, entry, l') <- forAll genValidStateTx
  let tx                    = getTxOfEntry entry
  let origTxIds             = collectIds <$> (Map.keys $ utxoMap (getUtxo l))
  let newTxIds              = collectIds <$> (Map.keys $ utxoMap (txouts tx))
  let txId                  = txid tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  True === ((all (== txId) newTxIds) &&
            (not $ any (== txId) origTxIds) &&
            Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ getUtxo l'))
         where collectIds (TxIn txId _) = txId

-- | 'TestTree' of property-based testing properties.
propertyTests :: TestTree
propertyTests = testGroup "Property-Based Testing"
                [ testGroup "Ledger Genesis State"
                  [testProperty
                    "non-empty genesis ledger state has non-zero balance"
                    propPositiveBalance
                  , testPropertyCoverage
                    "several transaction added to genesis ledger state"
                    propPreserveBalanceInitTx]
                , testGroup "Property tests starting from valid ledger state"
                  [testPropertyCoverage
                    "preserve balance restricted to TxIns in Balance of outputs"
                    propBalanceTxInTxOut
                  , testPropertyCoverage
                    "Preserve outputs of transaction"
                    propPreserveOutputs
                  , testPropertyCoverage
                    "Eliminate Inputs of Transaction"
                    propEliminateInputs
                  , testPropertyCoverage
                    "Completeness and Collision-Freeness of new TxIds"
                    propUniqueTxIds
                  ]
                , testGroup "Property tests with mutated transactions"
                  [testPropertyCoverage
                   "preserve balance of change in UTxO"
                   propBalanceTxInTxOut'
                  ]
                ]

-- | Mutations for Property 7.2
propBalanceTxInTxOut' :: Cover
propBalanceTxInTxOut' =
  Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
  (l, _, fee, entry, lv)  <- forAll genStateTx
  let tx                       = getTxOfEntry entry
  let inps                     = txins tx
  let getErrors (LedgerValidation valErrors _) = valErrors
  let balanceSource            = balance $ inps <| (getUtxo l)
  let balanceTarget            = (balance $ txouts tx)
  let valErrors                = getErrors lv
  let nonTrivial               =  balanceSource /= Coin 0
  let balanceOk                = balanceSource == balanceTarget <> fee
  classify (valErrors /= [] && balanceOk && nonTrivial) "non-valid, OK"
  if valErrors /= [] && balanceOk && nonTrivial
  then label (pack (  "inputs: "       ++ (show $ Set.size (inputs tx))
                     ++ " outputs: "   ++ (show $ length (outputs tx))
                     ++ " balance l "  ++ (show balanceSource)
                     ++ " balance l' " ++ (show balanceTarget)
                     ++ " fee " ++ show fee
                     ++ "\n  validationErrors: " ++ show valErrors))
  else (if valErrors /= [] && balanceOk
        then label ("non-validated, OK, trivial")
        else (if valErrors /= []
              then label ("non-validated, KO")
              else label ("validated")
        ))
  success


tests :: TestTree
tests = testGroup "Ledger with Delegation" [unitTests, propertyTests]

-- main entry point
main :: IO ()
main = defaultMain tests
