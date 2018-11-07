{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad           (foldM)
import qualified Data.Map                as Map
import           Data.Ratio
import qualified Data.Set                as Set
import           Numeric.Natural         (Natural)

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

import           Coin
import           Slot
import           Keys
import           LedgerState             (DelegationState (..), Ledger,
                                          LedgerEntry (..), LedgerState (..),
                                          ValidationError (..),
                                          asStateTransition, emptyDelegation,
                                          getRwdAcnt, genesisId, genesisState)
import           UTxO

import           Delegation.Certificates (DCert (..))
import           Delegation.StakePool    (Delegation (..), StakePool (..))

type KeyPairs = [(KeyPair, KeyPair)]

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
      Map.fromList [ (getRwdAcnt aliceStake, Coin 0)
                   , (getRwdAcnt bobStake, Coin 0)
                   , (getRwdAcnt stakePoolKey1, Coin 0)]
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

-- | Generator for '(Owner, Owner)' pairs, 'fst even', 'snd' is 'fst + 1'
genOwnerList :: Int -> Int -> Gen [(Owner, Owner)]
genOwnerList lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.linear (1 :: Natural) 1000)
  return $ fmap (\n -> (Owner $ 2*n, Owner $2*n+1)) xs

-- | Generates a list of '(pay, stake)' key pairs.
genKeyPairs :: Int -> Int -> Gen KeyPairs
genKeyPairs lower upper =
    fmap (\(a, b) -> (keyPair a, keyPair b))
             <$> genOwnerList lower upper

-- | Hashes all pairs of pay, stake key pairs of a list into a list of pairs of
-- hashed keys
hashKeyPairs :: KeyPairs -> [(HashKey, HashKey)]
hashKeyPairs keyPairs =
    (\(a, b) -> (hashKey $ vKey a, hashKey $ vKey b)) <$> keyPairs

-- | Transforms list of keypairs into 'Addr' types of the form 'AddrTxin pay
-- stake'
addrTxins :: KeyPairs -> [Addr]
addrTxins keyPairs = uncurry AddrTxin <$> hashKeyPairs keyPairs

-- | Generator for a natural number between 'lower' and 'upper'.
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = Gen.integral $ Range.linear lower upper

-- | Generator for a Slot between 'lower' and 'upper'.
genSlot :: Natural -> Natural -> Gen Slot
genSlot lower upper = Slot <$> Gen.integral (Range.linear lower upper)

-- | Generator for List of 'Coin' values. Generates between 'lower' and 'upper'
-- coins, with values between 'minCoin' and 'maxCoin'.
genCoinList :: Natural -> Natural -> Int -> Int -> Gen [Coin]
genCoinList minCoin maxCoin lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.exponential minCoin maxCoin)
  return (Coin <$> xs)

-- | Generator for a list of 'TxOut' where for each 'Addr' of 'addrs' one Coin
-- value is generated.
genTxOut :: [Addr] -> Gen [TxOut]
genTxOut addrs = do
  ys <- genCoinList 1 100 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- | Generator of a non-empty genesis ledger state, i.e., at least one valid
-- address and non-zero UTxO.
genNonemptyGenesisState :: Gen LedgerState
genNonemptyGenesisState = do
  keyPairs <- genKeyPairs 1 10
  genesisState <$> genTxOut (addrTxins keyPairs)

-- | Generator for a new 'LedgerEntry' and fee value for executing the
-- transaction. Selects one valid input from the UTxO, sums up all funds of the
-- address associated to that input, selects a random subsequence of other valid
-- addresses and spends the UTxO. If 'n' addresses are selected to spent 'b'
-- coins, the amount spent to each address is 'div b n' and the fees are set to
-- 'rem b n'.
genTxLedgerEntry :: KeyPairs -> UTxO -> Gen (Coin, LedgerEntry)
genTxLedgerEntry keyList (UTxO m) = do
  -- select payer
  selectedInputs <- Gen.shuffle utxoInputs
  let !selectedAddr    = addr $ head selectedInputs
  let !selectedUTxO    = Map.filter (\(TxOut a _) -> a == selectedAddr) m
  let !selectedKeyPair = findAddrKeyPair selectedAddr keyList
  let !selectedBalance = balance $ UTxO selectedUTxO

  -- select receipients, distribute balance of selected UTxO set
  n <- genNatural 1 10 -- (fromIntegral $ length keyList) -- TODO make this variable, but uses too much RAM atm
  receipients <- take (fromIntegral n) <$> Gen.shuffle keyList
  let realN                = length receipients
  let (perReceipient, fee) = splitCoin selectedBalance (fromIntegral realN)
  let !receipientAddrs      = fmap
          (\(p, d) -> AddrTxin (hashKey $ vKey p) (hashKey $ vKey d)) receipients
  let !txbody = Tx
           (Map.keysSet selectedUTxO)
           ((\r -> TxOut r perReceipient) <$> receipientAddrs)
           Set.empty
  let !txwit = makeWitness selectedKeyPair txbody
  pure (fee, TransactionData (TxWits txbody $ Set.fromList [txwit]))
            where utxoInputs = Map.keys m
                  addr inp   = getTxOutAddr $ m Map.! inp

-- | Generator for new transaction state transition, starting from a
-- 'LedgerState' and using a list of pairs of 'KeyPair'. Returns either the
-- accumulated fees and a resulting ledger state or the 'ValidationError'
-- information in case of an invalid transaction.
genLedgerStateTx :: KeyPairs -> LedgerState ->
                    Gen (Coin, LedgerEntry, Either [ValidationError] LedgerState)
genLedgerStateTx keyList sourceState = do
  let utxo = getUtxo sourceState
  (fee, ledgerEntry) <- genTxLedgerEntry keyList utxo
  slot <- genSlot 0 1000
  pure (fee, ledgerEntry, asStateTransition slot sourceState ledgerEntry)

-- | Generator of a non-emtpy ledger genesis state and a random number of
-- transactions applied to it. Returns the amount of accumulated fees, the
-- initial ledger state and the final ledger state or the validation error if an
-- invalid transaction has been generated.
genNonEmptyAndAdvanceTx
  :: Gen (KeyPairs, Coin, LedgerState, Either [ValidationError] LedgerState)
genNonEmptyAndAdvanceTx = do
  keyPairs    <- genKeyPairs 1 10
  steps       <- Gen.integral $ Range.linear 1 10
  ls          <- genesisState <$> genTxOut (addrTxins keyPairs)
  (fees, ls') <- repeatTx steps keyPairs (Coin 0) ls
  pure (keyPairs, fees, ls, ls')

-- | Generator for a fixed number of 'n' transaction step executions, using the
-- list of pairs of key pairs, the 'fees' coin accumulator, initial ledger state
-- 'ls' and returns the result of the repeated generation and application of
-- transactions.
repeatTx :: Natural -> KeyPairs -> Coin -> LedgerState ->
            Gen (Coin, Either [ValidationError] LedgerState)
repeatTx 0 _ fees ls = pure (fees, Right ls)
repeatTx n !keyPairs !fees !ls = do
  (fee, _, next) <- genLedgerStateTx keyPairs ls
  case next of
    Left  _   -> pure (fees, next)
    Right ls' -> repeatTx (n - 1) keyPairs (fee <> fees) ls'

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findAddrKeyPair :: Addr -> KeyPairs -> KeyPair
findAddrKeyPair (AddrTxin addr _) keyList =
     fst $ head $ filter (\(pay, _) -> addr == (hashKey $ vKey pay)) keyList

-- | Returns the hashed 'addr' part of a 'TxOut'.
getTxOutAddr :: TxOut -> Addr
getTxOutAddr (TxOut addr _) = addr

-- | Returns the number of entries of the UTxO set.
utxoSize :: UTxO -> Int
utxoSize (UTxO m) = Map.size m

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
propPreserveBalanceInitTx :: Property
propPreserveBalanceInitTx =
    property $ do
      (_, fees, ls, next)  <- forAll genNonEmptyAndAdvanceTx
      case next of
        Left _    -> failure
        Right ls' -> balance (getUtxo ls) === balance (getUtxo  ls') <> fees

-- | Generator for arbitrary valid ledger state, discarding any generated
-- invalid one.
genValidLedgerState :: Gen (KeyPairs, LedgerState)
genValidLedgerState = do
  (keyPairs, _, _, newState) <- genNonEmptyAndAdvanceTx
  case newState of
    Left _   -> Gen.discard
    Right ls -> pure (keyPairs, ls)

genValidSuccessorState :: KeyPairs -> LedgerState ->
  Gen (Coin, LedgerEntry, LedgerState)
genValidSuccessorState keyPairs sourceState = do
  (fee, entry, next) <- genLedgerStateTx keyPairs sourceState
  case next of
    Left _   -> Gen.discard
    Right ls -> pure (fee, entry, ls)

genValidStateTx :: Gen (LedgerState, Coin, LedgerEntry, LedgerState)
genValidStateTx = do
  (keyPairs, ls)    <- genValidLedgerState
  (fee, entry, ls') <- genValidSuccessorState keyPairs ls
  pure (ls, fee, entry, ls')

getTxOfEntry :: LedgerEntry -> Tx
getTxOfEntry entry =
  case entry of
    TransactionData wits -> body wits
    _                    -> undefined

-- | Property 7.2 (Preserve Balance Restricted to TxIns in Balance of TxOuts)
propBalanceTxInTxOut :: Property
propBalanceTxInTxOut = property $ do
  (l, fee, entry, _)  <- forAll genValidStateTx
  let tx               = getTxOfEntry entry
  let inps             = txins tx
  (balance $ inps <| (getUtxo l)) === ((balance $ txouts tx) <> fee)

utxoMap :: UTxO -> Map.Map TxIn TxOut
utxoMap (UTxO m) = m

-- | Property 7.3 (Preserve Outputs of Transaction)
propPreserveOutputs :: Property
propPreserveOutputs = property $ do
  (_, _, entry, l') <- forAll genValidStateTx
  let tx             = getTxOfEntry entry
  True === Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ getUtxo l')

-- | Property 7.4 (Eliminate Inputs of Transaction)
propEliminateInputs :: Property
propEliminateInputs = property $ do
  (_, _, entry, l') <- forAll genValidStateTx
  let tx             = getTxOfEntry entry
  -- no element of 'txins tx' is a key in the 'UTxO' of l'
  Map.empty === Map.restrictKeys (utxoMap $ getUtxo l') (txins tx)

-- | Property 7.5 (Completeness and Collision-Freeness of new TxIds)
propUniqueTxIds :: Property
propUniqueTxIds = property $ do
  (l, _, entry, l') <- forAll genValidStateTx
  let tx             = getTxOfEntry entry
  let origTxIds      = collectIds <$> (Map.keys $ utxoMap (getUtxo l))
  let newTxIds       = collectIds <$> (Map.keys $ utxoMap (txouts tx))
  let txId           = txid tx
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
                  , testProperty
                    "several transaction added to genesis ledger state"
                    propPreserveBalanceInitTx]
                , testGroup "Property tests starting from valid ledger state"
                  [testProperty
                    "preserve balance restricted to TxIns in Balance of outputs"
                    propBalanceTxInTxOut
                  , testProperty
                    "Preserve outputs of transaction"
                    propPreserveOutputs
                  , testProperty
                    "Eliminate Inputs of Transaction"
                    propEliminateInputs
                  , testProperty
                    "Completeness and Collision-Freeness of new TxIds"
                    propUniqueTxIds
                  ]
                ]

tests :: TestTree
tests = testGroup "Ledger with Delegation" [unitTests, propertyTests]

-- main entry point
main :: IO ()
main = defaultMain tests
