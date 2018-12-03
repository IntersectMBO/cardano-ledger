{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad           (foldM)
import qualified Data.Map                as Map
import           Data.MultiSet           (unions, fromSet, occur, filter, size)
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
import           LedgerState             (DelegationState (..),
                                          LedgerState (..),
                                          ValidationError (..),
                                          LedgerValidation(..),
                                          asStateTransition, emptyDelegation,
                                          mkRwdAcnt, genesisId, genesisState)
import           UTxO
import           PrtlConsts

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

pcs :: PrtlConsts
pcs = PrtlConsts 1 1 100 250 0.25 0.001

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

bobInitCoin :: Coin
bobInitCoin = Coin 1000

genesis :: LedgerState
genesis = genesisState
            pcs
            [ TxOut aliceAddr aliceInitCoin
            , TxOut bobAddr bobInitCoin ]

stakePoolKey1 :: KeyPair
stakePoolKey1 = keyPair (Owner 5)

ledgerState :: [TxWits] -> Either [ValidationError] LedgerState
ledgerState = foldM (asStateTransition (Slot 0)) genesis


testLedgerValidTransactions ::
  Either [ValidationError] LedgerState -> Map.Map TxIn TxOut -> Assertion
testLedgerValidTransactions ls utxo =
    ls @?= Right (LedgerState
                     (UTxO utxo)
                     LedgerState.emptyDelegation
                     pcs)

testValidStakeKeyRegistration ::
  TxWits -> Map.Map TxIn TxOut -> DelegationState -> Assertion
testValidStakeKeyRegistration tx utxo stakeKeyRegistration =
  let
    ls2 = ledgerState [tx]
  in ls2 @?= Right (LedgerState
                     (UTxO utxo)
                     stakeKeyRegistration
                     pcs)

testValidDelegation ::
  [TxWits] -> Map.Map TxIn TxOut -> DelegationState -> StakePool -> Assertion
testValidDelegation txs utxo stakeKeyRegistration pool =
  let
    ls2 = ledgerState txs
    poolhk = hashKey $ vKey stakePoolKey1
  in ls2 @?= Right (LedgerState
                     (UTxO utxo)
                     stakeKeyRegistration
                     { getDelegations =
                         Map.fromList [(hashKey $ vKey aliceStake, poolhk)]
                     , getStPools = Map.fromList [(poolhk, Slot 0)]
                     , getPParams = Map.fromList [(poolhk, pool)]
                     }
                     pcs)

aliceGivesBobLovelace :: TxIn -> Coin -> Coin -> Coin -> Coin ->
  Set.Set DCert -> Slot -> TxWits
aliceGivesBobLovelace txin coin txfee txdeps txrefs cs s = TxWits txbody wit
  where
    aliceCoin = aliceInitCoin + txrefs - (coin + txfee + txdeps)
    txbody = Tx
               (Set.fromList [txin])
               [ TxOut aliceAddr aliceCoin
               , TxOut bobAddr coin ]
               cs
               txfee
               s
    wit = Set.fromList [makeWitness alicePay txbody]

tx1 :: TxWits
tx1 = aliceGivesBobLovelace
        (TxIn genesisId 0)
        (Coin 3000) (Coin 600) (Coin 0) (Coin 0)
        Set.empty (Slot 0)

utxo1 :: Map.Map TxIn TxOut
utxo1 = Map.fromList
       [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
       , (TxIn (txid $ body tx1) 0, TxOut aliceAddr (Coin 6400))
       , (TxIn (txid $ body tx1) 1, TxOut bobAddr (Coin 3000)) ]

ls1 :: Either [ValidationError] LedgerState
ls1 = ledgerState [tx1]

tx2 :: TxWits
tx2 = aliceGivesBobLovelace
        (TxIn genesisId 0)
        (Coin 3000) (Coin 1300) (Coin 3*100) (Coin 0)
        (Set.fromList
          [ RegKey $ vKey aliceStake
          , RegKey $ vKey bobStake
          , RegKey $ vKey stakePoolKey1
        ])
        (Slot 100)


utxo2 :: Map.Map TxIn TxOut
utxo2 = Map.fromList
       [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
       , (TxIn (txid $ body tx2) 0, TxOut aliceAddr (Coin 5400))
       , (TxIn (txid $ body tx2) 1, TxOut bobAddr (Coin 3000)) ]

tx3Body :: Tx
tx3Body = Tx
          (Set.fromList [TxIn (txid $ body tx2) 0])
          [ TxOut aliceAddr (Coin 3950) ]
          (Set.fromList
            [ RegPool stakePool
            , Delegate (Delegation (vKey aliceStake) (vKey stakePoolKey1))
            ])
          (Coin 1200)
          (Slot 100)

tx3 :: TxWits
tx3 = TxWits tx3Body (Set.fromList [makeWitness alicePay tx3Body])

utxo3 :: Map.Map TxIn TxOut
utxo3 = Map.fromList
       [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
       , (TxIn (txid tx3Body) 0, TxOut aliceAddr (Coin 3950))
       , (TxIn (txid $ body tx2) 1, TxOut bobAddr (Coin 3000)) ]

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

stakePoolUpate :: StakePool
stakePoolUpate = StakePool
                   {
                     poolPubKey = vKey stakePoolKey1
                   , poolPledges = Map.empty
                   , poolCost = Coin 100      -- TODO: what is a sensible value?
                   , poolMargin = 1 % 2     --          or here?
                   , poolAltAcnt = Nothing  --          or here?
                   }

tx4Body :: Tx
tx4Body = Tx
          (Set.fromList [TxIn (txid $ body tx3) 0])
          [ TxOut aliceAddr (Coin 2950) ] -- Note the deposit is not charged
          (Set.fromList [ RegPool stakePoolUpate ])
          (Coin 1000)
          (Slot 100)

tx4 :: TxWits
tx4 = TxWits tx4Body (Set.fromList [makeWitness alicePay tx4Body])

utxo4 :: Map.Map TxIn TxOut
utxo4 = Map.fromList
       [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
       , (TxIn (txid tx4Body) 0, TxOut aliceAddr (Coin 2950))
       , (TxIn (txid $ body tx2) 1, TxOut bobAddr (Coin 3000)) ]



testsValidLedger :: TestTree
testsValidLedger =
  testGroup "Tests with valid transactions in ledger."
    [ testCase "Valid Ledger - Alice gives Bob 3000 of her 10000 lovelace" $
        testLedgerValidTransactions ls1 utxo1
      , testGroup "Tests for stake delegation."
          [ testCase "Valid stake key registration." $
              testValidStakeKeyRegistration tx2 utxo2 stakeKeyRegistration1
          , testCase "Valid stake delegation from Alice to stake pool." $
              testValidDelegation [tx2, tx3] utxo3 stakeKeyRegistration1 stakePool
          , testCase "Update stake pool parameters" $
              testValidDelegation [tx2, tx3, tx4] utxo4 stakeKeyRegistration1 stakePoolUpate
          ]
    ]

testSpendNonexistentInput :: Assertion
testSpendNonexistentInput =
  let
    tx = aliceGivesBobLovelace
           (TxIn genesisId 42)
           (Coin 3000) (Coin 1500) (Coin 0) (Coin 0)
           Set.empty (Slot 100)
  in ledgerState [tx] @?=
       Left [ BadInputs
            , ValueNotConserved (Coin 0) (Coin 10000)
            , InsufficientWitnesses]
  -- Note that BadInputs implies InsufficientWitnesses

testWitnessNotIncluded :: Assertion
testWitnessNotIncluded =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 6434)
              , TxOut bobAddr (Coin 3000) ]
              Set.empty
              (Coin 566)
              (Slot 100)
    tx = TxWits txbody Set.empty
  in ledgerState [tx] @?= Left [InsufficientWitnesses]

testSpendNotOwnedUTxO :: Assertion
testSpendNotOwnedUTxO =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 232)]
              Set.empty
              (Coin 768)
              (Slot 100)
    aliceWit = makeWitness alicePay txbody
    tx = TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?= Left [InsufficientWitnesses]

testInvalidTransaction :: Assertion
testInvalidTransaction =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 230)]
              Set.empty
              (Coin 770)
              (Slot 100)
    tx2body = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 19230)]
              Set.empty
              (Coin 770)
              (Slot 100)
    aliceWit = makeWitness alicePay tx2body
    tx = TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?= Left [InsufficientWitnesses]

testEmptyInputSet :: Assertion
testEmptyInputSet =
  let
    txbody = Tx
              Set.empty
              [ TxOut aliceAddr (Coin 1)]
              Set.empty
              (Coin 584)
              (Slot 100)
    aliceWit = makeWitness alicePay txbody
    tx = TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?=
       Left [ InputSetEmpty
            , ValueNotConserved (Coin 0) (Coin 585)
            , InsufficientWitnesses]

testFeeTooSmall :: Assertion
testFeeTooSmall =
  let
    tx = aliceGivesBobLovelace
           (TxIn genesisId 0)
           (Coin 3000) (Coin 1) (Coin 0) (Coin 0)
           Set.empty (Slot 100)
  in ledgerState [tx] @?=
       Left [ FeeTooSmall (Coin 538) (Coin 1) ]

testExpiredTx :: Assertion
testExpiredTx =
  let
    tx = aliceGivesBobLovelace
           (TxIn genesisId 0)
           (Coin 3000) (Coin 600) (Coin 0) (Coin 0)
           Set.empty (Slot 0)
  in (asStateTransition (Slot 1) genesis tx) @?=
       Left [ Expired (Slot 0) (Slot 1) ]

testsInvalidLedger :: TestTree
testsInvalidLedger = testGroup "Tests with invalid transactions in ledger"
  [ testCase "Invalid Ledger - Alice tries to spend a nonexistent input" testSpendNonexistentInput
  , testCase "Invalid Ledger - Alice does not include a witness" testWitnessNotIncluded
  , testCase "Invalid Ledger - Alice tries to spend Bob's UTxO" testSpendNotOwnedUTxO
  , testCase "Invalid Ledger - Alice provides witness of wrong UTxO" testInvalidTransaction
  , testCase "Invalid Ledger - Alice's transaction does not consume input" testEmptyInputSet
  , testCase "Invalid Ledger - Alice's fee is too small" testFeeTooSmall
  , testCase "Invalid Ledger - Alice's transaciton has expired" testExpiredTx
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
      (_, steps, fees, ls, _, next)  <- forAll genNonEmptyAndAdvanceTx
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
  (l, steps, txfee, txwits, l')  <- forAll genValidStateTx
  let tx                       = body txwits
  let inps                     = txins tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  (balance $ inps <| (getUtxo l)) === ((balance $ txouts tx) <> txfee)

-- | Property 7.3 (Preserve Outputs of Transaction)
propPreserveOutputs :: Cover
propPreserveOutputs = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = body txwits
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  True === Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ getUtxo l')

-- | Property 7.4 (Eliminate Inputs of Transaction)
propEliminateInputs :: Cover
propEliminateInputs = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = body txwits
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  -- no element of 'txins tx' is a key in the 'UTxO' of l'
  Map.empty === Map.restrictKeys (utxoMap $ getUtxo l') (txins tx)

-- | Property 7.5 (Completeness and Collision-Freeness of new TxIds)
propUniqueTxIds :: Cover
propUniqueTxIds = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = body txwits
  let origTxIds             = collectIds <$> (Map.keys $ utxoMap (getUtxo l))
  let newTxIds              = collectIds <$> (Map.keys $ utxoMap (txouts tx))
  let txId                  = txid tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  True === ((all (== txId) newTxIds) &&
            (not $ any (== txId) origTxIds) &&
            Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ getUtxo l'))
         where collectIds (TxIn txId _) = txId

-- | Property checks no double spend occurs in the currently generated 'TxWits'
-- transactions. Note: this is more a property of the current generator.
propNoDoubleSpend :: Cover
propNoDoubleSpend = Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
      (_, _, _, _, txs, next)  <- forAll genNonEmptyAndAdvanceTx
      case next of
        Left _  -> failure
        Right _ -> do
          let inputIndicesSet = unions $ map (\txwit -> fromSet $ inputs $ body txwit) txs
          0 === (Data.MultiSet.size $ Data.MultiSet.filter
                     (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
                     inputIndicesSet)

-- | Classify mutated transaction into double-spends (validated and
-- non-validated). This is a property of the validator, i.e., no validated
-- transaction should ever be able to do a double spend.
classifyInvalidDoubleSpend :: Cover
classifyInvalidDoubleSpend = Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
      (_, _, _, _, txs, LedgerValidation validationErrors _)
          <- forAll genNonEmptyAndAdvanceTx'
      let inputIndicesSet  = unions $ map (\txwit -> fromSet $ inputs $ body txwit) txs
      let multiSpentInputs = (Data.MultiSet.size $ Data.MultiSet.filter
                                   (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
                                   inputIndicesSet)
      let isMultiSpend = 0 < multiSpentInputs
      classify (isMultiSpend && validationErrors == []) "multi-spend, validation OK"
      classify (isMultiSpend && validationErrors /= []) "multi-spend, validation KO"
      classify (isMultiSpend) "multi-spend"
      True === ((not isMultiSpend) || validationErrors /= [])

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
                  , testPropertyCoverage
                    "No Double Spend in valid ledger states"
                    propNoDoubleSpend
                  ]
                , testGroup "Property tests with mutated transactions"
                  [testPropertyCoverage
                   "preserve balance of change in UTxO"
                   propBalanceTxInTxOut'
                  , testPropertyCoverage
                    "Classify double spend"
                    classifyInvalidDoubleSpend
                  ]
                ]

-- | Mutations for Property 7.2
propBalanceTxInTxOut' :: Cover
propBalanceTxInTxOut' =
  Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
  (l, _, txfee, txwits, lv)  <- forAll genStateTx
  let tx                       = body txwits
  let inps                     = txins tx
  let getErrors (LedgerValidation valErrors _) = valErrors
  let balanceSource            = balance $ inps <| (getUtxo l)
  let balanceTarget            = (balance $ txouts tx)
  let valErrors                = getErrors lv
  let nonTrivial               =  balanceSource /= Coin 0
  let balanceOk                = balanceSource == balanceTarget <> txfee
  classify (valErrors /= [] && balanceOk && nonTrivial) "non-valid, OK"
  if valErrors /= [] && balanceOk && nonTrivial
  then label (pack (  "inputs: "       ++ (show $ Set.size (inputs tx))
                     ++ " outputs: "   ++ (show $ length (outputs tx))
                     ++ " balance l "  ++ (show balanceSource)
                     ++ " balance l' " ++ (show balanceTarget)
                     ++ " txfee " ++ show txfee
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
