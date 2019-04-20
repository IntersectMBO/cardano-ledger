{-# LANGUAGE OverloadedStrings #-}

module UnitTests (unitTests) where

import           Control.Monad           (foldM)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Ratio
import qualified Data.Set                as Set
import           Data.Maybe              (fromMaybe)

import           Lens.Micro              ((^.), (&), (.~))

import           Test.Tasty
import           Test.Tasty.HUnit

import           BaseTypes
import           Coin
import           Delegation.Certificates (DCert (..), StakePools(..), StakeKeys(..))
import           Delegation.PoolParams   (Delegation (..), PoolParams (..),
                                                     RewardAcnt(..))
import           Keys
import           LedgerState
import           PParams
import           Slot
import           UTxO

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

oneFourth :: UnitInterval
oneFourth =
    fromMaybe (error "could not construct unit interval") $ mkUnitInterval 0.25

testPCs :: PParams
testPCs =
    PParams 1 1 100 250 oneFourth 0.001 interval0 0 (Epoch 0) (0%1, 0) oneFourth 0.001 interval0 interval0 0 0 interval0

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

bobInitCoin :: Coin
bobInitCoin = Coin 1000

genesis :: LedgerState
genesis = genesisState
            testPCs
            [ TxOut aliceAddr aliceInitCoin
            , TxOut bobAddr bobInitCoin ]

changeReward :: LedgerState -> RewardAcnt -> Coin -> LedgerState
changeReward ls acnt c = ls & delegationState . dstate . rewards .~ newAccounts
  where newAccounts = Map.insert acnt c (ls ^. delegationState . dstate. rewards)

stakePoolKey1 :: KeyPair
stakePoolKey1 = keyPair (Owner 5)

ledgerState :: [TxWits] -> Either [ValidationError] LedgerState
ledgerState = foldM (asStateTransition (Slot 0)) genesis


testLedgerValidTransactions ::
  Either [ValidationError] LedgerState -> UTxOState -> Assertion
testLedgerValidTransactions ls utxoState' =
    ls @?= Right (LedgerState
                     utxoState'
                     LedgerState.emptyDelegation
                     testPCs
                     1
                     (Slot 0))

testValidStakeKeyRegistration ::
  TxWits -> UTxOState -> DWState -> Assertion
testValidStakeKeyRegistration tx utxoState' stakeKeyRegistration =
  let
    ls2 = ledgerState [tx]
  in ls2 @?= Right (LedgerState
                     utxoState'
                     stakeKeyRegistration
                     testPCs
                     1
                     (Slot 0))

testValidDelegation ::
  [TxWits] -> UTxOState -> DWState -> PoolParams -> Assertion
testValidDelegation txs utxoState' stakeKeyRegistration pool =
  let
    ls2 = ledgerState txs
    poolhk = hashKey $ vKey stakePoolKey1
  in ls2 @?= Right
         (LedgerState
          utxoState'
          (stakeKeyRegistration
           & dstate . delegations .~ Map.fromList [(hashKey $ vKey aliceStake, poolhk)]
           & pstate . stPools .~ (StakePools $ Map.fromList [(poolhk, Slot 0)])
           & pstate . pParams .~ Map.fromList [(poolhk, pool)])
          testPCs
          (fromIntegral $ length txs)
          (Slot 0))

testValidRetirement ::
  [TxWits] -> UTxOState -> DWState -> Epoch -> PoolParams -> Assertion
testValidRetirement txs utxoState' stakeKeyRegistration e pool =
  let
    ls2 = ledgerState txs
    poolhk = hashKey $ vKey stakePoolKey1
  in ls2 @?= Right
         (LedgerState
          utxoState'
          (stakeKeyRegistration
           & dstate . delegations .~ Map.fromList [(hashKey $ vKey aliceStake, poolhk)]
           & pstate . stPools .~  (StakePools $ Map.fromList [(poolhk, Slot 0)])
           & pstate . pParams .~ Map.fromList [(poolhk, pool)]
           & pstate . retiring .~ Map.fromList [(poolhk, e)])
          testPCs
          3
          (Slot 0))

bobWithdrawal :: Map RewardAcnt Coin
bobWithdrawal = Map.singleton (mkRwdAcnt bobStake) (Coin 10)

genesisWithReward :: LedgerState
genesisWithReward = changeReward genesis (mkRwdAcnt bobStake) (Coin 10)

testValidWithdrawal :: Assertion
testValidWithdrawal =
  let
    tx = Tx
           (Set.fromList [TxIn genesisId 0])
           [ TxOut aliceAddr (Coin 6000)
           , TxOut bobAddr (Coin 3010) ]
           []
           bobWithdrawal
           (Coin 1000)
           (Slot 0)
    wits = makeWitnesses tx [alicePay, bobStake]
    utxo' = Map.fromList
       [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
       , (TxIn (txid tx) 0, TxOut aliceAddr (Coin 6000))
       , (TxIn (txid tx) 1, TxOut bobAddr (Coin 3010)) ]
    ls = asStateTransition (Slot 0) genesisWithReward (TxWits tx wits)
    expectedDS = LedgerState.emptyDelegation & dstate . rewards .~
                   Map.singleton (mkRwdAcnt bobStake) (Coin 0)
  in ls @?= Right (LedgerState
                     (UTxOState (UTxO utxo') (Coin 0) (Coin 1000))
                     expectedDS
                     testPCs
                     1
                     (Slot 0))

testInvalidWintess :: Assertion
testInvalidWintess =
  let
    tx = Tx
           (Set.fromList [TxIn genesisId 0])
           [ TxOut aliceAddr (Coin 6000)
           , TxOut bobAddr (Coin 3000) ]
           []
           Map.empty
           (Coin 1000)
           (Slot 1)
    tx' = tx & ttl .~ Slot 2
    wits = makeWitnesses tx' [alicePay]
  in ledgerState [TxWits tx wits] @?= Left [InvalidWitness]


testTooManyWintesses :: Assertion
testTooManyWintesses =
  let
    tx = Tx
           (Set.fromList [TxIn genesisId 0])
           [ TxOut aliceAddr (Coin 6000)
           , TxOut bobAddr (Coin 3000) ]
           []
           Map.empty
           (Coin 1000)
           (Slot 1)
    wits = makeWitnesses tx [alicePay, bobStake]
  in ledgerState [TxWits tx wits] @?= Left [UnneededWitnesses]


testWithdrawalNoWit :: Assertion
testWithdrawalNoWit =
  let
    tx = Tx
           (Set.fromList [TxIn genesisId 0])
           [ TxOut aliceAddr (Coin 6000)
           , TxOut bobAddr (Coin 3010) ]
           []
           bobWithdrawal
           (Coin 1000)
           (Slot 0)
    wits = Set.singleton $ makeWitness tx alicePay
    ls = asStateTransition (Slot 0) genesisWithReward (TxWits tx wits)
  in ls @?= Left [MissingWitnesses]

testWithdrawalWrongAmt :: Assertion
testWithdrawalWrongAmt =
  let
    tx = Tx
           (Set.fromList [TxIn genesisId 0])
           [ TxOut aliceAddr (Coin 6000)
           , TxOut bobAddr (Coin 3011) ]
           []
           (Map.singleton (mkRwdAcnt bobStake) (Coin 11))
           (Coin 1000)
           (Slot 0)
    wits = makeWitnesses tx [alicePay, bobStake]
    ls = asStateTransition (Slot 0) genesisWithReward (TxWits tx wits)
  in ls @?= Left [IncorrectRewards]

aliceGivesBobLovelace :: TxIn -> Coin -> Coin -> Coin -> Coin ->
  [DCert] -> Slot -> [KeyPair] -> TxWits
aliceGivesBobLovelace txin coin fee txdeps txrefs cs s signers = TxWits txbody wits
  where
    aliceCoin = aliceInitCoin + txrefs - (coin + fee + txdeps)
    txbody = Tx
               (Set.fromList [txin])
               [ TxOut aliceAddr aliceCoin
               , TxOut bobAddr coin ]
               cs
               Map.empty
               fee
               s
    wits = makeWitnesses txbody signers

tx1 :: TxWits
tx1 = aliceGivesBobLovelace
        (TxIn genesisId 0)
        (Coin 3000) (Coin 600) (Coin 0) (Coin 0)
        []
        (Slot 0)
        [alicePay]


utxoSt1 :: UTxOState
utxoSt1 = UTxOState
            (UTxO $ Map.fromList
               [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
               , (TxIn (txid $ tx1 ^. body) 0, TxOut aliceAddr (Coin 6400))
               , (TxIn (txid $ tx1 ^. body) 1, TxOut bobAddr (Coin 3000)) ])
            (Coin 0)
            (Coin 600)

ls1 :: Either [ValidationError] LedgerState
ls1 = ledgerState [tx1]

tx2 :: TxWits
tx2 = aliceGivesBobLovelace
        (TxIn genesisId 0)
        (Coin 3000) (Coin 1300) (Coin 3*100) (Coin 0)
        [ RegKey $ vKey aliceStake
        , RegKey $ vKey bobStake
        , RegKey $ vKey stakePoolKey1]
        (Slot 100)
        [alicePay, aliceStake, bobStake, stakePoolKey1]


utxoSt2 :: UTxOState
utxoSt2 = UTxOState
            (UTxO $ Map.fromList
              [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
              , (TxIn (txid $ tx2 ^. body) 0, TxOut aliceAddr (Coin 5400))
              , (TxIn (txid $ tx2 ^. body) 1, TxOut bobAddr (Coin 3000)) ])
            (Coin 300)
            (Coin 1300)

tx3Body :: Tx
tx3Body = Tx
          (Set.fromList [TxIn (txid $ tx2 ^. body) 0])
          [ TxOut aliceAddr (Coin 3950) ]
          [ RegPool stakePool
          , Delegate (Delegation (vKey aliceStake) (vKey stakePoolKey1))]
          Map.empty
          (Coin 1200)
          (Slot 100)

tx3 :: TxWits
tx3 = TxWits tx3Body (makeWitnesses tx3Body keys)
      where keys = [alicePay, aliceStake, stakePoolKey1]

utxoSt3 :: UTxOState
utxoSt3 = UTxOState
            (UTxO $ Map.fromList
              [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
              , (TxIn (txid tx3Body) 0, TxOut aliceAddr (Coin 3950))
              , (TxIn (txid $ tx2 ^. body) 1, TxOut bobAddr (Coin 3000)) ])
            (Coin 550)
            (Coin 2500)

stakeKeyRegistration1 :: DWState
stakeKeyRegistration1 = LedgerState.emptyDelegation
  & dstate . rewards .~
      Map.fromList [ (mkRwdAcnt aliceStake, Coin 0)
                   , (mkRwdAcnt bobStake, Coin 0)
                   , (mkRwdAcnt stakePoolKey1, Coin 0)]
  & dstate . stKeys .~ (StakeKeys $
      Map.fromList [ (hashKey $ vKey aliceStake, Slot 0)
                   , (hashKey $ vKey bobStake, Slot 0)
                   , (hashKey $ vKey stakePoolKey1, Slot 0)])
  & dstate . ptrs .~
      Map.fromList [ (Ptr (Slot 0) 0 0, hashKey $ vKey aliceStake)
                   , (Ptr (Slot 0) 0 1, hashKey $ vKey bobStake)
                   , (Ptr (Slot 0) 0 2, hashKey $ vKey stakePoolKey1)
                   ]

stakePool :: PoolParams
stakePool = PoolParams
            {
              _poolPubKey = vKey stakePoolKey1
            , _poolPledge  = Coin 0
            , _poolPledges = Map.empty
            , _poolCost = Coin 0      -- TODO: what is a sensible value?
            , _poolMargin = interval0     --          or here?
            , _poolAltAcnt = Nothing  --          or here?
            , _poolRAcnt   = RewardAcnt (hashKey . vKey $ stakePoolKey1)
            , _poolOwners  = Set.empty
            }

halfInterval :: UnitInterval
halfInterval =
    fromMaybe (error "could not construct unit interval") $ mkUnitInterval 0.5

stakePoolUpdate :: PoolParams
stakePoolUpdate = PoolParams
                   {
                     _poolPubKey = vKey stakePoolKey1
                   , _poolPledge  = Coin 0
                   , _poolPledges = Map.empty
                   , _poolCost = Coin 100      -- TODO: what is a sensible value?
                   , _poolMargin = halfInterval     --          or here?
                   , _poolAltAcnt = Nothing  --          or here?
                   , _poolRAcnt   = RewardAcnt (hashKey . vKey $ stakePoolKey1)
                   , _poolOwners  = Set.empty
                   }

tx4Body :: Tx
tx4Body = Tx
          (Set.fromList [TxIn (txid $ tx3 ^. body) 0])
          [ TxOut aliceAddr (Coin 2950) ] -- Note the deposit is not charged
          [ RegPool stakePoolUpdate ]
          Map.empty
          (Coin 1000)
          (Slot 100)

tx4 :: TxWits
tx4 = TxWits tx4Body (makeWitnesses tx4Body [alicePay, stakePoolKey1])

utxoSt4 :: UTxOState
utxoSt4 = UTxOState
            (UTxO $ Map.fromList
              [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
              , (TxIn (txid tx4Body) 0, TxOut aliceAddr (Coin 2950))
              , (TxIn (txid $ tx2 ^. body) 1, TxOut bobAddr (Coin 3000)) ])
            (Coin 550)
            (Coin 3500)

utxo5 :: Epoch -> UTxOState
utxo5 e = UTxOState
            (UTxO $ Map.fromList
              [ (TxIn genesisId 1, TxOut bobAddr (Coin 1000))
              , (TxIn (txid $ tx5Body e) 0, TxOut aliceAddr (Coin 2950))
              , (TxIn (txid $ tx2 ^. body) 1, TxOut bobAddr (Coin 3000)) ])
            (Coin 550)
            (Coin 3500)

tx5Body :: Epoch -> Tx
tx5Body e = Tx
          (Set.fromList [TxIn (txid $ tx3 ^. body) 0])
          [ TxOut aliceAddr (Coin 2950) ]
          [ RetirePool (vKey stakePoolKey1) e ]
          Map.empty
          (Coin 1000)
          (Slot 100)

tx5 :: Epoch -> TxWits
tx5 e = TxWits (tx5Body e) (makeWitnesses (tx5Body e) [alicePay, stakePoolKey1])


testsValidLedger :: TestTree
testsValidLedger =
  testGroup "Tests with valid transactions in ledger."
    [ testCase "Valid Ledger - Alice gives Bob 3000 of her 10000 lovelace" $
        testLedgerValidTransactions ls1 utxoSt1
      , testGroup "Tests for stake delegation."
          [ testCase "Valid stake key registration." $
              testValidStakeKeyRegistration tx2 utxoSt2 stakeKeyRegistration1
          , testCase "Valid stake delegation from Alice to stake pool." $
              testValidDelegation [tx2, tx3] utxoSt3 stakeKeyRegistration1 stakePool
          , testCase "Update stake pool parameters" $
              testValidDelegation [tx2, tx3, tx4] utxoSt4 stakeKeyRegistration1 stakePoolUpdate
          , testCase "Retire Pool" $
            testValidRetirement [tx2, tx3, tx5 (Epoch 1)] (utxo5 (Epoch 1)) stakeKeyRegistration1 (Epoch 1) stakePool
          ]
      , testGroup "Tests for withdrawals"
          [ testCase "Valid withdrawal." testValidWithdrawal
          ]
    ]

testSpendNonexistentInput :: Assertion
testSpendNonexistentInput =
  let
    tx = aliceGivesBobLovelace
           (TxIn genesisId 42)
           (Coin 3000) (Coin 1500) (Coin 0) (Coin 0)
           []
           (Slot 100)
           [alicePay]
  in ledgerState [tx] @?=
       Left [ BadInputs
            , ValueNotConserved (Coin 0) (Coin 10000)
            , UnneededWitnesses]
  -- Note that BadInputs implies UnneededWitnesses

testWitnessNotIncluded :: Assertion
testWitnessNotIncluded =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 6404)
              , TxOut bobAddr (Coin 3000) ]
              []
              Map.empty
              (Coin 596)
              (Slot 100)
    tx = TxWits txbody Set.empty
  in ledgerState [tx] @?= Left [MissingWitnesses]

testSpendNotOwnedUTxO :: Assertion
testSpendNotOwnedUTxO =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 232)]
              []
              Map.empty
              (Coin 768)
              (Slot 100)
    aliceWit = makeWitness  txbody alicePay
    tx = TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?= Left [MissingWitnesses, UnneededWitnesses]

testWitnessWrongUTxO :: Assertion
testWitnessWrongUTxO =
  let
    txbody = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 230)]
              []
              Map.empty
              (Coin 770)
              (Slot 100)
    tx2body = Tx
              (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 230)]
              []
              Map.empty
              (Coin 770)
              (Slot 101)
    aliceWit = makeWitness  tx2body alicePay
    tx = TxWits txbody (Set.fromList [aliceWit])
  in ledgerState [tx] @?= Left [ InvalidWitness
                               , MissingWitnesses
                               , UnneededWitnesses ]

testEmptyInputSet :: Assertion
testEmptyInputSet =
  let
    aliceWithdrawal = Map.singleton (mkRwdAcnt aliceStake) (Coin 2000)
    tx = Tx
           Set.empty
           [ TxOut aliceAddr (Coin 1000) ]
           []
           aliceWithdrawal
           (Coin 1000)
           (Slot 0)
    wits = makeWitnesses tx [aliceStake]
    genesisWithReward' = changeReward genesis (mkRwdAcnt aliceStake) (Coin 2000)
    ls = asStateTransition (Slot 0) genesisWithReward' (TxWits tx wits)
  in ls @?= Left [ InputSetEmpty ]

testFeeTooSmall :: Assertion
testFeeTooSmall =
  let
    tx = aliceGivesBobLovelace
           (TxIn genesisId 0)
           (Coin 3000) (Coin 1) (Coin 0) (Coin 0)
           []
           (Slot 100)
           [alicePay]
  in ledgerState [tx] @?=
       Left [ FeeTooSmall (minfee testPCs $ tx ^. body) (Coin 1) ]

testExpiredTx :: Assertion
testExpiredTx =
  let
    tx = aliceGivesBobLovelace
           (TxIn genesisId 0)
           (Coin 3000) (Coin 600) (Coin 0) (Coin 0)
           []
           (Slot 0)
           [alicePay]
  in asStateTransition (Slot 1) genesis tx @?=
       Left [ Expired (Slot 0) (Slot 1) ]

testsInvalidLedger :: TestTree
testsInvalidLedger = testGroup "Tests with invalid transactions in ledger"
  [ testCase "Invalid Ledger - Alice tries to spend a nonexistent input" testSpendNonexistentInput
  , testCase "Invalid Ledger - Alice does not include a witness" testWitnessNotIncluded
  , testCase "Invalid Ledger - Alice tries to spend Bob's UTxO" testSpendNotOwnedUTxO
  , testCase "Invalid Ledger - Alice provides witness of wrong UTxO" testWitnessWrongUTxO
  , testCase "Invalid Ledger - Alice's transaction does not consume input" testEmptyInputSet
  , testCase "Invalid Ledger - Alice's fee is too small" testFeeTooSmall
  , testCase "Invalid Ledger - Alice's transaction has expired" testExpiredTx
  , testCase "Invalid Ledger - Invalid witnesses" testInvalidWintess
  , testCase "Invalid Ledger - Too many witnesses" testTooManyWintesses
  , testCase "Invalid Ledger - No withdrawal witness" testWithdrawalNoWit
  , testCase "Invalid Ledger - Incorrect withdrawal amount" testWithdrawalWrongAmt
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testsValidLedger, testsInvalidLedger ]
