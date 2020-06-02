{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.UnitTests (unitTests) where

import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Address (mkVKeyRwdAcnt, pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.BlockChain (checkVRFValue)
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Credential (Credential (..), pattern StakeRefBase)
import Shelley.Spec.Ledger.Keys (KeyRole (..), asWitness, hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    _dstate,
    _rewards,
    emptyDState,
    emptyPState,
    genesisCoins,
    genesisId,
    overlaySchedule,
    pattern DPState,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.STS.Delegs (PredicateFailure (..))
import Shelley.Spec.Ledger.STS.Ledger (pattern DelegsFailure, pattern LedgerEnv, pattern UtxoFailure, pattern UtxowFailure)
import Shelley.Spec.Ledger.STS.Utxo (PredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxow (PredicateFailure (..))
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx
  ( _ttl,
    pattern Tx,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
  )
import Shelley.Spec.Ledger.TxData (Wdrl (..))
import Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessVKey, makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Test.Shelley.Spec.Ledger.Fees (sizeTests)
import Test.Shelley.Spec.Ledger.Generator.Core (unitIntervalToNatural)
import Test.Shelley.Spec.Ledger.Orphans ()
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

alicePay :: KeyPair 'Payment
alicePay = KeyPair 1 1

aliceStake :: KeyPair 'Staking
aliceStake = KeyPair 2 2

aliceAddr :: Addr
aliceAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey alicePay)
    (StakeRefBase . KeyHashObj . hashKey $ vKey aliceStake)

bobPay :: KeyPair 'Payment
bobPay = KeyPair 3 3

bobStake :: KeyPair 'Staking
bobStake = KeyPair 4 4

bobAddr :: Addr
bobAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey bobPay)
    (StakeRefBase . KeyHashObj . hashKey $ vKey bobStake)

pp :: PParams
pp =
  emptyPParams
    { _minfeeA = 1,
      _minfeeB = 1,
      _keyDeposit = 100,
      _poolDeposit = 250,
      _maxTxSize = 1024,
      _eMax = EpochNo 10,
      _minUTxOValue = 100
    }

testOverlayScheduleZero :: Assertion
testOverlayScheduleZero =
  let os =
        runShelleyBase $
          overlaySchedule
            (EpochNo 0)
            mempty
            (emptyPParams {_d = unsafeMkUnitInterval 0})
   in os @?= Map.empty

testNoGenesisOverlay :: Assertion
testNoGenesisOverlay =
  let os =
        runShelleyBase $
          overlaySchedule
            (EpochNo 0)
            mempty
            (emptyPParams {_d = unsafeMkUnitInterval 0.5})
   in os @?= Map.empty

testVRFCheckWithActiveSlotCoeffOne :: Assertion
testVRFCheckWithActiveSlotCoeffOne =
  checkVRFValue 0 (1 % 2) (mkActiveSlotCoeff $ unsafeMkUnitInterval 1) @?= True

testVRFCheckWithLeaderValueOne :: Assertion
testVRFCheckWithLeaderValueOne =
  checkVRFValue vrfOne (1 % 2) (mkActiveSlotCoeff $ unsafeMkUnitInterval 0.5) @?= False
  where
    vrfOne = unitIntervalToNatural (unsafeMkUnitInterval 1)

testsPParams :: TestTree
testsPParams =
  testGroup
    "Test the protocol parameters."
    [ testCase "Overlay Schedule when d is zero" $
        testOverlayScheduleZero,
      testCase "generate overlay schedule without genesis nodes" $
        testNoGenesisOverlay,
      testCase "VRF checks when the activeSlotCoeff is one" $
        testVRFCheckWithActiveSlotCoeffOne,
      testCase "VRF checks when the VRF leader value is one" $
        testVRFCheckWithLeaderValueOne
    ]

testTruncateUnitInterval :: TestTree
testTruncateUnitInterval = testProperty "truncateUnitInterval in [0,1]" $
  \n ->
    let x = intervalValue $ truncateUnitInterval n
     in (x <= 1) && (x >= 0)

testLEDGER ::
  (UTxOState, DPState) ->
  Tx ->
  LedgerEnv ->
  Either [[PredicateFailure LEDGER]] (UTxOState, DPState) ->
  Assertion
testLEDGER initSt tx env (Right expectedSt) = do
  checkTrace @LEDGER runShelleyBase env $ pure initSt .- tx .-> expectedSt
testLEDGER initSt tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTS @LEDGER (TRC (env, initSt, tx))
  st @?= predicateFailure

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

data AliceToBob = AliceToBob
  { input :: TxIn,
    toBob :: Coin,
    fee :: Coin,
    deposits :: Coin,
    refunds :: Coin,
    certs :: [DCert],
    ttl :: SlotNo,
    signers :: [KeyPair 'Witness]
  }

aliceGivesBobLovelace :: AliceToBob -> Tx
aliceGivesBobLovelace
  AliceToBob
    { input,
      toBob,
      fee,
      deposits,
      refunds,
      certs,
      ttl,
      signers
    } = Tx txbody wits Map.empty SNothing
    where
      aliceCoin = aliceInitCoin + refunds - (toBob + fee + deposits)
      txbody =
        TxBody
          (Set.singleton input)
          ( StrictSeq.fromList
              [ TxOut aliceAddr aliceCoin,
                TxOut bobAddr toBob
              ]
          )
          (StrictSeq.fromList certs)
          (Wdrl Map.empty)
          fee
          ttl
          SNothing
          SNothing
      wits = makeWitnessesVKey (hashTxBody txbody) signers

utxoState :: UTxOState
utxoState =
  UTxOState
    ( genesisCoins
        [ TxOut aliceAddr aliceInitCoin,
          TxOut bobAddr (Coin 1000)
        ]
    )
    (Coin 0)
    (Coin 0)
    emptyPPPUpdates

dpState :: DPState
dpState = DPState emptyDState emptyPState

addReward :: DPState -> RewardAcnt -> Coin -> DPState
addReward dp ra c = dp {_dstate = ds {_rewards = rewards}}
  where
    ds = _dstate dp
    rewards = Map.insert ra c $ _rewards ds

ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 pp (AccountState 0 0)

testInvalidTx ::
  [PredicateFailure LEDGER] ->
  Tx ->
  Assertion
testInvalidTx errs tx =
  testLEDGER (utxoState, dpState) tx ledgerEnv (Left [errs])

testSpendNonexistentInput :: Assertion
testSpendNonexistentInput =
  testInvalidTx
    [ UtxowFailure (UtxoFailure (ValueNotConservedUTxO (Coin 0) (Coin 10000))),
      UtxowFailure (UtxoFailure $ BadInputsUTxO (Set.singleton $ TxIn genesisId 42))
    ]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = (TxIn genesisId 42), -- Non Existent
        toBob = (Coin 3000),
        fee = (Coin 1500),
        deposits = (Coin 0),
        refunds = (Coin 0),
        certs = [],
        ttl = (SlotNo 100),
        signers = [asWitness alicePay]
      }

testWitnessNotIncluded :: Assertion
testWitnessNotIncluded =
  let txbody =
        TxBody
          (Set.fromList [TxIn genesisId 0])
          ( StrictSeq.fromList
              [ TxOut aliceAddr (Coin 6404),
                TxOut bobAddr (Coin 3000)
              ]
          )
          Empty
          (Wdrl Map.empty)
          (Coin 596)
          (SlotNo 100)
          SNothing
          SNothing
      tx = Tx txbody Set.empty Map.empty SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey alicePay)
   in testInvalidTx [UtxowFailure $ MissingVKeyWitnessesUTXOW wits] tx

testSpendNotOwnedUTxO :: Assertion
testSpendNotOwnedUTxO =
  let txbody =
        TxBody
          (Set.fromList [TxIn genesisId 1])
          (StrictSeq.singleton $ TxOut aliceAddr (Coin 232))
          Empty
          (Wdrl Map.empty)
          (Coin 768)
          (SlotNo 100)
          SNothing
          SNothing
      aliceWit = makeWitnessVKey (hashTxBody txbody) alicePay
      tx = Tx txbody (Set.fromList [aliceWit]) Map.empty SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx [UtxowFailure $ MissingVKeyWitnessesUTXOW wits] tx

testWitnessWrongUTxO :: Assertion
testWitnessWrongUTxO =
  let txbody =
        TxBody
          (Set.fromList [TxIn genesisId 1])
          (StrictSeq.singleton $ TxOut aliceAddr (Coin 230))
          Empty
          (Wdrl Map.empty)
          (Coin 770)
          (SlotNo 100)
          SNothing
          SNothing
      tx2body =
        TxBody
          (Set.fromList [TxIn genesisId 1])
          (StrictSeq.singleton $ TxOut aliceAddr (Coin 230))
          Empty
          (Wdrl Map.empty)
          (Coin 770)
          (SlotNo 101)
          SNothing
          SNothing
      aliceWit = makeWitnessVKey (hashTxBody tx2body) alicePay
      tx = Tx txbody (Set.fromList [aliceWit]) Map.empty SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $ InvalidWitnessesUTXOW [asWitness $ vKey alicePay],
          UtxowFailure $ MissingVKeyWitnessesUTXOW wits
        ]
        tx

testEmptyInputSet :: Assertion
testEmptyInputSet =
  let aliceWithdrawal = Map.singleton (mkVKeyRwdAcnt Testnet aliceStake) (Coin 2000)
      txb =
        TxBody
          Set.empty
          (StrictSeq.singleton $ TxOut aliceAddr (Coin 1000))
          Empty
          (Wdrl aliceWithdrawal)
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      wits = makeWitnessesVKey (hashTxBody txb) [aliceStake]
      tx = Tx txb wits Map.empty SNothing
      dpState' = addReward dpState (mkVKeyRwdAcnt Testnet aliceStake) (Coin 2000)
   in testLEDGER
        (utxoState, dpState')
        tx
        ledgerEnv
        (Left [[UtxowFailure (UtxoFailure InputSetEmptyUTxO)]])

testFeeTooSmall :: Assertion
testFeeTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 206) (Coin 1)))]
    $ aliceGivesBobLovelace
      AliceToBob
        { input = (TxIn genesisId 0),
          toBob = (Coin 3000),
          fee = (Coin 1),
          deposits = (Coin 0),
          refunds = (Coin 0),
          certs = [],
          ttl = (SlotNo 100),
          signers = [asWitness alicePay]
        }

testExpiredTx :: Assertion
testExpiredTx =
  let errs = [UtxowFailure (UtxoFailure (ExpiredUTxO (SlotNo {unSlotNo = 0}) (SlotNo {unSlotNo = 1})))]
      tx =
        aliceGivesBobLovelace $
          AliceToBob
            { input = (TxIn genesisId 0),
              toBob = (Coin 3000),
              fee = (Coin 600),
              deposits = (Coin 0),
              refunds = (Coin 0),
              certs = [],
              ttl = (SlotNo 0),
              signers = [asWitness alicePay]
            }
      ledgerEnv' = LedgerEnv (SlotNo 1) 0 pp (AccountState 0 0)
   in testLEDGER (utxoState, dpState) tx ledgerEnv' (Left [errs])

testInvalidWintess :: Assertion
testInvalidWintess =
  let txb =
        TxBody
          (Set.fromList [TxIn genesisId 0])
          ( StrictSeq.fromList
              [ TxOut aliceAddr (Coin 6000),
                TxOut bobAddr (Coin 3000)
              ]
          )
          Empty
          (Wdrl Map.empty)
          (Coin 1000)
          (SlotNo 1)
          SNothing
          SNothing
      txb' = txb {_ttl = SlotNo 2}
      wits = makeWitnessesVKey (hashTxBody txb') [alicePay]
      tx = Tx txb wits Map.empty SNothing
      errs = [UtxowFailure $ InvalidWitnessesUTXOW [asWitness $ vKey alicePay]]
   in testLEDGER (utxoState, dpState) tx ledgerEnv (Left [errs])

testWithdrawalNoWit :: Assertion
testWithdrawalNoWit =
  let txb =
        TxBody
          (Set.fromList [TxIn genesisId 0])
          ( StrictSeq.fromList
              [ TxOut aliceAddr (Coin 6000),
                TxOut bobAddr (Coin 3010)
              ]
          )
          Empty
          (Wdrl $ Map.singleton (mkVKeyRwdAcnt Testnet bobStake) (Coin 10))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      wits = Set.singleton $ makeWitnessVKey (hashTxBody txb) alicePay
      tx = Tx txb wits Map.empty SNothing
      missing = Set.singleton (asWitness $ hashKey $ vKey bobStake)
      errs = [UtxowFailure $ MissingVKeyWitnessesUTXOW missing]
      dpState' = addReward dpState (mkVKeyRwdAcnt Testnet bobStake) (Coin 10)
   in testLEDGER (utxoState, dpState') tx ledgerEnv (Left [errs])

testWithdrawalWrongAmt :: Assertion
testWithdrawalWrongAmt =
  let txb =
        TxBody
          (Set.fromList [TxIn genesisId 0])
          ( StrictSeq.fromList
              [ TxOut aliceAddr (Coin 6000),
                TxOut bobAddr (Coin 3011)
              ]
          )
          Empty
          (Wdrl $ Map.singleton (mkVKeyRwdAcnt Testnet bobStake) (Coin 11))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      wits = makeWitnessesVKey (hashTxBody txb) [asWitness alicePay, asWitness bobStake]
      rAcnt = mkVKeyRwdAcnt Testnet bobStake
      dpState' = addReward dpState rAcnt (Coin 10)
      tx = Tx txb wits Map.empty SNothing
      errs = [DelegsFailure (WithdrawalsNotInRewardsDELEGS (Map.singleton rAcnt (Coin 11)))]
   in testLEDGER (utxoState, dpState') tx ledgerEnv (Left [errs])

testOutputTooSmall :: Assertion
testOutputTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure $ OutputTooSmallUTxO [TxOut bobAddr (Coin 1)])]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = (TxIn genesisId 0),
        toBob = (Coin 1), -- Too Small
        fee = (Coin 997),
        deposits = (Coin 0),
        refunds = (Coin 0),
        certs = [],
        ttl = (SlotNo 0),
        signers = [asWitness alicePay]
      }

testsInvalidLedger :: TestTree
testsInvalidLedger =
  testGroup
    "Tests with invalid transactions in ledger"
    [ testCase "Invalid Ledger - Alice tries to spend a nonexistent input" testSpendNonexistentInput,
      testCase "Invalid Ledger - Alice does not include a witness" testWitnessNotIncluded,
      testCase "Invalid Ledger - Alice tries to spend Bob's UTxO" testSpendNotOwnedUTxO,
      testCase "Invalid Ledger - Alice provides witness of wrong UTxO" testWitnessWrongUTxO,
      testCase "Invalid Ledger - Alice's transaction does not consume input" testEmptyInputSet,
      testCase "Invalid Ledger - Alice's fee is too small" testFeeTooSmall,
      testCase "Invalid Ledger - Alice's transaction has expired" testExpiredTx,
      testCase "Invalid Ledger - Invalid witnesses" testInvalidWintess,
      testCase "Invalid Ledger - No withdrawal witness" testWithdrawalNoWit,
      testCase "Invalid Ledger - Incorrect withdrawal amount" testWithdrawalWrongAmt,
      testCase "Invalid Ledger - OutputTooSmall" testOutputTooSmall
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testsInvalidLedger,
      testsPParams,
      sizeTests,
      testTruncateUnitInterval
    ]
