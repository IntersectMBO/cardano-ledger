{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.UnitTests (unitTests) where

import Cardano.Crypto.Hash (ShortHash)
import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Control.State.Transition.Trace ((.-), (.->), checkTrace)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Address (mkVKeyRwdAcnt, pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.BlockChain (checkVRFValue)
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Credential (Credential (..), pattern StakeRefBase)
import Shelley.Spec.Ledger.Delegation.Certificates (pattern RegPool)
import Shelley.Spec.Ledger.Keys (KeyRole (..), asWitness, hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    WitHashes (..),
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
import Shelley.Spec.Ledger.STS.Delpl (PredicateFailure (..))
import Shelley.Spec.Ledger.STS.Ledger
  ( pattern DelegsFailure,
    pattern LedgerEnv,
    pattern UtxoFailure,
    pattern UtxowFailure,
  )
import Shelley.Spec.Ledger.STS.Pool (PredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxo (PredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxow (PredicateFailure (..))
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx
  ( WitnessSetHKD (..),
    _ttl,
    pattern Tx,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
  )
import Shelley.Spec.Ledger.TxData
  ( PoolMetaData (..),
    Wdrl (..),
    _poolCost,
    _poolMD,
    _poolMDHash,
    _poolMDUrl,
    _poolMargin,
    _poolOwners,
    _poolPledge,
    _poolPubKey,
    _poolRAcnt,
    _poolRelays,
    _poolVrf,
    pattern DCertPool,
    pattern PoolParams,
    pattern RewardAcnt,
  )
import Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessVKey, makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Test.Shelley.Spec.Ledger.Fees (sizeTests)
import Test.Shelley.Spec.Ledger.Generator.Core (unitIntervalToNatural)
import Test.Shelley.Spec.Ledger.Orphans ()
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

alicePay :: KeyPair ShortHash 'Payment
alicePay = KeyPair 1 1

aliceStake :: KeyPair ShortHash 'Staking
aliceStake = KeyPair 2 2

aliceAddr :: Addr ShortHash
aliceAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey alicePay)
    (StakeRefBase . KeyHashObj . hashKey $ vKey aliceStake)

bobPay :: KeyPair ShortHash 'Payment
bobPay = KeyPair 3 3

bobStake :: KeyPair ShortHash 'Staking
bobStake = KeyPair 4 4

bobAddr :: Addr ShortHash
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
      _minUTxOValue = 100,
      _minPoolCost = 100
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
  (UTxOState ShortHash, DPState ShortHash) ->
  Tx ShortHash ->
  LedgerEnv ->
  Either [[PredicateFailure (LEDGER ShortHash)]] (UTxOState ShortHash, DPState ShortHash) ->
  Assertion
testLEDGER initSt tx env (Right expectedSt) = do
  checkTrace @(LEDGER ShortHash) runShelleyBase env $ pure initSt .- tx .-> expectedSt
testLEDGER initSt tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTS @(LEDGER ShortHash) (TRC (env, initSt, tx))
  st @?= predicateFailure

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

data AliceToBob = AliceToBob
  { input :: TxIn ShortHash,
    toBob :: Coin,
    fee :: Coin,
    deposits :: Coin,
    refunds :: Coin,
    certs :: [DCert ShortHash],
    ttl :: SlotNo,
    signers :: ([KeyPair ShortHash 'AWitness], [KeyPair ShortHash 'RWitness])
  }

aliceGivesBobLovelace :: AliceToBob -> Tx ShortHash
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
    } = Tx txbody mempty {addrWits = awits, regWits = rwits} SNothing
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
      (asigs, rsigs) = signers
      awits = makeWitnessesVKey (hashTxBody txbody) asigs
      rwits = makeWitnessesVKey (hashTxBody txbody) rsigs

utxoState :: UTxOState ShortHash
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

dpState :: DPState ShortHash
dpState = DPState emptyDState emptyPState

addReward :: DPState ShortHash -> RewardAcnt ShortHash -> Coin -> DPState ShortHash
addReward dp ra c = dp {_dstate = ds {_rewards = rewards}}
  where
    ds = _dstate dp
    rewards = Map.insert ra c $ _rewards ds

ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 pp (AccountState 0 0)

testInvalidTx ::
  [PredicateFailure (LEDGER ShortHash)] ->
  Tx ShortHash ->
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
        signers = ([asWitness alicePay], [])
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
      tx = Tx txbody mempty SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey alicePay)
   in testInvalidTx
        [ UtxowFailure
            $ MissingVKeyWitnessesUTXOW
            $ WitHashes
              { addrWitHashes = wits,
                regWitHashes =
                  mempty
              }
        ]
        tx

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
      tx = Tx txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure
            $ MissingVKeyWitnessesUTXOW
            $ WitHashes
              { addrWitHashes = wits,
                regWitHashes =
                  mempty
              }
        ]
        tx

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
      tx = Tx txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              ([asWitness $ vKey alicePay], mempty),
          UtxowFailure
            $ MissingVKeyWitnessesUTXOW
            $ WitHashes
              { addrWitHashes = wits,
                regWitHashes =
                  mempty
              }
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
      wits = mempty {addrWits = makeWitnessesVKey (hashTxBody txb) [aliceStake]}
      tx = Tx txb wits SNothing
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
          signers = ([asWitness alicePay], [])
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
              signers = ([asWitness alicePay], [])
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
      wits = mempty {addrWits = makeWitnessesVKey (hashTxBody txb') [alicePay]}
      tx = Tx txb wits SNothing
      errs =
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              ([asWitness $ vKey alicePay], mempty)
        ]
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
      wits = mempty {addrWits = Set.singleton $ makeWitnessVKey (hashTxBody txb) alicePay}
      tx = Tx txb wits SNothing
      missing = Set.singleton (asWitness $ hashKey $ vKey bobStake)
      errs =
        [ UtxowFailure . MissingVKeyWitnessesUTXOW $
            WitHashes {addrWitHashes = missing, regWitHashes = mempty}
        ]
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
      wits =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashTxBody txb)
                [asWitness alicePay, asWitness bobStake]
          }
      rAcnt = mkVKeyRwdAcnt Testnet bobStake
      dpState' = addReward dpState rAcnt (Coin 10)
      tx = Tx txb wits SNothing
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
        signers = ([asWitness alicePay], [])
      }

alicePoolColdKeys :: KeyPair ShortHash 'StakePool
alicePoolColdKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

alicePoolParamsSmallCost :: PoolParams ShortHash
alicePoolParamsSmallCost =
  PoolParams
    { _poolPubKey = hashKey . vKey $ alicePoolColdKeys,
      _poolVrf = hashKeyVRF vkVrf,
      _poolPledge = Coin 1,
      _poolCost = Coin 5, -- Too Small!
      _poolMargin = unsafeMkUnitInterval 0.1,
      _poolRAcnt = RewardAcnt Testnet (KeyHashObj . hashKey . vKey $ aliceStake),
      _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      _poolRelays = StrictSeq.empty,
      _poolMD =
        SJust $
          PoolMetaData
            { _poolMDUrl = fromJust $ textToUrl "alice.pool",
              _poolMDHash = BS.pack "{}"
            }
    }
  where
    (_skVrf, vkVrf) = mkVRFKeyPair (0, 0, 0, 0, 2)

testPoolCostTooSmall :: Assertion
testPoolCostTooSmall =
  testInvalidTx
    [ DelegsFailure
        ( DelplFailure
            ( PoolFailure
                ( StakePoolCostTooLowPOOL (_poolCost alicePoolParamsSmallCost) (_minPoolCost pp)
                )
            )
        )
    ]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = (TxIn genesisId 0),
        toBob = (Coin 100),
        fee = (Coin 997),
        deposits = (Coin 250),
        refunds = (Coin 0),
        certs = [DCertPool $ RegPool alicePoolParamsSmallCost],
        ttl = (SlotNo 0),
        signers =
          ( [asWitness alicePay, asWitness aliceStake],
            [asWitness alicePoolColdKeys]
          )
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
      testCase "Invalid Ledger - OutputTooSmall" testOutputTooSmall,
      testCase "Invalid Ledger - PoolCostTooSmall" testPoolCostTooSmall
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
