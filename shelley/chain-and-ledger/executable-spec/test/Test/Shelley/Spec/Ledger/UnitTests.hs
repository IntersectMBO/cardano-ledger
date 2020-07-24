{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.UnitTests (unitTests) where

import qualified Cardano.Crypto.VRF as VRF
import Control.State.Transition.Extended (PredicateFailure, TRC (..))
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (getRwdCred, mkVKeyRwdAcnt, pattern Addr)
import Shelley.Spec.Ledger.BaseTypes hiding ((==>))
import Shelley.Spec.Ledger.BlockChain (checkLeaderValue)
import Shelley.Spec.Ledger.Coin
import qualified Shelley.Spec.Ledger.Credential as Credential (Credential (KeyHashObj), pattern StakeRefBase)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates (pattern RegPool)
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys (KeyRole (..), asWitness, hashKey, vKey)
import Shelley.Spec.Ledger.Keys (KeyPair (..))
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    WitHashes (..),
    emptyDState,
    emptyPPUPState,
    emptyPState,
    overlaySchedule,
    _dstate,
    _rewards,
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
import Shelley.Spec.Ledger.Value (zeroV, coinToValue, valueToCompactValue)
import Shelley.Spec.Ledger.Tx
  ( WitnessSetHKD (..),
    _ttl,
    pattern Tx,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
    pattern UTxOOut,
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
import Shelley.Spec.Ledger.UTxO (makeWitnessVKey, makeWitnessesVKey)
import qualified Test.QuickCheck.Gen as Gen
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Test.Shelley.Spec.Ledger.Fees (sizeTests)
import Test.Shelley.Spec.Ledger.Generator.Core
  ( genesisCoins,
    genesisId,
  )
import Test.Shelley.Spec.Ledger.Orphans ()
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- TODO change forge values!

alicePay :: KeyPair 'Payment C
alicePay = KeyPair 1 1

aliceStake :: KeyPair 'Staking C
aliceStake = KeyPair 2 2

aliceAddr :: Addr C
aliceAddr =
  Addr
    Testnet
    (Credential.KeyHashObj . hashKey $ vKey alicePay)
    (Credential.StakeRefBase . Credential.KeyHashObj . hashKey $ vKey aliceStake)

bobPay :: KeyPair 'Payment C
bobPay = KeyPair 3 3

bobStake :: KeyPair 'Staking C
bobStake = KeyPair 4 4

bobAddr :: Addr C
bobAddr =
  Addr
    Testnet
    (Credential.KeyHashObj . hashKey $ vKey bobPay)
    (Credential.StakeRefBase . Credential.KeyHashObj . hashKey $ vKey bobStake)

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
  checkLeaderValue
    (VRF.mkTestOutputVRF 0 :: VRF.OutputVRF (VRF (ConcreteCrypto C)))
    (1 % 2)
    (mkActiveSlotCoeff $ unsafeMkUnitInterval 1)
    @?= True

testsPParams :: TestTree
testsPParams =
  testGroup
    "Test the protocol parameters."
    [ testCase "Overlay Schedule when d is zero" $
        testOverlayScheduleZero,
      testCase "generate overlay schedule without genesis nodes" $
        testNoGenesisOverlay,
      testCase "VRF checks when the activeSlotCoeff is one" $
        testVRFCheckWithActiveSlotCoeffOne
    ]

testTruncateUnitInterval :: TestTree
testTruncateUnitInterval = testProperty "truncateUnitInterval in [0,1]" $
  \n ->
    let x = intervalValue $ truncateUnitInterval n
     in (x <= 1) && (x >= 0)

newtype VRFNatVal = VRFNatVal Natural
  deriving (Show)

instance Arbitrary VRFNatVal where
  arbitrary =
    VRFNatVal . fromIntegral
      <$> choose @Integer
        ( 0,
          2
            ^ ( 8
                  * VRF.sizeOutputVRF
                    (Proxy @(VRF (ConcreteCrypto C)))
              )
        )
  shrink (VRFNatVal v) = VRFNatVal <$> shrinkIntegral v

newtype ASC = ASC ActiveSlotCoeff
  deriving (Show)

instance Arbitrary ASC where
  arbitrary =
    ASC
      . mkActiveSlotCoeff
      . unsafeMkUnitInterval
      . fromRational
      . toRational
      <$> choose @Double (0.01, 0.5)

newtype StakeProportion = StakeProportion Rational
  deriving (Show)

instance Arbitrary StakeProportion where
  arbitrary = StakeProportion . toRational <$> choose @Double (0, 1)
  shrink (StakeProportion r) = StakeProportion <$> shrinkRealFrac r

-- | Test @checkLeaderVal@ in 'Shelley.Spec.Ledger.BlockChain'
testCheckLeaderVal ::
  forall v.
  (v ~ VRF C) =>
  -- (v ~ CLVVRF) =>
  TestTree
testCheckLeaderVal =
  testGroup
    "Test checkLeaderVal calculation"
    [ testProperty "With a stake of 0, cannot lead" $
        \(VRFNatVal n) (ASC f) ->
          checkLeaderValue @v (VRF.mkTestOutputVRF n) 0 f == False,
      testProperty "With a maximal VRF, cannot lead" $
        \(ASC f) (StakeProportion r) ->
          checkLeaderValue @v
            (VRF.mkTestOutputVRF maxVRFVal)
            r
            f
            == False,
      testProperty "checkLeaderVal succeeds iff l < 1 - (1-f)^r" $
        \(VRFNatVal n) (ASC f) (StakeProportion r) ->
          r > 0
            ==> let ascVal :: Double
                    ascVal = fromRational . unitIntervalToRational $ activeSlotVal f
                 in checkLeaderValue @v
                      (VRF.mkTestOutputVRF n)
                      r
                      f
                      === ( (realToFrac n / realToFrac (maxVRFVal + 1))
                              < (1 - (1 - ascVal) ** fromRational r)
                          ),
      -- Suppose that our VRF value V is drawn uniformly from [0, maxVRFVal).
      -- The leader check verifies that fromNat V < 1 - (1-f)^r, where fromNat
      -- is an appropriate mapping into the unit interval giving fromNat V ~
      -- U(0,1). Then the probability X of being selected leader, given that VRF
      -- value, is given by p = 1 - (1 - f)^r. So assuming n independent draws
      -- of V, the number of slots in which we lead is given by S ~ Bin(n, p)
      -- and has an expected value of np.
      --
      -- The probability that S sits outside a δ-window around the mean (i.e.
      -- that this test fails under the hypothesis that our leader check is
      -- correct) is given by P(np - δ <= S <= np + δ).
      --
      -- We wish to choose δ such that this value is sufficiently low (say, <
      -- 1/1000).
      --
      testProperty "We are elected as leader proportional to our stake" $
        \(ASC f) (StakeProportion r) ->
          r > 0
            ==> let ascVal :: Double
                    ascVal = fromRational . unitIntervalToRational $ activeSlotVal f
                    numTrials = 500
                    -- 4 standard deviations
                    δ = 4 * sqrt (realToFrac numTrials * p * (1 - p))
                    p = 1 - (1 - ascVal) ** fromRational r
                    mean = realToFrac numTrials * p
                    maxVRFValInt :: Integer
                    maxVRFValInt = fromIntegral maxVRFVal
                    lb = floor (mean - δ)
                    ub = ceiling (mean + δ)
                 in do
                      vrfVals <- Gen.vectorOf numTrials (Gen.choose (0, maxVRFValInt))
                      let s =
                            length . filter id $
                              ( \v ->
                                  checkLeaderValue @v
                                    (VRF.mkTestOutputVRF $ fromIntegral v)
                                    r
                                    f
                              )
                                <$> vrfVals
                      pure
                        . counterexample
                          ( show lb
                              ++ " /< "
                              ++ show s
                              ++ " /< "
                              ++ show ub
                              ++ " (p="
                              ++ show p
                              ++ ")"
                          )
                        $ s > lb && s < ub
    ]
  where
    maxVRFVal :: Natural
    maxVRFVal = (2 ^ (8 * VRF.sizeOutputVRF (Proxy @v))) - 1

testLEDGER ::
  (UTxOState C, DPState C) ->
  Tx C ->
  LedgerEnv ->
  Either [[PredicateFailure (LEDGER C)]] (UTxOState C, DPState C) ->
  Assertion
testLEDGER initSt tx env (Right expectedSt) = do
  checkTrace @(LEDGER C) runShelleyBase env $ pure initSt .- tx .-> expectedSt
testLEDGER initSt tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @(LEDGER C) (TRC (env, initSt, tx))
  st @?= predicateFailure

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

data AliceToBob = AliceToBob
  { input :: TxIn C,
    toBob :: Coin,
    fee :: Coin,
    forge :: Value ShortHash,
    deposits :: Coin,
    refunds :: Coin,
    certs :: [DCert C],
    ttl :: SlotNo,
    signers :: [KeyPair 'Witness C]
  }

aliceGivesBobLovelace :: AliceToBob -> Tx C
aliceGivesBobLovelace
  AliceToBob
    { input,
      toBob,
      fee,
      forge,
      deposits,
      refunds,
      certs,
      ttl,
      signers
    } = Tx txbody mempty {addrWits = awits} SNothing
    where
      aliceCoin = aliceInitCoin + refunds - (toBob + fee + deposits)
      txbody =
        TxBody
          (Set.singleton input)
          ( StrictSeq.fromList
              [ TxOut aliceAddr (coinToValue aliceCoin),
                TxOut bobAddr (coinToValue toBob)
              ]
          )
          (StrictSeq.fromList certs)
          forge
          (Wdrl Map.empty)
          fee
          ttl
          SNothing
          SNothing
      awits = makeWitnessesVKey (hashAnnotated txbody) signers

utxoState :: UTxOState C
utxoState =
  UTxOState
    ( genesisCoins
        [ TxOut aliceAddr (coinToValue aliceInitCoin),
          TxOut bobAddr (coinToValue $ Coin 1000)
        ]
    )
    (Coin 0)
    (Coin 0)
    emptyPPUPState

dpState :: DPState C
dpState = DPState emptyDState emptyPState

addReward :: DPState C -> Credential C 'Staking -> Coin -> DPState C
addReward dp ra c = dp {_dstate = ds {_rewards = rewards}}
  where
    ds = _dstate dp
    rewards = Map.insert ra c $ _rewards ds

ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 pp (AccountState 0 0)

testInvalidTx ::
  [PredicateFailure (LEDGER C)] ->
  Tx C ->
  Assertion
testInvalidTx errs tx =
  testLEDGER (utxoState, dpState) tx ledgerEnv (Left [errs])

testSpendNonexistentInput :: Assertion
testSpendNonexistentInput =
  testInvalidTx
    [ UtxowFailure (UtxoFailure (ValueNotConservedUTxO (coinToValue $ Coin 0) (coinToValue $ Coin 10000))),
      UtxowFailure (UtxoFailure $ BadInputsUTxO (Set.singleton $ TxIn genesisId 42))
    ]
    $ aliceGivesBobLovelace $
      AliceToBob
        { input = (TxIn genesisId 42), -- Non Existent
          toBob = (Coin 3000),
          fee = (Coin 1500),
          forge = zeroV,
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
              [ TxOut aliceAddr (coinToValue $ Coin 6404),
                TxOut bobAddr (coinToValue $ Coin 3000)
              ]
          )
          Empty
          (zeroV)
          (Wdrl Map.empty)
          (Coin 596)
          (SlotNo 100)
          SNothing
          SNothing
      tx = Tx txbody mempty SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey alicePay)
   in testInvalidTx
        [ UtxowFailure $
            MissingVKeyWitnessesUTXOW $
              WitHashes wits
        ]
        tx

testSpendNotOwnedUTxO :: Assertion
testSpendNotOwnedUTxO =
  let txbody =
        TxBody
          (Set.fromList [TxIn genesisId 1])
          (StrictSeq.singleton $ TxOut aliceAddr (coinToValue $ Coin 232))
          Empty
          (zeroV)
          (Wdrl Map.empty)
          (Coin 768)
          (SlotNo 100)
          SNothing
          SNothing
      aliceWit = makeWitnessVKey (hashAnnotated txbody) alicePay
      tx = Tx txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $
            MissingVKeyWitnessesUTXOW $
              WitHashes wits
        ]
        tx

testWitnessWrongUTxO :: Assertion
testWitnessWrongUTxO =
  let txbody =
        TxBody
          (Set.fromList [TxIn genesisId 1])
          (StrictSeq.singleton $ TxOut aliceAddr (coinToValue $ Coin 230))
          Empty
          (zeroV)
          (Wdrl Map.empty)
          (Coin 770)
          (SlotNo 100)
          SNothing
          SNothing
      tx2body =
        TxBody
          (Set.fromList [TxIn genesisId 1])
          (StrictSeq.singleton $ TxOut aliceAddr (coinToValue $ Coin 230))
          Empty
          (zeroV)
          (Wdrl Map.empty)
          (Coin 770)
          (SlotNo 101)
          SNothing
          SNothing
      aliceWit = makeWitnessVKey (hashAnnotated tx2body) alicePay
      tx = Tx txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      wits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              [asWitness $ vKey alicePay],
          UtxowFailure $
            MissingVKeyWitnessesUTXOW $
              WitHashes wits
        ]
        tx

testEmptyInputSet :: Assertion
testEmptyInputSet =
  let aliceWithdrawal = Map.singleton (mkVKeyRwdAcnt Testnet aliceStake) (Coin 2000)
      txb =
        TxBody
          Set.empty
          (StrictSeq.singleton $ TxOut aliceAddr (coinToValue $ Coin 1000))
          Empty
          (zeroV)
          (Wdrl aliceWithdrawal)
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      wits = mempty {addrWits = makeWitnessesVKey (hashAnnotated txb) [aliceStake]}
      tx = Tx txb wits SNothing
      dpState' = addReward dpState (getRwdCred $ mkVKeyRwdAcnt Testnet aliceStake) (Coin 2000)
   in testLEDGER
        (utxoState, dpState')
        tx
        ledgerEnv
        (Left [[UtxowFailure (UtxoFailure InputSetEmptyUTxO)]])

testFeeTooSmall :: Assertion
testFeeTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 98) (Coin 1)))]
    $ aliceGivesBobLovelace
      AliceToBob
        { input = (TxIn genesisId 0),
          toBob = (Coin 3000),
          fee = (Coin 1),
          forge = zeroV,
          deposits = (Coin 0),
          refunds = (Coin 0),
          certs = [],
          ttl = (SlotNo 100),
          signers = ([asWitness alicePay])
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
              forge = zeroV,
              deposits = (Coin 0),
              refunds = (Coin 0),
              certs = [],
              ttl = (SlotNo 0),
              signers = ([asWitness alicePay])
            }
      ledgerEnv' = LedgerEnv (SlotNo 1) 0 pp (AccountState 0 0)
   in testLEDGER (utxoState, dpState) tx ledgerEnv' (Left [errs])

testInvalidWintess :: Assertion
testInvalidWintess =
  let txb =
        TxBody
          (Set.fromList [TxIn genesisId 0])
          ( StrictSeq.fromList
              [ TxOut aliceAddr (coinToValue $ Coin 6000),
                TxOut bobAddr (coinToValue $ Coin 3000)
              ]
          )
          Empty
          (zeroV)
          (Wdrl Map.empty)
          (Coin 1000)
          (SlotNo 1)
          SNothing
          SNothing
      txb' = txb {_ttl = SlotNo 2}
      wits = mempty {addrWits = makeWitnessesVKey (hashAnnotated txb') [alicePay]}
      tx = Tx txb wits SNothing
      errs =
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              [asWitness $ vKey alicePay]
        ]
   in testLEDGER (utxoState, dpState) tx ledgerEnv (Left [errs])

testWithdrawalNoWit :: Assertion
testWithdrawalNoWit =
  let txb =
        TxBody
          (Set.fromList [TxIn genesisId 0])
          ( StrictSeq.fromList
              [ TxOut aliceAddr (coinToValue $ Coin 6000),
                TxOut bobAddr (coinToValue $ Coin 3010)
              ]
          )
          Empty
          (zeroV)
          (Wdrl $ Map.singleton (mkVKeyRwdAcnt Testnet bobStake) (Coin 10))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      wits = mempty {addrWits = Set.singleton $ makeWitnessVKey (hashAnnotated txb) alicePay}
      tx = Tx txb wits SNothing
      missing = Set.singleton (asWitness $ hashKey $ vKey bobStake)
      errs =
        [ UtxowFailure . MissingVKeyWitnessesUTXOW $ WitHashes missing
        ]
      dpState' = addReward dpState (getRwdCred $ mkVKeyRwdAcnt Testnet bobStake) (Coin 10)
   in testLEDGER (utxoState, dpState') tx ledgerEnv (Left [errs])

testWithdrawalWrongAmt :: Assertion
testWithdrawalWrongAmt =
  let txb =
        TxBody
          (Set.fromList [TxIn genesisId 0])
          ( StrictSeq.fromList
              [ TxOut aliceAddr (coinToValue $ Coin 6000),
                TxOut bobAddr (coinToValue $ Coin 3011)
              ]
          )
          Empty
          (zeroV)
          (Wdrl $ Map.singleton (mkVKeyRwdAcnt Testnet bobStake) (Coin 11))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      wits =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated txb)
                [asWitness alicePay, asWitness bobStake]
          }
      rAcnt = mkVKeyRwdAcnt Testnet bobStake
      dpState' = addReward dpState (getRwdCred rAcnt) (Coin 10)
      tx = Tx txb wits SNothing
      errs = [DelegsFailure (WithdrawalsNotInRewardsDELEGS (Map.singleton rAcnt (Coin 11)))]
   in testLEDGER (utxoState, dpState') tx ledgerEnv (Left [errs])

testOutputTooSmall :: Assertion
testOutputTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure $ OutputTooSmallUTxO [UTxOOut bobAddr (valueToCompactValue $ coinToValue $ Coin 1)])]
    $ aliceGivesBobLovelace $
      AliceToBob
        { input = (TxIn genesisId 0),
          toBob = (Coin 1), -- Too Small
          fee = (Coin 997),
          forge = zeroV,
          deposits = (Coin 0),
          refunds = (Coin 0),
          certs = [],
          ttl = (SlotNo 0),
          signers = [asWitness alicePay]
        }

alicePoolColdKeys :: KeyPair 'StakePool C
alicePoolColdKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

alicePoolParamsSmallCost :: PoolParams C
alicePoolParamsSmallCost =
  PoolParams
    { _poolPubKey = hashKey . vKey $ alicePoolColdKeys,
      _poolVrf = hashKeyVRF vkVrf,
      _poolPledge = Coin 1,
      _poolCost = Coin 5, -- Too Small!
      _poolMargin = unsafeMkUnitInterval 0.1,
      _poolRAcnt = RewardAcnt Testnet (Credential.KeyHashObj . hashKey . vKey $ aliceStake),
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
    $ aliceGivesBobLovelace $
      AliceToBob
        { input = (TxIn genesisId 0),
          toBob = (Coin 100),
          fee = (Coin 997),
          forge = zeroV,
          deposits = (Coin 250),
          refunds = (Coin 0),
          certs = [DCertPool $ RegPool alicePoolParamsSmallCost],
          ttl = (SlotNo 0),
          signers =
            ( [ asWitness alicePay,
                asWitness aliceStake,
                asWitness alicePoolColdKeys
              ]
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
--      testCase "Invalid Ledger - Alice is trying to forge Ada" testForgingAda,  --TODO make this test
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
      testTruncateUnitInterval,
      testCheckLeaderVal
    ]
