{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.UnitTests (unitTests) where

import Cardano.Crypto.DSIGN.Class (SignKeyDSIGN, VerKeyDSIGN)
import Cardano.Crypto.Hash.Class (HashAlgorithm)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address
  ( Addr (..),
    getRwdCred,
    mkVKeyRwdAcnt,
  )
import Cardano.Ledger.BaseTypes hiding ((==>))
import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
  ( Credential (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto (DSIGN, HASH, VRF)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
    asWitness,
    hashKey,
    hashVerKeyVRF,
    vKey,
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API
  ( DCert (..),
    LedgerEnv (..),
    ShelleyLEDGER,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (pattern RegPool)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    IncrementalStake (..),
    LedgerState (..),
    UTxOState (..),
    dpsDState,
    dsUnified,
    rewards,
  )
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Rules
  ( ShelleyDelegsPredFailure (..),
    ShelleyDelplPredFailure (..),
    ShelleyLedgerPredFailure (..),
    ShelleyPoolPredFailure (..),
    ShelleyUtxoPredFailure (..),
    ShelleyUtxowPredFailure (..),
  )
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx (..),
    ShelleyTxBody (..),
    ShelleyTxOut (..),
    TxIn (..),
    stbTTL,
  )
import Cardano.Ledger.Shelley.TxBody
  ( PoolMetadata (..),
    PoolParams (..),
    Wdrl (..),
    pmHash,
    pmUrl,
    ppCost,
    ppId,
    ppMargin,
    ppMetadata,
    ppOwners,
    ppPledge,
    ppRelays,
    ppRewardAcnt,
    ppVrf,
    pattern RewardAcnt,
  )
import Cardano.Ledger.Shelley.TxWits
  ( ShelleyTxWits,
    addrWits,
  )
import Cardano.Ledger.UTxO (makeWitnessVKey, makeWitnessesVKey)
import Cardano.Ledger.Val ((<+>), (<->))
import Cardano.Protocol.TPraos.BHeader (checkLeaderValue)
import Control.State.Transition.Extended (PredicateFailure, TRC (..))
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.UMap as UM
import Data.Word (Word64)
import GHC.Stack
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Shelley.Address.Bootstrap
  ( testBootstrapNotSpending,
    testBootstrapSpending,
  )
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, C_Crypto)
import Test.Cardano.Ledger.Shelley.Fees (sizeTests)
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( genesisCoins,
  )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils
import qualified Test.QuickCheck.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- ========================================================================================

-- | - This constraint says we can coerce Numbers into a signable keys
type NumKey c =
  ( Num (SignKeyDSIGN (DSIGN c)),
    Num (VerKeyDSIGN (DSIGN c))
  )

alicePay :: NumKey c => KeyPair 'Payment c
alicePay = KeyPair 1 1

aliceStake :: NumKey c => KeyPair 'Staking c
aliceStake = KeyPair 2 2

aliceAddr :: (NumKey c, CC.Crypto c) => Addr c
aliceAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey alicePay)
    (StakeRefBase . KeyHashObj . hashKey $ vKey aliceStake)

bobPay :: NumKey c => KeyPair 'Payment c
bobPay = KeyPair 3 3

bobStake :: NumKey c => KeyPair 'Staking c
bobStake = KeyPair 4 4

bobAddr :: (NumKey c, CC.Crypto c) => Addr c
bobAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey bobPay)
    (StakeRefBase . KeyHashObj . hashKey $ vKey bobStake)

mkGenesisTxIn :: (HashAlgorithm (HASH c), HasCallStack) => Integer -> TxIn c
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

pp :: ShelleyPParams era
pp =
  emptyPParams
    { _minfeeA = 1,
      _minfeeB = 1,
      _keyDeposit = Coin 100,
      _poolDeposit = Coin 250,
      _maxTxSize = 1024,
      _eMax = EpochNo 10,
      _minUTxOValue = Coin 100,
      _minPoolCost = Coin 100
    }

testVRFCheckWithActiveSlotCoeffOne :: Assertion
testVRFCheckWithActiveSlotCoeffOne =
  checkLeaderValue
    (VRF.mkTestOutputVRF 0 :: VRF.OutputVRF (VRF C_Crypto))
    (1 % 2)
    (mkActiveSlotCoeff $ unsafeBoundRational 1)
    @?= True

testsPParams :: TestTree
testsPParams =
  testGroup
    "Test the protocol parameters."
    [ testCase "VRF checks when the activeSlotCoeff is one" testVRFCheckWithActiveSlotCoeffOne
    ]

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
                    (Proxy @(VRF C_Crypto))
              )
        )
  shrink (VRFNatVal v) = VRFNatVal <$> shrinkIntegral v

newtype ASC = ASC ActiveSlotCoeff
  deriving (Show)

instance Arbitrary ASC where
  arbitrary =
    ASC
      . mkActiveSlotCoeff
      . unsafeBoundRational
      . fromRational
      . toRational
      <$> choose @Double (0.01, 0.5)

newtype StakeProportion = StakeProportion Rational
  deriving (Show)

instance Arbitrary StakeProportion where
  arbitrary = StakeProportion . toRational <$> choose @Double (0, 1)
  shrink (StakeProportion r) = StakeProportion <$> shrinkRealFrac r

-- | Test @checkLeaderVal@ in 'Cardano.Ledger.Shelley.BlockChain'
testCheckLeaderVal ::
  forall v.
  (v ~ VRF C_Crypto) =>
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
          r > 0 ==>
            let ascVal :: Double
                ascVal = fromRational . unboundRational $ activeSlotVal f
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
          r > 0 ==>
            let ascVal :: Double
                ascVal = fromRational . unboundRational $ activeSlotVal f
                numTrials = 2000
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
  HasCallStack =>
  LedgerState C ->
  ShelleyTx C ->
  LedgerEnv C ->
  Either [PredicateFailure (ShelleyLEDGER C)] (LedgerState C) ->
  Assertion
testLEDGER initSt tx env (Right expectedSt) = do
  checkTrace @(ShelleyLEDGER C) runShelleyBase env $ pure initSt .- tx .-> expectedSt
testLEDGER initSt tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @(ShelleyLEDGER C) (TRC (env, initSt, tx))
  st @?= predicateFailure

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

data AliceToBob = AliceToBob
  { input :: TxIn C_Crypto,
    toBob :: Coin,
    fee :: Coin,
    deposits :: Coin,
    refunds :: Coin,
    certs :: [DCert C_Crypto],
    ttl :: SlotNo,
    signers :: [KeyPair 'Witness C_Crypto]
  }

aliceGivesBobLovelace :: AliceToBob -> ShelleyTx C
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
    } = ShelleyTx txbody mempty {addrWits = awits} SNothing
    where
      aliceCoin = aliceInitCoin <+> refunds <-> (toBob <+> fee <+> deposits)
      txbody =
        ShelleyTxBody
          (Set.singleton input)
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr aliceCoin,
                ShelleyTxOut bobAddr toBob
              ]
          )
          (StrictSeq.fromList certs)
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
        genesisId
        [ ShelleyTxOut aliceAddr aliceInitCoin,
          ShelleyTxOut bobAddr (Coin 1000)
        ]
    )
    (Coin 0)
    (Coin 0)
    def
    (IStake mempty mempty)

dpState :: DPState C_Crypto
dpState = DPState def def

ledgerState :: LedgerState C
ledgerState = LedgerState utxoState dpState

addReward :: DPState C_Crypto -> Credential 'Staking C_Crypto -> Coin -> DPState C_Crypto
addReward dp ra c = dp {dpsDState = ds {dsUnified = rewards'}}
  where
    ds = dpsDState dp
    rewards' = UM.insert ra c (rewards ds)

ledgerEnv :: LedgerEnv C
ledgerEnv = LedgerEnv (SlotNo 0) minBound pp (AccountState (Coin 0) (Coin 0))

testInvalidTx ::
  [PredicateFailure (ShelleyLEDGER C)] ->
  ShelleyTx C ->
  Assertion
testInvalidTx errs tx =
  testLEDGER ledgerState tx ledgerEnv (Left errs)

testSpendNonexistentInput :: Assertion
testSpendNonexistentInput =
  testInvalidTx
    [ UtxowFailure (UtxoFailure (ValueNotConservedUTxO (Coin 0) (Coin 10000))),
      UtxowFailure (UtxoFailure $ BadInputsUTxO (Set.singleton $ mkGenesisTxIn 42))
    ]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = mkGenesisTxIn 42, -- Non Existent
        toBob = Coin 3000,
        fee = Coin 1500,
        deposits = Coin 0,
        refunds = Coin 0,
        certs = [],
        ttl = SlotNo 100,
        signers = [asWitness alicePay]
      }

testWitnessNotIncluded :: Assertion
testWitnessNotIncluded =
  let txbody =
        ShelleyTxBody @C
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6404),
                ShelleyTxOut bobAddr (Coin 3000)
              ]
          )
          Empty
          (Wdrl Map.empty)
          (Coin 596)
          (SlotNo 100)
          SNothing
          SNothing
      tx = ShelleyTx @C txbody mempty SNothing
      txwits = Set.singleton (asWitness $ hashKey $ vKey alicePay)
   in testInvalidTx
        [ UtxowFailure $
            MissingVKeyWitnessesUTXOW txwits
        ]
        tx

testSpendNotOwnedUTxO :: Assertion
testSpendNotOwnedUTxO =
  let txbody =
        ShelleyTxBody @C
          (Set.fromList [mkGenesisTxIn 1])
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 232))
          Empty
          (Wdrl Map.empty)
          (Coin 768)
          (SlotNo 100)
          SNothing
          SNothing
      aliceWit = makeWitnessVKey (hashAnnotated txbody) alicePay
      tx = ShelleyTx @C txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      txwits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $
            MissingVKeyWitnessesUTXOW txwits
        ]
        tx

testWitnessWrongUTxO :: Assertion
testWitnessWrongUTxO =
  let txbody =
        ShelleyTxBody @C
          (Set.fromList [mkGenesisTxIn 1])
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 230))
          Empty
          (Wdrl Map.empty)
          (Coin 770)
          (SlotNo 100)
          SNothing
          SNothing
      tx2body =
        ShelleyTxBody @C
          (Set.fromList [mkGenesisTxIn 1])
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 230))
          Empty
          (Wdrl Map.empty)
          (Coin 770)
          (SlotNo 101)
          SNothing
          SNothing
      aliceWit = makeWitnessVKey (hashAnnotated tx2body) alicePay
      tx = ShelleyTx @C txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      txwits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              [asWitness $ vKey alicePay],
          UtxowFailure $
            MissingVKeyWitnessesUTXOW txwits
        ]
        tx

testEmptyInputSet :: Assertion
testEmptyInputSet =
  let aliceWithdrawal = Map.singleton (mkVKeyRwdAcnt Testnet aliceStake) (Coin 2000)
      txb =
        ShelleyTxBody
          Set.empty
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 1000))
          Empty
          (Wdrl aliceWithdrawal)
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      txwits = mempty {addrWits = makeWitnessesVKey (hashAnnotated txb) [aliceStake]}
      tx = ShelleyTx txb txwits SNothing
      dpState' = addReward dpState (getRwdCred $ mkVKeyRwdAcnt Testnet aliceStake) (Coin 2000)
   in testLEDGER
        (LedgerState utxoState dpState')
        tx
        ledgerEnv
        (Left [UtxowFailure (UtxoFailure InputSetEmptyUTxO)])

testFeeTooSmall :: Assertion
testFeeTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 205) (Coin 1)))]
    $ aliceGivesBobLovelace
      AliceToBob
        { input = TxIn genesisId minBound,
          toBob = Coin 3000,
          fee = Coin 1,
          deposits = Coin 0,
          refunds = Coin 0,
          certs = [],
          ttl = SlotNo 100,
          signers = [asWitness alicePay]
        }

testExpiredTx :: Assertion
testExpiredTx =
  let errs = [UtxowFailure (UtxoFailure (ExpiredUTxO (SlotNo {unSlotNo = 0}) (SlotNo {unSlotNo = 1})))]
      tx =
        aliceGivesBobLovelace $
          AliceToBob
            { input = TxIn genesisId minBound,
              toBob = Coin 3000,
              fee = Coin 600,
              deposits = Coin 0,
              refunds = Coin 0,
              certs = [],
              ttl = SlotNo 0,
              signers = [asWitness alicePay]
            }
      ledgerEnv' = LedgerEnv (SlotNo 1) minBound pp (AccountState (Coin 0) (Coin 0))
   in testLEDGER ledgerState tx ledgerEnv' (Left errs)

testInvalidWintess :: Assertion
testInvalidWintess =
  let txb =
        ShelleyTxBody @C
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6000),
                ShelleyTxOut bobAddr (Coin 3000)
              ]
          )
          Empty
          (Wdrl Map.empty)
          (Coin 1000)
          (SlotNo 1)
          SNothing
          SNothing
      txb' = txb {stbTTL = SlotNo 2}
      txwits :: Cardano.Ledger.Shelley.TxWits.ShelleyTxWits C
      txwits = mempty {addrWits = makeWitnessesVKey (hashAnnotated txb') [alicePay]}
      tx = ShelleyTx @C txb txwits SNothing
      errs =
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              [asWitness $ vKey alicePay]
        ]
   in testLEDGER ledgerState tx ledgerEnv (Left errs)

testWithdrawalNoWit :: Assertion
testWithdrawalNoWit =
  let txb =
        ShelleyTxBody @C
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6000),
                ShelleyTxOut bobAddr (Coin 3010)
              ]
          )
          Empty
          (Wdrl $ Map.singleton (mkVKeyRwdAcnt Testnet bobStake) (Coin 10))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      txwits :: ShelleyTxWits C
      txwits = mempty {addrWits = Set.singleton $ makeWitnessVKey (hashAnnotated txb) alicePay}
      tx = ShelleyTx @C txb txwits SNothing
      missing = Set.singleton (asWitness $ hashKey $ vKey bobStake)
      errs =
        [ UtxowFailure $ MissingVKeyWitnessesUTXOW missing
        ]
      dpState' = addReward dpState (getRwdCred $ mkVKeyRwdAcnt Testnet bobStake) (Coin 10)
   in testLEDGER (LedgerState utxoState dpState') tx ledgerEnv (Left errs)

testWithdrawalWrongAmt :: Assertion
testWithdrawalWrongAmt =
  let txb =
        ShelleyTxBody @C
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6000),
                ShelleyTxOut bobAddr (Coin 3011)
              ]
          )
          Empty
          (Wdrl $ Map.singleton (mkVKeyRwdAcnt Testnet bobStake) (Coin 11))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      txwits =
        mempty
          { addrWits =
              makeWitnessesVKey @C_Crypto
                (hashAnnotated txb)
                [asWitness alicePay, asWitness bobStake]
          }
      rAcnt = mkVKeyRwdAcnt Testnet bobStake
      dpState' = addReward dpState (getRwdCred rAcnt) (Coin 10)
      tx = ShelleyTx @C txb txwits SNothing
      errs = [DelegsFailure (WithdrawalsNotInRewardsDELEGS (Map.singleton rAcnt (Coin 11)))]
   in testLEDGER (LedgerState utxoState dpState') tx ledgerEnv (Left errs)

testOutputTooSmall :: Assertion
testOutputTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure $ OutputTooSmallUTxO [ShelleyTxOut bobAddr (Coin 1)])]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = TxIn genesisId minBound,
        toBob = Coin 1, -- Too Small
        fee = Coin 997,
        deposits = Coin 0,
        refunds = Coin 0,
        certs = [],
        ttl = SlotNo 0,
        signers = [asWitness alicePay]
      }

alicePoolColdKeys :: KeyPair 'StakePool C_Crypto
alicePoolColdKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 1)

alicePoolParamsSmallCost :: PoolParams C_Crypto
alicePoolParamsSmallCost =
  PoolParams
    { ppId = hashKey . vKey $ alicePoolColdKeys,
      ppVrf = hashVerKeyVRF vkVrf,
      ppPledge = Coin 1,
      ppCost = Coin 5, -- Too Small!
      ppMargin = unsafeBoundRational 0.1,
      ppRewardAcnt = RewardAcnt Testnet (KeyHashObj . hashKey . vKey $ aliceStake),
      ppOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      ppRelays = StrictSeq.empty,
      ppMetadata =
        SJust $
          PoolMetadata
            { pmUrl = fromJust $ textToUrl "alice.pool",
              pmHash = BS.pack "{}"
            }
    }
  where
    (_skVrf, vkVrf) = mkVRFKeyPair (RawSeed 0 0 0 0 2)

testPoolCostTooSmall :: Assertion
testPoolCostTooSmall =
  testInvalidTx
    [ DelegsFailure
        ( DelplFailure
            ( PoolFailure
                ( StakePoolCostTooLowPOOL (ppCost alicePoolParamsSmallCost) (_minPoolCost (pp @C))
                )
            )
        )
    ]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = TxIn genesisId minBound,
        toBob = Coin 100,
        fee = Coin 997,
        deposits = Coin 250,
        refunds = Coin 0,
        certs = [DCertPool $ RegPool alicePoolParamsSmallCost],
        ttl = SlotNo 0,
        signers =
          ( [ asWitness alicePay,
              asWitness aliceStake,
              asWitness alicePoolColdKeys
            ]
          )
      }

testProducedOverMaxWord64 :: Assertion
testProducedOverMaxWord64 =
  let biggestCoin = fromIntegral (maxBound :: Word64)
      txbody =
        ShelleyTxBody @C
          (Set.fromList [TxIn genesisId minBound])
          (StrictSeq.fromList [ShelleyTxOut bobAddr (Coin biggestCoin)])
          Empty
          (Wdrl Map.empty)
          (Coin 1) -- @produced@ will return biggestCoin + 1, which is > 2^64.
          (SlotNo 100)
          SNothing
          SNothing
      txwits = mempty {addrWits = makeWitnessesVKey @C_Crypto (hashAnnotated txbody) [alicePay]}
      tx = ShelleyTx @C txbody txwits SNothing
      st =
        runShelleyBase $
          applySTSTest @(ShelleyLEDGER C) (TRC (ledgerEnv, ledgerState, tx))
   in -- We test that the serialization of the predicate failure does not return bottom
      serialize' shelleyProtVer st @?= serialize' shelleyProtVer st

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
      testCase "Invalid Ledger - PoolCostTooSmall" testPoolCostTooSmall,
      testCase "Invalid Ledger - ProducedOverMaxWord64" testProducedOverMaxWord64
    ]

testBootstrap :: TestTree
testBootstrap =
  testGroup
    "bootstrap witnesses"
    [ testCase "spend from a bootstrap address" testBootstrapSpending,
      testCase "don't spend from a bootstrap address" testBootstrapNotSpending
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testsInvalidLedger,
      testsPParams,
      sizeTests,
      testCheckLeaderVal,
      testBootstrap
    ]
