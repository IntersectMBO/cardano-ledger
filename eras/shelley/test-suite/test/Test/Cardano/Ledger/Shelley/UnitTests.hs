{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.UnitTests (unitTests) where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (Addr (..), raCredential, pattern RewardAccount)
import Cardano.Ledger.BaseTypes hiding ((==>))
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential (Credential (..), Ptr (..), SlotNo32 (..), StakeReference (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (
  LedgerEnv (..),
  ShelleyLEDGER,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure (..),
  ShelleyLedgerPredFailure (..),
  ShelleyPoolPredFailure (..),
  ShelleyUtxoPredFailure (..),
  ShelleyUtxowPredFailure (..),
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), Tx (..))
import Cardano.Ledger.Shelley.TxBody (TxBody (..))
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits, addrWits)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>), (<->))
import Cardano.Protocol.Crypto (StandardCrypto, VRF, hashVerKeyVRF)
import Cardano.Protocol.TPraos.BHeader (checkLeaderValue)
import Control.DeepSeq (rnf)
import Control.State.Transition.Extended (PredicateFailure, TRC (..))
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Core.KeyPair (
  KeyPair (..),
  mkVKeyRewardAccount,
  mkWitnessVKey,
  mkWitnessesVKey,
  vKey,
 )
import Test.Cardano.Ledger.Shelley.Arbitrary (
  ASC (ASC),
  StakeProportion (StakeProportion),
 )
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Fees (sizeTests)
import Test.Cardano.Ledger.Shelley.Generator.Core (VRFKeyPair (..), genesisCoins)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Protocol.TPraos.Arbitrary (VRFNatVal (VRFNatVal))
import Test.Control.State.Transition.Trace (checkTrace, (.-), (.->>))
import Test.QuickCheck ((===), (==>))
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Witness)

-- ========================================================================================

alicePay :: KeyPair Payment
alicePay = mkKeyPair' $ RawSeed 1 1 1 1 1

aliceStake :: KeyPair Staking
aliceStake = mkKeyPair' $ RawSeed 2 2 2 2 2

aliceAddr :: Addr
aliceAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey alicePay)
    (StakeRefBase . KeyHashObj . hashKey $ vKey aliceStake)

bobPay :: KeyPair Payment
bobPay = mkKeyPair' $ RawSeed 3 3 3 3 3

bobStake :: KeyPair Staking
bobStake = mkKeyPair' $ RawSeed 4 4 4 4 4

bobAddr :: Addr
bobAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey bobPay)
    (StakeRefBase . KeyHashObj . hashKey $ vKey bobStake)

mkGenesisTxIn :: HasCallStack => Integer -> TxIn
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

pp :: forall era. (EraPParams era, AtMostEra "Mary" era) => PParams era
pp =
  emptyPParams
    & ppMinFeeAL .~ Coin 1
    & ppMinFeeBL .~ Coin 1
    & ppKeyDepositL .~ Coin 100
    & ppPoolDepositL .~ Coin 250
    & ppMaxTxSizeL .~ 1024
    & ppEMaxL .~ EpochInterval 10
    & ppMinUTxOValueL .~ Coin 100
    & ppMinPoolCostL .~ Coin 10

testVRFCheckWithActiveSlotCoeffOne :: Assertion
testVRFCheckWithActiveSlotCoeffOne =
  checkLeaderValue
    (VRF.mkTestOutputVRF 0 :: VRF.OutputVRF (VRF MockCrypto))
    (1 % 2)
    (mkActiveSlotCoeff $ unsafeBoundRational 1)
    @?= True

testsPParams :: TestTree
testsPParams =
  testGroup
    "Test the protocol parameters."
    [ testCase "VRF checks when the activeSlotCoeff is one" testVRFCheckWithActiveSlotCoeffOne
    ]

-- | Test @checkLeaderVal@ in 'Cardano.Ledger.Shelley.BlockChain'
testCheckLeaderVal ::
  forall v.
  v ~ VRF StandardCrypto =>
  TestTree
testCheckLeaderVal =
  testGroup
    "Test checkLeaderVal calculation"
    [ testProperty "With a stake of 0, cannot lead" $
        \(VRFNatVal n) (ASC f) ->
          checkLeaderValue @v (VRF.mkTestOutputVRF n) 0 f == False
    , testProperty "With a maximal VRF, cannot lead" $
        \(ASC f) (StakeProportion r) ->
          checkLeaderValue @v
            (VRF.mkTestOutputVRF maxVRFVal)
            r
            f
            == False
    , testProperty "checkLeaderVal succeeds iff l < 1 - (1-f)^r" $
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
                      )
    , -- Suppose that our VRF value V is drawn uniformly from [0, maxVRFVal).
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
      -- Previously, we chose δ such that this value is quite low (eg, <1/1000).
      -- However, this means the test *will* fail sometimes. If the probability
      -- is 0.0004 and we run the test 100 times for each run of the test
      -- suite, we would expect it to fail approximately once in every 25 test
      -- runs.
      --
      -- We therefore now use a fixed seed so that this is effectively a unit test
      -- and not a property test.
      --
      testProperty "We are elected as leader proportional to our stake" $
        -- 6297 is a seed value that fails
        -- --quickcheck-replay=618 fails when withSeed isn't used
        once . withSeed 12345 $ do
          let numTrials = 2000
          ASC f <- arbitrary
          StakeProportion r <- arbitrary
          vrfVals <- QC.vectorOf numTrials (QC.choose (0, fromIntegral maxVRFVal :: Integer))
          let ascVal :: Double
              ascVal = fromRational . unboundRational $ activeSlotVal f
              -- 4 standard deviations
              δ = 4 * sqrt (realToFrac numTrials * p * (1 - p))
              p = 1 - (1 - ascVal) ** fromRational r
              mean = realToFrac numTrials * p
              lb = floor (mean - δ)
              ub = ceiling (mean + δ)
              check vrf = checkLeaderValue @v (VRF.mkTestOutputVRF $ fromIntegral vrf) r f
              s = length . filter id $ check <$> vrfVals
          pure $
            r > 0 ==>
              counterexample
                (show lb <> " /< " <> show s <> " /< " <> show ub <> " (p=" <> show p <> ")")
                (lb < s && s < ub)
    ]
  where
    maxVRFVal :: Natural
    maxVRFVal = (2 ^ (8 * VRF.sizeOutputVRF (Proxy @v))) - 1
    withSeed i (QC.MkGen f) = QC.MkGen $ \_r n -> f (QC.mkQCGen i) n

testLEDGER ::
  HasCallStack =>
  LedgerState ShelleyEra ->
  Tx TopTx ShelleyEra ->
  LedgerEnv ShelleyEra ->
  Either (NonEmpty (PredicateFailure (ShelleyLEDGER ShelleyEra))) (LedgerState ShelleyEra) ->
  Assertion
testLEDGER initSt tx env (Right expectedSt) = do
  checkTrace @(ShelleyLEDGER ShelleyEra) runShelleyBase env $ pure initSt .- tx .->> expectedSt
testLEDGER initSt tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @(ShelleyLEDGER ShelleyEra) (TRC (env, initSt, tx))
  st @?= predicateFailure

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

data AliceToBob = AliceToBob
  { input :: TxIn
  , toBob :: Coin
  , fee :: Coin
  , deposits :: Coin
  , refunds :: Coin
  , certs :: [TxCert ShelleyEra]
  , ttl :: SlotNo
  , signers :: [KeyPair Witness]
  }

aliceGivesBobLovelace :: AliceToBob -> Tx TopTx ShelleyEra
aliceGivesBobLovelace
  AliceToBob
    { input
    , toBob
    , fee
    , deposits
    , refunds
    , certs
    , ttl
    , signers
    } = MkShelleyTx $ ShelleyTx txbody mempty {addrWits = awits} SNothing
    where
      aliceCoin = aliceInitCoin <+> refunds <-> (toBob <+> fee <+> deposits)
      txbody =
        ShelleyTxBody
          (Set.singleton input)
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr aliceCoin
              , ShelleyTxOut bobAddr toBob
              ]
          )
          (StrictSeq.fromList certs)
          (Withdrawals Map.empty)
          fee
          ttl
          SNothing
          SNothing
      awits = mkWitnessesVKey (hashAnnotated txbody) signers

utxoState :: UTxOState ShelleyEra
utxoState =
  UTxOState
    ( genesisCoins
        genesisId
        [ ShelleyTxOut aliceAddr aliceInitCoin
        , ShelleyTxOut bobAddr (Coin 1000)
        ]
    )
    (Coin 0)
    (Coin 0)
    def
    mempty
    mempty

dpState :: CertState ShelleyEra
dpState = def

ledgerState :: LedgerState ShelleyEra
ledgerState = LedgerState utxoState dpState

addReward :: CertState ShelleyEra -> Credential Staking -> Coin -> CertState ShelleyEra
addReward dp cred c =
  dp
    & certDStateL . accountsL
      %~ addToBalanceAccounts (Map.singleton cred (compactCoinOrError c))
        . registerShelleyAccount cred ptr (CompactCoin 2) Nothing
  where
    ptr = Ptr (SlotNo32 45) (mkTxIxPartial 1234) (mkCertIxPartial 12)

-- Any key deposit works in this test ^
ledgerEnv :: LedgerEnv ShelleyEra
ledgerEnv = LedgerEnv (SlotNo 0) Nothing minBound pp (ChainAccountState (Coin 0) (Coin 0))

testInvalidTx ::
  NonEmpty (PredicateFailure (ShelleyLEDGER ShelleyEra)) ->
  Tx TopTx ShelleyEra ->
  Assertion
testInvalidTx errs tx =
  testLEDGER ledgerState tx ledgerEnv (Left errs)

testSpendNonexistentInput :: Assertion
testSpendNonexistentInput =
  testInvalidTx
    [ UtxowFailure (UtxoFailure (ValueNotConservedUTxO $ Mismatch (Coin 0) (Coin 10000)))
    , UtxowFailure (UtxoFailure $ BadInputsUTxO (Set.singleton $ mkGenesisTxIn 42))
    ]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = mkGenesisTxIn 42 -- Non Existent
      , toBob = Coin 3000
      , fee = Coin 1500
      , deposits = Coin 0
      , refunds = Coin 0
      , certs = []
      , ttl = SlotNo 100
      , signers = [asWitness alicePay]
      }

testWitnessNotIncluded :: Assertion
testWitnessNotIncluded =
  let txbody =
        ShelleyTxBody
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6404)
              , ShelleyTxOut bobAddr (Coin 3000)
              ]
          )
          Empty
          (Withdrawals Map.empty)
          (Coin 596)
          (SlotNo 100)
          SNothing
          SNothing
      tx = ShelleyTx @ShelleyEra txbody mempty SNothing
      txwits = Set.singleton (asWitness $ hashKey $ vKey alicePay)
   in testInvalidTx
        [ UtxowFailure $
            MissingVKeyWitnessesUTXOW txwits
        ]
        (MkShelleyTx tx)

testSpendNotOwnedUTxO :: Assertion
testSpendNotOwnedUTxO =
  let txbody =
        ShelleyTxBody
          (Set.fromList [mkGenesisTxIn 1])
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 232))
          Empty
          (Withdrawals Map.empty)
          (Coin 768)
          (SlotNo 100)
          SNothing
          SNothing
      aliceWit = mkWitnessVKey (hashAnnotated txbody) alicePay
      tx = MkShelleyTx $ ShelleyTx @ShelleyEra txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      txwits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $
            MissingVKeyWitnessesUTXOW txwits
        ]
        tx

testWitnessWrongUTxO :: Assertion
testWitnessWrongUTxO =
  let txbody =
        ShelleyTxBody
          (Set.fromList [mkGenesisTxIn 1])
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 230))
          Empty
          (Withdrawals Map.empty)
          (Coin 770)
          (SlotNo 100)
          SNothing
          SNothing
      tx2body =
        ShelleyTxBody
          (Set.fromList [mkGenesisTxIn 1])
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 230))
          Empty
          (Withdrawals Map.empty)
          (Coin 770)
          (SlotNo 101)
          SNothing
          SNothing
      aliceWit = mkWitnessVKey (hashAnnotated tx2body) alicePay
      tx = ShelleyTx @ShelleyEra txbody mempty {addrWits = Set.fromList [aliceWit]} SNothing
      txwits = Set.singleton (asWitness $ hashKey $ vKey bobPay)
   in testInvalidTx
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              [asWitness $ vKey alicePay]
        , UtxowFailure $
            MissingVKeyWitnessesUTXOW txwits
        ]
        (MkShelleyTx tx)

testEmptyInputSet :: Assertion
testEmptyInputSet =
  let aliceWithdrawal = Map.singleton (mkVKeyRewardAccount Testnet aliceStake) (Coin 2000)
      txb =
        ShelleyTxBody
          Set.empty
          (StrictSeq.singleton $ ShelleyTxOut aliceAddr (Coin 1000))
          Empty
          (Withdrawals aliceWithdrawal)
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      txwits = mempty {addrWits = mkWitnessesVKey (hashAnnotated txb) [aliceStake]}
      tx = ShelleyTx txb txwits SNothing
      dpState' = addReward dpState (raCredential $ mkVKeyRewardAccount Testnet aliceStake) (Coin 2000)
   in testLEDGER
        (LedgerState utxoState dpState')
        (MkShelleyTx tx)
        ledgerEnv
        (Left [UtxowFailure (UtxoFailure InputSetEmptyUTxO)])

testFeeTooSmall :: Assertion
testFeeTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure (FeeTooSmallUTxO $ Mismatch (Coin 1) (Coin 279)))]
    $ aliceGivesBobLovelace
      AliceToBob
        { input = TxIn genesisId minBound
        , toBob = Coin 3000
        , fee = Coin 1
        , deposits = Coin 0
        , refunds = Coin 0
        , certs = []
        , ttl = SlotNo 100
        , signers = [asWitness alicePay]
        }

testExpiredTx :: Assertion
testExpiredTx =
  let errs =
        [ UtxowFailure (UtxoFailure (ExpiredUTxO $ Mismatch (SlotNo {unSlotNo = 0}) (SlotNo {unSlotNo = 1})))
        ]
      tx =
        aliceGivesBobLovelace $
          AliceToBob
            { input = TxIn genesisId minBound
            , toBob = Coin 3000
            , fee = Coin 600
            , deposits = Coin 0
            , refunds = Coin 0
            , certs = []
            , ttl = SlotNo 0
            , signers = [asWitness alicePay]
            }
      ledgerEnv' = LedgerEnv (SlotNo 1) Nothing minBound pp (ChainAccountState (Coin 0) (Coin 0))
   in testLEDGER ledgerState tx ledgerEnv' (Left errs)

testInvalidWintess :: Assertion
testInvalidWintess =
  let txb =
        ShelleyTxBody
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6000)
              , ShelleyTxOut bobAddr (Coin 3000)
              ]
          )
          Empty
          (Withdrawals Map.empty)
          (Coin 1000)
          (SlotNo 1)
          SNothing
          SNothing
      txb' = txb {stbTTL = SlotNo 2}
      txwits :: Cardano.Ledger.Shelley.TxWits.ShelleyTxWits ShelleyEra
      txwits = mempty {addrWits = mkWitnessesVKey (hashAnnotated txb') [alicePay]}
      tx = ShelleyTx @ShelleyEra txb txwits SNothing
      errs =
        [ UtxowFailure $
            InvalidWitnessesUTXOW
              [asWitness $ vKey alicePay]
        ]
   in testLEDGER ledgerState (MkShelleyTx tx) ledgerEnv (Left errs)

testWithdrawalNoWit :: Assertion
testWithdrawalNoWit =
  let txb =
        ShelleyTxBody
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6000)
              , ShelleyTxOut bobAddr (Coin 3010)
              ]
          )
          Empty
          (Withdrawals $ Map.singleton (mkVKeyRewardAccount Testnet bobStake) (Coin 10))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      txwits :: ShelleyTxWits ShelleyEra
      txwits = mempty {addrWits = Set.singleton $ mkWitnessVKey (hashAnnotated txb) alicePay}
      tx = ShelleyTx @ShelleyEra txb txwits SNothing
      missing = Set.singleton (asWitness $ hashKey $ vKey bobStake)
      errs =
        [ UtxowFailure $ MissingVKeyWitnessesUTXOW missing
        ]
      dpState' = addReward dpState (raCredential $ mkVKeyRewardAccount Testnet bobStake) (Coin 10)
   in testLEDGER (LedgerState utxoState dpState') (MkShelleyTx tx) ledgerEnv (Left errs)

testWithdrawalWrongAmt :: Assertion
testWithdrawalWrongAmt =
  let txb =
        ShelleyTxBody
          (Set.fromList [TxIn genesisId minBound])
          ( StrictSeq.fromList
              [ ShelleyTxOut aliceAddr (Coin 6000)
              , ShelleyTxOut bobAddr (Coin 3011)
              ]
          )
          Empty
          (Withdrawals $ Map.singleton (mkVKeyRewardAccount Testnet bobStake) (Coin 11))
          (Coin 1000)
          (SlotNo 0)
          SNothing
          SNothing
      txwits =
        mempty
          { addrWits =
              mkWitnessesVKey
                (hashAnnotated txb)
                [asWitness alicePay, asWitness bobStake]
          }
      rAccount = mkVKeyRewardAccount Testnet bobStake
      dpState' = addReward dpState (raCredential rAccount) (Coin 10)
      tx = MkShelleyTx $ ShelleyTx @ShelleyEra txb txwits SNothing
      errs = [ShelleyIncompleteWithdrawals $ Withdrawals $ Map.singleton rAccount $ Coin 11]
   in testLEDGER (LedgerState utxoState dpState') tx ledgerEnv (Left errs)

testOutputTooSmall :: Assertion
testOutputTooSmall =
  testInvalidTx
    [UtxowFailure (UtxoFailure $ OutputTooSmallUTxO [ShelleyTxOut bobAddr (Coin 1)])]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = TxIn genesisId minBound
      , toBob = Coin 1 -- Too Small
      , fee = Coin 997
      , deposits = Coin 0
      , refunds = Coin 0
      , certs = []
      , ttl = SlotNo 0
      , signers = [asWitness alicePay]
      }

alicePoolColdKeys :: KeyPair StakePool
alicePoolColdKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 1)

aliceStakePoolParamsSmallCost :: StakePoolParams
aliceStakePoolParamsSmallCost =
  StakePoolParams
    { sppId = hashKey $ vKey alicePoolColdKeys
    , sppVrf = hashVerKeyVRF @MockCrypto vkVrf
    , sppPledge = Coin 1
    , sppCost = Coin 5 -- Too Small!
    , sppMargin = unsafeBoundRational 0.1
    , sppRewardAccount = RewardAccount Testnet (KeyHashObj . hashKey . vKey $ aliceStake)
    , sppOwners = Set.singleton $ (hashKey . vKey) aliceStake
    , sppRelays = StrictSeq.empty
    , sppMetadata =
        SJust $
          PoolMetadata
            { pmUrl = fromJust $ textToUrl 64 "alice.pool"
            , pmHash = BS.pack "{}"
            }
    }
  where
    vkVrf = vrfVerKey $ mkVRFKeyPair @MockCrypto (RawSeed 0 0 0 0 2)

testPoolCostTooSmall :: Assertion
testPoolCostTooSmall =
  testInvalidTx
    [ DelegsFailure $
        DelplFailure $
          PoolFailure $
            StakePoolCostTooLowPOOL $
              Mismatch (sppCost aliceStakePoolParamsSmallCost) (pp @ShelleyEra ^. ppMinPoolCostL)
    ]
    $ aliceGivesBobLovelace
    $ AliceToBob
      { input = TxIn genesisId minBound
      , toBob = Coin 100
      , fee = Coin 997
      , deposits = Coin 250
      , refunds = Coin 0
      , certs = [ShelleyTxCertPool $ RegPool aliceStakePoolParamsSmallCost]
      , ttl = SlotNo 0
      , signers =
          ( [ asWitness alicePay
            , asWitness aliceStake
            , asWitness alicePoolColdKeys
            ]
          )
      }

testProducedOverMaxWord64 :: Assertion
testProducedOverMaxWord64 =
  let biggestCoin = fromIntegral (maxBound :: Word64)
      txbody =
        ShelleyTxBody
          (Set.fromList [TxIn genesisId minBound])
          (StrictSeq.fromList [ShelleyTxOut bobAddr (Coin biggestCoin)])
          Empty
          (Withdrawals Map.empty)
          (Coin 1) -- @produced@ will return biggestCoin + 1, which is > 2^64.
          (SlotNo 100)
          SNothing
          SNothing
      txwits = mempty {addrWits = mkWitnessesVKey (hashAnnotated txbody) [alicePay]}
      tx = ShelleyTx @ShelleyEra txbody txwits SNothing
      st =
        runShelleyBase $
          applySTSTest @(ShelleyLEDGER ShelleyEra) (TRC (ledgerEnv, ledgerState, MkShelleyTx tx))
   in -- We test that the predicate failure does not return bottom
      pure $! rnf st

testsInvalidLedger :: TestTree
testsInvalidLedger =
  testGroup
    "Tests with invalid transactions in ledger"
    [ testCase "Invalid Ledger - Alice tries to spend a nonexistent input" testSpendNonexistentInput
    , testCase "Invalid Ledger - Alice does not include a witness" testWitnessNotIncluded
    , testCase "Invalid Ledger - Alice tries to spend Bob's UTxO" testSpendNotOwnedUTxO
    , testCase "Invalid Ledger - Alice provides witness of wrong UTxO" testWitnessWrongUTxO
    , testCase "Invalid Ledger - Alice's transaction does not consume input" testEmptyInputSet
    , testCase "Invalid Ledger - Alice's fee is too small" testFeeTooSmall
    , testCase "Invalid Ledger - Alice's transaction has expired" testExpiredTx
    , testCase "Invalid Ledger - Invalid witnesses" testInvalidWintess
    , testCase "Invalid Ledger - No withdrawal witness" testWithdrawalNoWit
    , testCase "Invalid Ledger - Incorrect withdrawal amount" testWithdrawalWrongAmt
    , testCase "Invalid Ledger - OutputTooSmall" testOutputTooSmall
    , testCase "Invalid Ledger - PoolCostTooSmall" testPoolCostTooSmall
    , testCase "Invalid Ledger - ProducedOverMaxWord64" testProducedOverMaxWord64
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testsInvalidLedger
    , testsPParams
    , sizeTests
    , testCheckLeaderVal
    ]
