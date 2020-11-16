-- |
-- Module      : Test.Cardano.Ledger.Mary.Examples.MultiAssets
-- Description : Multi-Assets Examples
--
-- Examples demonstrating the use of multi-assets.
module Test.Cardano.Ledger.Mary.Examples.MultiAssets
  ( multiAssetsExample,
  )
where

import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (TxBody (..))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition.Extended (PredicateFailure)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API (LEDGER, LedgerEnv (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Hashing (HashAnnotated (hashAnnotated))
import Shelley.Spec.Ledger.Keys (KeyPair (..), asWitness, hashKey)
import Shelley.Spec.Ledger.LedgerState (AccountState (..))
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), emptyPParams)
import Shelley.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
    hashScript,
  )
import Shelley.Spec.Ledger.TxBody
  ( TxId,
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import Test.Cardano.Ledger.EraBuffet (MaryTest)
import Test.Cardano.Ledger.Mary.Examples (testMaryNoDelegLEDGER)
import qualified Test.Cardano.Ledger.Mary.Examples.Cast as Cast
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

------------------------------
-- Set Up the Initial State --
------------------------------

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

unboundedInterval :: ValidityInterval
unboundedInterval = ValidityInterval SNothing SNothing

bootstrapTxId :: TxId MaryTest
bootstrapTxId = txid txb
  where
    txb =
      TxBody
        Set.empty
        StrictSeq.empty
        StrictSeq.empty
        (Wdrl Map.empty)
        (Coin 0)
        unboundedInterval
        SNothing
        SNothing
        (Val.inject (Coin 0))

initUTxO :: UTxO MaryTest
initUTxO =
  UTxO $
    Map.fromList
      [ (TxIn bootstrapTxId 0, TxOut Cast.aliceAddr (Val.inject aliceInitCoin)),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

pp :: PParams MaryTest
pp =
  emptyPParams
    { _minfeeA = 0,
      _minfeeB = 1,
      _maxTxSize = 1024,
      _minUTxOValue = Coin 100
    }

ledgerEnv :: SlotNo -> LedgerEnv MaryTest
ledgerEnv s = LedgerEnv s 0 pp (AccountState (Coin 0) (Coin 0))

feeEx :: Coin
feeEx = Coin 3

-- These examples do not use several of the transaction components,
-- so we can simplify building them.
makeTxb ::
  [TxIn MaryTest] ->
  [TxOut MaryTest] ->
  ValidityInterval ->
  Value MaryTest ->
  TxBody MaryTest
makeTxb ins outs interval minted =
  TxBody
    (Set.fromList ins)
    (StrictSeq.fromList outs)
    StrictSeq.empty
    (Wdrl Map.empty)
    feeEx
    interval
    SNothing
    SNothing
    minted

policyFailure :: PolicyID MaryTest -> Either [[PredicateFailure (LEDGER MaryTest)]] (UTxO MaryTest)
policyFailure p =
  Left
    [ [ UtxowFailure
          ( ScriptWitnessNotValidatingUTXOW
              (Set.singleton (policyID p))
          )
      ]
    ]

-------------------------------------------------
-- Introduce a new Token Bundle, Purple Tokens --
-------------------------------------------------

-- This is the most lax policy possible, requiring no authorization at all.
purplePolicy :: Timelock MaryTest
purplePolicy = RequireAllOf (StrictSeq.fromList [])

purplePolicyId :: PolicyID MaryTest
purplePolicyId = PolicyID $ hashScript purplePolicy

plum :: AssetName
plum = AssetName $ BS.pack "plum"

amethyst :: AssetName
amethyst = AssetName $ BS.pack "amethyst"

------------------------
-- Mint Purple Tokens --
------------------------

purpleTokensEx1 :: Value MaryTest
purpleTokensEx1 =
  Value 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> feeEx

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodyEx1 :: TxBody MaryTest
txbodyEx1 =
  makeTxb
    [TxIn bootstrapTxId 0]
    [ TxOut Cast.aliceAddr (Val.inject aliceCoinEx1),
      TxOut Cast.aliceAddr purpleTokensEx1
    ]
    unboundedInterval
    purpleTokensEx1

txEx1 :: Tx MaryTest
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodyEx1) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
      }
    SNothing

expectedUTxOEx1 :: UTxO MaryTest
expectedUTxOEx1 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyEx1) 0, TxOut Cast.aliceAddr (Val.inject aliceCoinEx1)),
        (TxIn (txid txbodyEx1) 1, TxOut Cast.aliceAddr purpleTokensEx1),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------
-- Transfer Purple Tokens --
----------------------------

feeTx2 :: Coin
feeTx2 = Coin 3

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 <-> feeTx2

purpleTokensAliceEx2 :: Value MaryTest
purpleTokensAliceEx2 =
  Value (unCoin aliceCoinEx2) $
    Map.singleton purplePolicyId (Map.fromList [(plum, 8), (amethyst, 2)])

purpleTokensBobEx2 :: Value MaryTest
purpleTokensBobEx2 =
  Value 0 $
    Map.singleton purplePolicyId (Map.singleton plum 5)

-- Alice gives five plums to Bob.
txbodyEx2 :: TxBody MaryTest
txbodyEx2 =
  makeTxb
    [TxIn (txid txbodyEx1) 0, TxIn (txid txbodyEx1) 1]
    [ TxOut Cast.aliceAddr purpleTokensAliceEx2,
      TxOut Cast.bobAddr purpleTokensBobEx2
    ]
    unboundedInterval
    Val.zero

txEx2 :: Tx MaryTest
txEx2 =
  Tx
    txbodyEx2
    mempty {addrWits = makeWitnessesVKey (hashAnnotated txbodyEx2) [asWitness Cast.alicePay]}
    SNothing

expectedUTxOEx2 :: UTxO MaryTest
expectedUTxOEx2 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyEx2) 0, TxOut Cast.aliceAddr purpleTokensAliceEx2),
        (TxIn (txid txbodyEx2) 1, TxOut Cast.bobAddr purpleTokensBobEx2),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens With a Time Range --
------------------------------------------------------------

beforeStart :: SlotNo
beforeStart = SlotNo 12

startInterval :: SlotNo
startInterval = SlotNo 13

stopInterval :: SlotNo
stopInterval = SlotNo 19

afterStop :: SlotNo
afterStop = SlotNo 20

boundedTimePolicy :: Timelock MaryTest
boundedTimePolicy =
  RequireAllOf
    ( StrictSeq.fromList
        [ RequireTimeStart startInterval,
          RequireTimeExpire stopInterval
        ]
    )

boundedTimePolicyId :: PolicyID MaryTest
boundedTimePolicyId = PolicyID $ hashScript boundedTimePolicy

tokenEx3 :: AssetName
tokenEx3 = AssetName $ BS.pack "tokenEx3"

------------------------------------
-- Mint Bounded Time Range Tokens --
------------------------------------

tokensEx3 :: Value MaryTest
tokensEx3 =
  Value 0 $
    Map.singleton boundedTimePolicyId (Map.singleton tokenEx3 1)

aliceCoinEx3 :: Coin
aliceCoinEx3 = aliceInitCoin <-> feeEx

-- Mint tokens
txbodyEx3 :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> TxBody MaryTest
txbodyEx3 s e =
  makeTxb
    [TxIn bootstrapTxId 0]
    [ TxOut Cast.aliceAddr (Val.inject aliceCoinEx3),
      TxOut Cast.aliceAddr tokensEx3
    ]
    (ValidityInterval s e)
    tokensEx3

txbodyEx3Valid :: TxBody MaryTest
txbodyEx3Valid = txbodyEx3 (SJust startInterval) (SJust stopInterval)

txEx3 :: TxBody MaryTest -> Tx MaryTest
txEx3 body =
  Tx
    body
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated body) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID boundedTimePolicyId, boundedTimePolicy)]
      }
    SNothing

txEx3Valid :: Tx MaryTest
txEx3Valid = txEx3 txbodyEx3Valid

txEx3InvalidLHSfixed :: Tx MaryTest
txEx3InvalidLHSfixed = txEx3 $ txbodyEx3 (SJust beforeStart) (SJust stopInterval)

txEx3InvalidLHSopen :: Tx MaryTest
txEx3InvalidLHSopen = txEx3 $ txbodyEx3 SNothing (SJust stopInterval)

txEx3InvalidRHSfixed :: Tx MaryTest
txEx3InvalidRHSfixed = txEx3 $ txbodyEx3 (SJust startInterval) (SJust afterStop)

txEx3InvalidRHSopen :: Tx MaryTest
txEx3InvalidRHSopen = txEx3 $ txbodyEx3 (SJust startInterval) SNothing

expectedUTxOEx3 :: UTxO MaryTest
expectedUTxOEx3 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyEx3Valid) 0, TxOut Cast.aliceAddr (Val.inject aliceCoinEx3)),
        (TxIn (txid txbodyEx3Valid) 1, TxOut Cast.aliceAddr tokensEx3),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------------------
-- Transfer Bounded Time Range Tokens --
----------------------------------------

aliceCoinEx4 :: Coin
aliceCoinEx4 = aliceCoinEx3 <-> feeEx

tokensEx4 :: Value MaryTest
tokensEx4 = Value 0 $ Map.singleton boundedTimePolicyId (Map.singleton tokenEx3 1)

-- Alice gives one token to Bob
txbodyEx4 :: TxBody MaryTest
txbodyEx4 =
  makeTxb
    [TxIn (txid txbodyEx3Valid) 0, TxIn (txid txbodyEx3Valid) 1]
    [ TxOut Cast.aliceAddr (Val.inject aliceCoinEx4),
      TxOut Cast.bobAddr tokensEx4
    ]
    unboundedInterval
    Val.zero

txEx4 :: Tx MaryTest
txEx4 =
  Tx
    txbodyEx4
    mempty {addrWits = makeWitnessesVKey (hashAnnotated txbodyEx4) [asWitness Cast.alicePay]}
    SNothing

expectedUTxOEx4 :: UTxO MaryTest
expectedUTxOEx4 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyEx4) 0, TxOut Cast.aliceAddr (Val.inject aliceCoinEx4)),
        (TxIn (txid txbodyEx4) 1, TxOut Cast.bobAddr tokensEx4),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

--------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens only Alice can mint --
--------------------------------------------------------------

alicePolicy :: Timelock MaryTest
alicePolicy = RequireSignature . asWitness . hashKey . vKey $ Cast.alicePay

alicePolicyId :: PolicyID MaryTest
alicePolicyId = PolicyID $ hashScript alicePolicy

tokenEx5 :: AssetName
tokenEx5 = AssetName $ BS.pack "alice"

-----------------------
-- Mint Alice Tokens --
-----------------------

tokensEx5 :: Value MaryTest
tokensEx5 =
  Value 0 $
    Map.singleton alicePolicyId (Map.singleton tokenEx5 17)

bobCoinEx5 :: Coin
bobCoinEx5 = bobInitCoin <-> feeEx

-- Bob pays the fees, but only alice can witness the minting
txbodyEx5 :: TxBody MaryTest
txbodyEx5 =
  makeTxb
    [TxIn bootstrapTxId 1]
    [ TxOut Cast.bobAddr (Val.inject bobCoinEx5),
      TxOut Cast.bobAddr tokensEx5
    ]
    unboundedInterval
    tokensEx5

txEx5Valid :: Tx MaryTest
txEx5Valid =
  Tx
    txbodyEx5
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodyEx5) [asWitness Cast.bobPay, asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID alicePolicyId, alicePolicy)]
      }
    SNothing

expectedUTxOEx5 :: UTxO MaryTest
expectedUTxOEx5 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyEx5) 0, TxOut Cast.bobAddr (Val.inject bobCoinEx5)),
        (TxIn (txid txbodyEx5) 1, TxOut Cast.bobAddr tokensEx5),
        (TxIn bootstrapTxId 0, TxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      ]

txEx5Invalid :: Tx MaryTest
txEx5Invalid =
  Tx
    txbodyEx5
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodyEx5) [asWitness Cast.bobPay],
        scriptWits = Map.fromList [(policyID alicePolicyId, alicePolicy)]
      }
    SNothing

--
-- Multi-Assets Test Group
--

multiAssetsExample :: TestTree
multiAssetsExample =
  testGroup
    "multi-assets"
    [ testGroup
        "simple"
        [ testCase "minting" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx1
              (ledgerEnv $ SlotNo 0)
              (Right expectedUTxOEx1),
          testCase "transfer" $
            testMaryNoDelegLEDGER
              expectedUTxOEx1
              txEx2
              (ledgerEnv $ SlotNo 1)
              (Right expectedUTxOEx2)
        ],
      testGroup
        "bounded time interval"
        [ testCase "minting, valid" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx3Valid
              (ledgerEnv startInterval)
              (Right expectedUTxOEx3),
          testCase "minting, invalid LHS too small" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx3InvalidLHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid LHS unspecified" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx3InvalidLHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid RHS too big" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx3InvalidRHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid RHS unspecified" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx3InvalidRHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "transfer, after minting period" $
            testMaryNoDelegLEDGER
              expectedUTxOEx3
              txEx4
              (ledgerEnv afterStop)
              (Right expectedUTxOEx4)
        ],
      testGroup
        "single key"
        [ testCase "minting, valid" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx5Valid
              (ledgerEnv $ SlotNo 0)
              (Right expectedUTxOEx5),
          testCase "minting, invalid no forge signature" $
            testMaryNoDelegLEDGER
              initUTxO
              txEx5Invalid
              (ledgerEnv $ SlotNo 0)
              (policyFailure alicePolicyId)
        ]
    ]
