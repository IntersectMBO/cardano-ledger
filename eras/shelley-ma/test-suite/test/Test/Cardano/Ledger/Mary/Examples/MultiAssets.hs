{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Mary.Examples.MultiAssets
-- Description : Multi-Assets Examples
--
-- Examples demonstrating the use of multi-assets.
module Test.Cardano.Ledger.Mary.Examples.MultiAssets
  ( multiAssetsExample,
  )
where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Keys (KeyPair (..), asWitness, hashKey)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (LEDGER, LedgerEnv (..))
import Cardano.Ledger.Shelley.LedgerState (AccountState (..))
import Cardano.Ledger.Shelley.PParams (PParams, PParams' (..), emptyPParams)
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (..))
import Cardano.Ledger.Shelley.Tx
  ( Tx (..),
    WitnessSetHKD (..),
    hashScript,
  )
import Cardano.Ledger.Shelley.TxBody (TxOut (..), Wdrl (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessesVKey)
import Cardano.Ledger.ShelleyMA.Rules.Utxo (UtxoPredicateFailure (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (TxBody (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..), mkTxInPartial, txid)
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Control.Exception (ErrorCall (ErrorCall), evaluate, try)
import Control.State.Transition.Extended (PredicateFailure)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.Mary.Examples (testMaryNoDelegLEDGER)
import qualified Test.Cardano.Ledger.Mary.Examples.Cast as Cast
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

type MaryTest = MaryEra TestCrypto

------------------------------
-- Set Up the Initial State --
------------------------------

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

unboundedInterval :: ValidityInterval
unboundedInterval = ValidityInterval SNothing SNothing

bootstrapTxId :: TxId TestCrypto
bootstrapTxId = txid txb
  where
    txb :: TxBody MaryTest
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
    SplitMap.fromList
      [ (mkTxInPartial bootstrapTxId 0, TxOut Cast.aliceAddr (Val.inject aliceInitCoin)),
        (mkTxInPartial bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

pp :: PParams MaryTest
pp =
  emptyPParams
    { _minfeeA = 0,
      _minfeeB = 1,
      _maxTxSize = 16384,
      _minUTxOValue = Coin 100
    }

ledgerEnv :: SlotNo -> LedgerEnv MaryTest
ledgerEnv s = LedgerEnv s minBound pp (AccountState (Coin 0) (Coin 0))

feeEx :: Coin
feeEx = Coin 3

-- These examples do not use several of the transaction components,
-- so we can simplify building them.
makeTxb ::
  [TxIn TestCrypto] ->
  [TxOut MaryTest] ->
  ValidityInterval ->
  Value TestCrypto ->
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

policyFailure :: PolicyID TestCrypto -> Either [PredicateFailure (LEDGER MaryTest)] (UTxO MaryTest)
policyFailure p =
  Left
    [ UtxowFailure
        (ScriptWitnessNotValidatingUTXOW (Set.singleton (policyID p)))
    ]

outTooBigFailure :: TxOut MaryTest -> Either [PredicateFailure (LEDGER MaryTest)] (UTxO MaryTest)
outTooBigFailure out = Left [UtxowFailure (UtxoFailure (OutputTooBigUTxO [out]))]

----------------------------------------------------
-- Introduce a new Token Bundle, Purple Tokens
--
-- Variables ending with SimpleExN (for a numeral N)
-- refer to this example.
----------------------------------------------------

-- This is the most lax policy possible, requiring no authorization at all.
purplePolicy :: Timelock TestCrypto
purplePolicy = RequireAllOf (StrictSeq.fromList [])

purplePolicyId :: PolicyID TestCrypto
purplePolicyId = PolicyID $ hashScript @MaryTest purplePolicy

plum :: AssetName
plum = AssetName $ BS.pack "plum"

amethyst :: AssetName
amethyst = AssetName $ BS.pack "amethyst"

------------------------
-- Mint Purple Tokens --
------------------------

mintSimpleEx1 :: Value TestCrypto
mintSimpleEx1 =
  Value 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

aliceCoinSimpleEx1 :: Coin
aliceCoinSimpleEx1 = aliceInitCoin <-> feeEx

tokensSimpleEx1 :: Value TestCrypto
tokensSimpleEx1 = mintSimpleEx1 <+> Val.inject aliceCoinSimpleEx1

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodySimpleEx1 :: TxBody MaryTest
txbodySimpleEx1 =
  makeTxb
    [mkTxInPartial bootstrapTxId 0]
    [TxOut Cast.aliceAddr tokensSimpleEx1]
    unboundedInterval
    mintSimpleEx1

txSimpleEx1 :: Tx MaryTest
txSimpleEx1 =
  Tx
    txbodySimpleEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodySimpleEx1) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
      }
    SNothing

expectedUTxOSimpleEx1 :: UTxO MaryTest
expectedUTxOSimpleEx1 =
  UTxO $
    SplitMap.fromList
      [ (mkTxInPartial (txid txbodySimpleEx1) 0, TxOut Cast.aliceAddr tokensSimpleEx1),
        (mkTxInPartial bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------
-- Transfer Purple Tokens --
----------------------------

minUtxoSimpleEx2 :: Coin
minUtxoSimpleEx2 = Coin 117

aliceCoinsSimpleEx2 :: Coin
aliceCoinsSimpleEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> minUtxoSimpleEx2)

aliceTokensSimpleEx2 :: Value TestCrypto
aliceTokensSimpleEx2 =
  Value (unCoin aliceCoinsSimpleEx2) $
    Map.singleton purplePolicyId (Map.fromList [(plum, 8), (amethyst, 2)])

bobTokensSimpleEx2 :: Value TestCrypto
bobTokensSimpleEx2 =
  Value (unCoin minUtxoSimpleEx2) $
    Map.singleton purplePolicyId (Map.singleton plum 5)

-- Alice gives five plums to Bob.
txbodySimpleEx2 :: TxBody MaryTest
txbodySimpleEx2 =
  makeTxb
    [mkTxInPartial (txid txbodySimpleEx1) 0]
    [ TxOut Cast.aliceAddr aliceTokensSimpleEx2,
      TxOut Cast.bobAddr bobTokensSimpleEx2
    ]
    unboundedInterval
    Val.zero

txSimpleEx2 :: Tx MaryTest
txSimpleEx2 =
  Tx
    txbodySimpleEx2
    mempty {addrWits = makeWitnessesVKey (hashAnnotated txbodySimpleEx2) [asWitness Cast.alicePay]}
    SNothing

expectedUTxOSimpleEx2 :: UTxO MaryTest
expectedUTxOSimpleEx2 =
  UTxO $
    SplitMap.fromList
      [ (mkTxInPartial (txid txbodySimpleEx2) 0, TxOut Cast.aliceAddr aliceTokensSimpleEx2),
        (mkTxInPartial (txid txbodySimpleEx2) 1, TxOut Cast.bobAddr bobTokensSimpleEx2),
        (mkTxInPartial bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens With a Time Range
--
-- Variables ending with TimeExN (for a numeral N)
-- refer to this example.
------------------------------------------------------------

beforeStart :: SlotNo
beforeStart = SlotNo 12

startInterval :: SlotNo
startInterval = SlotNo 13

stopInterval :: SlotNo
stopInterval = SlotNo 19

afterStop :: SlotNo
afterStop = SlotNo 20

boundedTimePolicy :: Timelock TestCrypto
boundedTimePolicy =
  RequireAllOf
    ( StrictSeq.fromList
        [ RequireTimeStart startInterval,
          RequireTimeExpire stopInterval
        ]
    )

boundedTimePolicyId :: PolicyID TestCrypto
boundedTimePolicyId = PolicyID $ hashScript @MaryTest boundedTimePolicy

tokenTimeEx :: AssetName
tokenTimeEx = AssetName $ BS.pack "tokenTimeEx"

------------------------------------
-- Mint Bounded Time Range Tokens --
------------------------------------

mintTimeEx1 :: Value TestCrypto
mintTimeEx1 =
  Value 0 $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx1 :: Coin
aliceCoinsTimeEx1 = aliceInitCoin <-> feeEx

tokensTimeEx1 :: Value TestCrypto
tokensTimeEx1 = mintTimeEx1 <+> Val.inject aliceCoinsTimeEx1

-- Mint tokens
txbodyTimeEx1 :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> TxBody MaryTest
txbodyTimeEx1 s e =
  makeTxb
    [mkTxInPartial bootstrapTxId 0]
    [TxOut Cast.aliceAddr tokensTimeEx1]
    (ValidityInterval s e)
    mintTimeEx1

txbodyTimeEx1Valid :: TxBody MaryTest
txbodyTimeEx1Valid = txbodyTimeEx1 (SJust startInterval) (SJust stopInterval)

txTimeEx1 :: TxBody MaryTest -> Tx MaryTest
txTimeEx1 txbody =
  Tx
    txbody
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbody) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID boundedTimePolicyId, boundedTimePolicy)]
      }
    SNothing

txTimeEx1Valid :: Tx MaryTest
txTimeEx1Valid = txTimeEx1 txbodyTimeEx1Valid

txTimeEx1InvalidLHSfixed :: Tx MaryTest
txTimeEx1InvalidLHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust beforeStart) (SJust stopInterval)

txTimeEx1InvalidLHSopen :: Tx MaryTest
txTimeEx1InvalidLHSopen = txTimeEx1 $ txbodyTimeEx1 SNothing (SJust stopInterval)

txTimeEx1InvalidRHSfixed :: Tx MaryTest
txTimeEx1InvalidRHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) (SJust afterStop)

txTimeEx1InvalidRHSopen :: Tx MaryTest
txTimeEx1InvalidRHSopen = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) SNothing

expectedUTxOTimeEx1 :: UTxO MaryTest
expectedUTxOTimeEx1 =
  UTxO $
    SplitMap.fromList
      [ (mkTxInPartial (txid txbodyTimeEx1Valid) 0, TxOut Cast.aliceAddr tokensTimeEx1),
        (mkTxInPartial bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------------------
-- Transfer Bounded Time Range Tokens --
----------------------------------------

mintTimeEx2 :: Coin
mintTimeEx2 = Coin 120

bobTokensTimeEx2 :: Value TestCrypto
bobTokensTimeEx2 =
  Value (unCoin mintTimeEx2) $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx2 :: Coin
aliceCoinsTimeEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> mintTimeEx2)

-- Alice gives one token to Bob
txbodyTimeEx2 :: TxBody MaryTest
txbodyTimeEx2 =
  makeTxb
    [mkTxInPartial (txid txbodyTimeEx1Valid) 0]
    [ TxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2),
      TxOut Cast.bobAddr bobTokensTimeEx2
    ]
    unboundedInterval
    Val.zero

txTimeEx2 :: Tx MaryTest
txTimeEx2 =
  Tx
    txbodyTimeEx2
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated txbodyTimeEx2) [asWitness Cast.alicePay]
      }
    SNothing

expectedUTxOTimeEx2 :: UTxO MaryTest
expectedUTxOTimeEx2 =
  UTxO $
    SplitMap.fromList
      [ (mkTxInPartial (txid txbodyTimeEx2) 0, TxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2)),
        (mkTxInPartial (txid txbodyTimeEx2) 1, TxOut Cast.bobAddr bobTokensTimeEx2),
        (mkTxInPartial bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

--------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens only Alice can mint
--
-- Variables ending with SingExN (for a numeral N)
-- refer to this example.
--------------------------------------------------------------

alicePolicy :: Timelock TestCrypto
alicePolicy = RequireSignature . asWitness . hashKey . vKey $ Cast.alicePay

alicePolicyId :: PolicyID TestCrypto
alicePolicyId = PolicyID $ hashScript @MaryTest alicePolicy

tokenSingWitEx1 :: AssetName
tokenSingWitEx1 = AssetName $ BS.pack "tokenSingWitEx1"

-----------------------
-- Mint Alice Tokens --
-----------------------

mintSingWitEx1 :: Value TestCrypto
mintSingWitEx1 =
  Value 0 $
    Map.singleton alicePolicyId (Map.singleton tokenSingWitEx1 17)

bobCoinsSingWitEx1 :: Coin
bobCoinsSingWitEx1 = bobInitCoin <-> feeEx

tokensSingWitEx1 :: Value TestCrypto
tokensSingWitEx1 = mintSingWitEx1 <+> Val.inject bobCoinsSingWitEx1

-- Bob pays the fees, but only alice can witness the minting
txbodySingWitEx1 :: TxBody MaryTest
txbodySingWitEx1 =
  makeTxb
    [mkTxInPartial bootstrapTxId 1]
    [TxOut Cast.bobAddr tokensSingWitEx1]
    unboundedInterval
    mintSingWitEx1

txSingWitEx1Valid :: Tx MaryTest
txSingWitEx1Valid =
  Tx
    txbodySingWitEx1
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated txbodySingWitEx1) [asWitness Cast.bobPay, asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID alicePolicyId, alicePolicy)]
      }
    SNothing

expectedUTxOSingWitEx1 :: UTxO MaryTest
expectedUTxOSingWitEx1 =
  UTxO $
    SplitMap.fromList
      [ (mkTxInPartial (txid txbodySingWitEx1) 0, TxOut Cast.bobAddr tokensSingWitEx1),
        (mkTxInPartial bootstrapTxId 0, TxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      ]

txSingWitEx1Invalid :: Tx MaryTest
txSingWitEx1Invalid =
  Tx
    txbodySingWitEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodySingWitEx1) [asWitness Cast.bobPay],
        scriptWits = Map.fromList [(policyID alicePolicyId, alicePolicy)]
      }
    SNothing

------------------------
-- Mint Negative Values
--
-- Variables ending with NegExN (for a numeral N)
-- refer to this example. We assume that the simple
-- tokens in the SimpleEx1 example have been minted
-- and we use expectedUTxOSimpleEx1 as our starting
-- state.
------------------------

-- Mint negative valued tokens
mintNegEx1 :: Value TestCrypto
mintNegEx1 =
  Value 0 $
    Map.singleton purplePolicyId (Map.singleton plum (-8))

aliceTokensNegEx1 :: Value TestCrypto
aliceTokensNegEx1 =
  Value (unCoin $ aliceCoinsSimpleEx2 <-> feeEx) $
    Map.singleton purplePolicyId (Map.singleton amethyst 2)

txbodyNegEx1 :: TxBody MaryTest
txbodyNegEx1 =
  makeTxb
    [mkTxInPartial (txid txbodySimpleEx2) 0]
    [TxOut Cast.aliceAddr aliceTokensNegEx1]
    unboundedInterval
    mintNegEx1

txNegEx1 :: Tx MaryTest
txNegEx1 =
  Tx
    txbodyNegEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodyNegEx1) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
      }
    SNothing

initialUTxONegEx1 :: UTxO MaryTest
initialUTxONegEx1 = expectedUTxOSimpleEx2

expectedUTxONegEx1 :: UTxO MaryTest
expectedUTxONegEx1 =
  UTxO $
    SplitMap.fromList
      [ (mkTxInPartial (txid txbodyNegEx1) 0, TxOut Cast.aliceAddr aliceTokensNegEx1),
        (mkTxInPartial bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin)),
        (mkTxInPartial (txid txbodySimpleEx2) 1, TxOut Cast.bobAddr bobTokensSimpleEx2)
      ]

--
-- Now attempt to produce negative outputs
--

mintNegEx2 :: Value TestCrypto
mintNegEx2 =
  Value 0 $
    Map.singleton purplePolicyId (Map.singleton plum (-9))

aliceTokensNegEx2 :: Value TestCrypto
aliceTokensNegEx2 =
  Value (unCoin $ aliceCoinsSimpleEx2 <-> feeEx) $
    Map.singleton purplePolicyId (Map.fromList [(plum, -1), (amethyst, 2)])

-- Mint negative valued tokens
txbodyNegEx2 :: TxBody MaryTest
txbodyNegEx2 =
  makeTxb
    [mkTxInPartial (txid txbodySimpleEx2) 0]
    [TxOut Cast.aliceAddr aliceTokensNegEx2]
    unboundedInterval
    mintNegEx2

testNegEx2 :: Assertion
testNegEx2 = do
  r <- try (evaluate $ txbodyNegEx2 == txbodyNegEx2)
  case r of
    Left (ErrorCall _) -> pure ()
    Right _ -> assertFailure "constructed negative TxOut Value"

--
-- Create a Value that is too big
--

minUtxoBigEx :: Coin
minUtxoBigEx = Coin 50000

smallValue :: Value TestCrypto
smallValue =
  Value 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

smallOut :: TxOut MaryTest
smallOut =
  TxOut Cast.aliceAddr $ smallValue <+> Val.inject (aliceInitCoin <-> (feeEx <+> minUtxoBigEx))

numAssets :: Int
numAssets = 1000

bigValue :: Value TestCrypto
bigValue =
  Value 0 $
    Map.singleton
      purplePolicyId
      (Map.fromList $ map (\x -> (AssetName . BS.pack $ show x, 1)) [1 .. numAssets])

bigOut :: TxOut MaryTest
bigOut = TxOut Cast.aliceAddr $ bigValue <+> Val.inject minUtxoBigEx

txbodyWithBigValue :: TxBody MaryTest
txbodyWithBigValue =
  makeTxb
    [mkTxInPartial bootstrapTxId 0]
    [smallOut, bigOut]
    unboundedInterval
    (bigValue <+> smallValue)

txBigValue :: Tx MaryTest
txBigValue =
  Tx
    txbodyWithBigValue
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodyWithBigValue) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
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
              txSimpleEx1
              (ledgerEnv $ SlotNo 0)
              (Right expectedUTxOSimpleEx1),
          testCase "transfer" $
            testMaryNoDelegLEDGER
              expectedUTxOSimpleEx1
              txSimpleEx2
              (ledgerEnv $ SlotNo 1)
              (Right expectedUTxOSimpleEx2)
        ],
      testGroup
        "bounded time interval"
        [ testCase "minting, valid" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1Valid
              (ledgerEnv startInterval)
              (Right expectedUTxOTimeEx1),
          testCase "minting, invalid LHS too small" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidLHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid LHS unspecified" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidLHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid RHS too big" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidRHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid RHS unspecified" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidRHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "transfer, after minting period" $
            testMaryNoDelegLEDGER
              expectedUTxOTimeEx1
              txTimeEx2
              (ledgerEnv afterStop)
              (Right expectedUTxOTimeEx2)
        ],
      testGroup
        "single key"
        [ testCase "minting, valid" $
            testMaryNoDelegLEDGER
              initUTxO
              txSingWitEx1Valid
              (ledgerEnv $ SlotNo 0)
              (Right expectedUTxOSingWitEx1),
          testCase "minting, invalid no mint signature" $
            testMaryNoDelegLEDGER
              initUTxO
              txSingWitEx1Invalid
              (ledgerEnv $ SlotNo 0)
              (policyFailure alicePolicyId)
        ],
      testGroup
        "negative minting"
        [ testCase "remove assets" $
            testMaryNoDelegLEDGER
              initialUTxONegEx1
              txNegEx1
              (ledgerEnv $ SlotNo 3)
              (Right expectedUTxONegEx1),
          testCase "no negative outputs" testNegEx2
        ],
      testCase "value too big" $
        testMaryNoDelegLEDGER
          initUTxO
          txBigValue
          (ledgerEnv $ SlotNo 0)
          (outTooBigFailure bigOut)
    ]
