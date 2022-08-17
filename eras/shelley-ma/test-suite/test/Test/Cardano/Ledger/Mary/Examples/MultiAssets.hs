{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Keys (KeyPair (..), asWitness, hashKey)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    MaryValue (..),
    PolicyID (..),
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (LedgerEnv (..), ShelleyLEDGER)
import Cardano.Ledger.Shelley.LedgerState (AccountState (..))
import Cardano.Ledger.Shelley.PParams (ShelleyPParams, ShelleyPParamsHKD (..), emptyPParams)
import Cardano.Ledger.Shelley.Rules.Ledger (ShelleyLedgerPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), WitnessSetHKD (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxOut (..), Wdrl (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessesVKey)
import Cardano.Ledger.ShelleyMA.Rules (ShelleyMAUtxoPredFailure (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..), mkTxInPartial)
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Control.Exception (ErrorCall (ErrorCall), evaluate, try)
import Control.State.Transition.Extended (PredicateFailure)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Exts (fromString)
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
    txb :: MATxBody MaryTest
    txb =
      MATxBody
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
      [ (mkTxInPartial bootstrapTxId 0, ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin)),
        (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

pp :: ShelleyPParams MaryTest
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
  [ShelleyTxOut MaryTest] ->
  ValidityInterval ->
  MaryValue TestCrypto ->
  MATxBody MaryTest
makeTxb ins outs interval minted =
  MATxBody
    (Set.fromList ins)
    (StrictSeq.fromList outs)
    StrictSeq.empty
    (Wdrl Map.empty)
    feeEx
    interval
    SNothing
    SNothing
    minted

policyFailure :: PolicyID TestCrypto -> Either [PredicateFailure (ShelleyLEDGER MaryTest)] (UTxO MaryTest)
policyFailure p =
  Left
    [ UtxowFailure
        (ScriptWitnessNotValidatingUTXOW (Set.singleton (policyID p)))
    ]

outTooBigFailure :: ShelleyTxOut MaryTest -> Either [PredicateFailure (ShelleyLEDGER MaryTest)] (UTxO MaryTest)
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
plum = AssetName "plum"

amethyst :: AssetName
amethyst = AssetName "amethyst"

------------------------
-- Mint Purple Tokens --
------------------------

mintSimpleEx1 :: MaryValue TestCrypto
mintSimpleEx1 =
  MaryValue 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

aliceCoinSimpleEx1 :: Coin
aliceCoinSimpleEx1 = aliceInitCoin <-> feeEx

tokensSimpleEx1 :: MaryValue TestCrypto
tokensSimpleEx1 = mintSimpleEx1 <+> Val.inject aliceCoinSimpleEx1

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodySimpleEx1 :: MATxBody MaryTest
txbodySimpleEx1 =
  makeTxb
    [mkTxInPartial bootstrapTxId 0]
    [ShelleyTxOut Cast.aliceAddr tokensSimpleEx1]
    unboundedInterval
    mintSimpleEx1

txSimpleEx1 :: ShelleyTx MaryTest
txSimpleEx1 =
  ShelleyTx
    txbodySimpleEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodySimpleEx1) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
      }
    SNothing

expectedUTxOSimpleEx1 :: UTxO MaryTest
expectedUTxOSimpleEx1 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txid txbodySimpleEx1) 0, ShelleyTxOut Cast.aliceAddr tokensSimpleEx1),
        (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------
-- Transfer Purple Tokens --
----------------------------

minUtxoSimpleEx2 :: Coin
minUtxoSimpleEx2 = Coin 117

aliceCoinsSimpleEx2 :: Coin
aliceCoinsSimpleEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> minUtxoSimpleEx2)

aliceTokensSimpleEx2 :: MaryValue TestCrypto
aliceTokensSimpleEx2 =
  MaryValue (unCoin aliceCoinsSimpleEx2) $
    Map.singleton purplePolicyId (Map.fromList [(plum, 8), (amethyst, 2)])

bobTokensSimpleEx2 :: MaryValue TestCrypto
bobTokensSimpleEx2 =
  MaryValue (unCoin minUtxoSimpleEx2) $
    Map.singleton purplePolicyId (Map.singleton plum 5)

-- Alice gives five plums to Bob.
txbodySimpleEx2 :: MATxBody MaryTest
txbodySimpleEx2 =
  makeTxb
    [mkTxInPartial (txid txbodySimpleEx1) 0]
    [ ShelleyTxOut Cast.aliceAddr aliceTokensSimpleEx2,
      ShelleyTxOut Cast.bobAddr bobTokensSimpleEx2
    ]
    unboundedInterval
    Val.zero

txSimpleEx2 :: ShelleyTx MaryTest
txSimpleEx2 =
  ShelleyTx
    txbodySimpleEx2
    mempty {addrWits = makeWitnessesVKey (hashAnnotated txbodySimpleEx2) [asWitness Cast.alicePay]}
    SNothing

expectedUTxOSimpleEx2 :: UTxO MaryTest
expectedUTxOSimpleEx2 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txid txbodySimpleEx2) 0, ShelleyTxOut Cast.aliceAddr aliceTokensSimpleEx2),
        (mkTxInPartial (txid txbodySimpleEx2) 1, ShelleyTxOut Cast.bobAddr bobTokensSimpleEx2),
        (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
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
tokenTimeEx = AssetName "tokenTimeEx"

------------------------------------
-- Mint Bounded Time Range Tokens --
------------------------------------

mintTimeEx1 :: MaryValue TestCrypto
mintTimeEx1 =
  MaryValue 0 $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx1 :: Coin
aliceCoinsTimeEx1 = aliceInitCoin <-> feeEx

tokensTimeEx1 :: MaryValue TestCrypto
tokensTimeEx1 = mintTimeEx1 <+> Val.inject aliceCoinsTimeEx1

-- Mint tokens
txbodyTimeEx1 :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> MATxBody MaryTest
txbodyTimeEx1 s e =
  makeTxb
    [mkTxInPartial bootstrapTxId 0]
    [ShelleyTxOut Cast.aliceAddr tokensTimeEx1]
    (ValidityInterval s e)
    mintTimeEx1

txbodyTimeEx1Valid :: MATxBody MaryTest
txbodyTimeEx1Valid = txbodyTimeEx1 (SJust startInterval) (SJust stopInterval)

txTimeEx1 :: MATxBody MaryTest -> ShelleyTx MaryTest
txTimeEx1 txbody =
  ShelleyTx
    txbody
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbody) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID boundedTimePolicyId, boundedTimePolicy)]
      }
    SNothing

txTimeEx1Valid :: ShelleyTx MaryTest
txTimeEx1Valid = txTimeEx1 txbodyTimeEx1Valid

txTimeEx1InvalidLHSfixed :: ShelleyTx MaryTest
txTimeEx1InvalidLHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust beforeStart) (SJust stopInterval)

txTimeEx1InvalidLHSopen :: ShelleyTx MaryTest
txTimeEx1InvalidLHSopen = txTimeEx1 $ txbodyTimeEx1 SNothing (SJust stopInterval)

txTimeEx1InvalidRHSfixed :: ShelleyTx MaryTest
txTimeEx1InvalidRHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) (SJust afterStop)

txTimeEx1InvalidRHSopen :: ShelleyTx MaryTest
txTimeEx1InvalidRHSopen = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) SNothing

expectedUTxOTimeEx1 :: UTxO MaryTest
expectedUTxOTimeEx1 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txid txbodyTimeEx1Valid) 0, ShelleyTxOut Cast.aliceAddr tokensTimeEx1),
        (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------------------
-- Transfer Bounded Time Range Tokens --
----------------------------------------

mintTimeEx2 :: Coin
mintTimeEx2 = Coin 120

bobTokensTimeEx2 :: MaryValue TestCrypto
bobTokensTimeEx2 =
  MaryValue (unCoin mintTimeEx2) $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx2 :: Coin
aliceCoinsTimeEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> mintTimeEx2)

-- Alice gives one token to Bob
txbodyTimeEx2 :: MATxBody MaryTest
txbodyTimeEx2 =
  makeTxb
    [mkTxInPartial (txid txbodyTimeEx1Valid) 0]
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2),
      ShelleyTxOut Cast.bobAddr bobTokensTimeEx2
    ]
    unboundedInterval
    Val.zero

txTimeEx2 :: ShelleyTx MaryTest
txTimeEx2 =
  ShelleyTx
    txbodyTimeEx2
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated txbodyTimeEx2) [asWitness Cast.alicePay]
      }
    SNothing

expectedUTxOTimeEx2 :: UTxO MaryTest
expectedUTxOTimeEx2 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txid txbodyTimeEx2) 0, ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2)),
        (mkTxInPartial (txid txbodyTimeEx2) 1, ShelleyTxOut Cast.bobAddr bobTokensTimeEx2),
        (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
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
tokenSingWitEx1 = AssetName "tokenSingWitEx1"

-----------------------
-- Mint Alice Tokens --
-----------------------

mintSingWitEx1 :: MaryValue TestCrypto
mintSingWitEx1 =
  MaryValue 0 $
    Map.singleton alicePolicyId (Map.singleton tokenSingWitEx1 17)

bobCoinsSingWitEx1 :: Coin
bobCoinsSingWitEx1 = bobInitCoin <-> feeEx

tokensSingWitEx1 :: MaryValue TestCrypto
tokensSingWitEx1 = mintSingWitEx1 <+> Val.inject bobCoinsSingWitEx1

-- Bob pays the fees, but only alice can witness the minting
txbodySingWitEx1 :: MATxBody MaryTest
txbodySingWitEx1 =
  makeTxb
    [mkTxInPartial bootstrapTxId 1]
    [ShelleyTxOut Cast.bobAddr tokensSingWitEx1]
    unboundedInterval
    mintSingWitEx1

txSingWitEx1Valid :: ShelleyTx MaryTest
txSingWitEx1Valid =
  ShelleyTx
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
    Map.fromList
      [ (mkTxInPartial (txid txbodySingWitEx1) 0, ShelleyTxOut Cast.bobAddr tokensSingWitEx1),
        (mkTxInPartial bootstrapTxId 0, ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      ]

txSingWitEx1Invalid :: ShelleyTx MaryTest
txSingWitEx1Invalid =
  ShelleyTx
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
mintNegEx1 :: MaryValue TestCrypto
mintNegEx1 =
  MaryValue 0 $
    Map.singleton purplePolicyId (Map.singleton plum (-8))

aliceTokensNegEx1 :: MaryValue TestCrypto
aliceTokensNegEx1 =
  MaryValue (unCoin $ aliceCoinsSimpleEx2 <-> feeEx) $
    Map.singleton purplePolicyId (Map.singleton amethyst 2)

txbodyNegEx1 :: MATxBody MaryTest
txbodyNegEx1 =
  makeTxb
    [mkTxInPartial (txid txbodySimpleEx2) 0]
    [ShelleyTxOut Cast.aliceAddr aliceTokensNegEx1]
    unboundedInterval
    mintNegEx1

txNegEx1 :: ShelleyTx MaryTest
txNegEx1 =
  ShelleyTx
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
    Map.fromList
      [ (mkTxInPartial (txid txbodyNegEx1) 0, ShelleyTxOut Cast.aliceAddr aliceTokensNegEx1),
        (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin)),
        (mkTxInPartial (txid txbodySimpleEx2) 1, ShelleyTxOut Cast.bobAddr bobTokensSimpleEx2)
      ]

--
-- Now attempt to produce negative outputs
--

mintNegEx2 :: MaryValue TestCrypto
mintNegEx2 =
  MaryValue 0 $
    Map.singleton purplePolicyId (Map.singleton plum (-9))

aliceTokensNegEx2 :: MaryValue TestCrypto
aliceTokensNegEx2 =
  MaryValue (unCoin $ aliceCoinsSimpleEx2 <-> feeEx) $
    Map.singleton purplePolicyId (Map.fromList [(plum, -1), (amethyst, 2)])

-- Mint negative valued tokens
txbodyNegEx2 :: MATxBody MaryTest
txbodyNegEx2 =
  makeTxb
    [mkTxInPartial (txid txbodySimpleEx2) 0]
    [ShelleyTxOut Cast.aliceAddr aliceTokensNegEx2]
    unboundedInterval
    mintNegEx2

testNegEx2 :: Assertion
testNegEx2 = do
  r <- try (evaluate $ txbodyNegEx2 == txbodyNegEx2)
  case r of
    Left (ErrorCall _) -> pure ()
    Right _ -> assertFailure "constructed negative ShelleyTxOut Value"

--
-- Create a Value that is too big
--

minUtxoBigEx :: Coin
minUtxoBigEx = Coin 50000

smallValue :: MaryValue TestCrypto
smallValue =
  MaryValue 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

smallOut :: ShelleyTxOut MaryTest
smallOut =
  ShelleyTxOut Cast.aliceAddr $ smallValue <+> Val.inject (aliceInitCoin <-> (feeEx <+> minUtxoBigEx))

numAssets :: Int
numAssets = 1000

bigValue :: MaryValue TestCrypto
bigValue =
  MaryValue 0 $
    Map.singleton
      purplePolicyId
      (Map.fromList $ map (\x -> (AssetName . fromString $ show x, 1)) [1 .. numAssets])

bigOut :: ShelleyTxOut MaryTest
bigOut = ShelleyTxOut Cast.aliceAddr $ bigValue <+> Val.inject minUtxoBigEx

txbodyWithBigValue :: MATxBody MaryTest
txbodyWithBigValue =
  makeTxb
    [mkTxInPartial bootstrapTxId 0]
    [smallOut, bigOut]
    unboundedInterval
    (bigValue <+> smallValue)

txBigValue :: ShelleyTx MaryTest
txBigValue =
  ShelleyTx
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
