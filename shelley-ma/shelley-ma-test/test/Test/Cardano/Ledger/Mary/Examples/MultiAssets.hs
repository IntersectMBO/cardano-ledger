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
import Cardano.Ledger.Val ((<+>), (<->))
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

----------------------------------------------------
-- Introduce a new Token Bundle, Purple Tokens
--
-- Variables ending with SimpleExN (for a numeral N)
-- refer to this example.
----------------------------------------------------

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

mintSimpleEx1 :: Value MaryTest
mintSimpleEx1 =
  Value 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

aliceCoinSimpleEx1 :: Coin
aliceCoinSimpleEx1 = aliceInitCoin <-> feeEx

tokensSimpleEx1 :: Value MaryTest
tokensSimpleEx1 = mintSimpleEx1 <+> (Val.inject aliceCoinSimpleEx1)

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodySimpleEx1 :: TxBody MaryTest
txbodySimpleEx1 =
  makeTxb
    [TxIn bootstrapTxId 0]
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
    Map.fromList
      [ (TxIn (txid txbodySimpleEx1) 0, TxOut Cast.aliceAddr tokensSimpleEx1),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------
-- Transfer Purple Tokens --
----------------------------

minUtxoSimpleEx2 :: Coin
minUtxoSimpleEx2 = Coin 100

aliceCoinsSimpleEx2 :: Coin
aliceCoinsSimpleEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> minUtxoSimpleEx2)

aliceTokensSimpleEx2 :: Value MaryTest
aliceTokensSimpleEx2 =
  Value (unCoin aliceCoinsSimpleEx2) $
    Map.singleton purplePolicyId (Map.fromList [(plum, 8), (amethyst, 2)])

bobTokensSimpleEx2 :: Value MaryTest
bobTokensSimpleEx2 =
  Value (unCoin minUtxoSimpleEx2) $
    Map.singleton purplePolicyId (Map.singleton plum 5)

-- Alice gives five plums to Bob.
txbodySimpleEx2 :: TxBody MaryTest
txbodySimpleEx2 =
  makeTxb
    [TxIn (txid txbodySimpleEx1) 0]
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
    Map.fromList
      [ (TxIn (txid txbodySimpleEx2) 0, TxOut Cast.aliceAddr aliceTokensSimpleEx2),
        (TxIn (txid txbodySimpleEx2) 1, TxOut Cast.bobAddr bobTokensSimpleEx2),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
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

tokenTimeEx :: AssetName
tokenTimeEx = AssetName $ BS.pack "tokenTimeEx"

------------------------------------
-- Mint Bounded Time Range Tokens --
------------------------------------

mintTimeEx1 :: Value MaryTest
mintTimeEx1 =
  Value 0 $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx1 :: Coin
aliceCoinsTimeEx1 = aliceInitCoin <-> feeEx

tokensTimeEx1 :: Value MaryTest
tokensTimeEx1 = mintTimeEx1 <+> (Val.inject aliceCoinsTimeEx1)

-- Mint tokens
txbodyTimeEx1 :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> TxBody MaryTest
txbodyTimeEx1 s e =
  makeTxb
    [TxIn bootstrapTxId 0]
    [TxOut Cast.aliceAddr tokensTimeEx1]
    (ValidityInterval s e)
    mintTimeEx1

txbodyTimeEx1Valid :: TxBody MaryTest
txbodyTimeEx1Valid = txbodyTimeEx1 (SJust startInterval) (SJust stopInterval)

txTimeEx1 :: TxBody MaryTest -> Tx MaryTest
txTimeEx1 body =
  Tx
    body
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated body) [asWitness Cast.alicePay],
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
    Map.fromList
      [ (TxIn (txid txbodyTimeEx1Valid) 0, TxOut Cast.aliceAddr tokensTimeEx1),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------------------
-- Transfer Bounded Time Range Tokens --
----------------------------------------

mintTimeEx2 :: Coin
mintTimeEx2 = Coin 100

bobTokensTimeEx2 :: Value MaryTest
bobTokensTimeEx2 =
  Value (unCoin mintTimeEx2) $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx2 :: Coin
aliceCoinsTimeEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> mintTimeEx2)

-- Alice gives one token to Bob
txbodyTimeEx2 :: TxBody MaryTest
txbodyTimeEx2 =
  makeTxb
    [TxIn (txid txbodyTimeEx1Valid) 0]
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
    Map.fromList
      [ (TxIn (txid txbodyTimeEx2) 0, TxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2)),
        (TxIn (txid txbodyTimeEx2) 1, TxOut Cast.bobAddr bobTokensTimeEx2),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

--------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens only Alice can mint
--
-- Variables ending with SingExN (for a numeral N)
-- refer to this example.
--------------------------------------------------------------

alicePolicy :: Timelock MaryTest
alicePolicy = RequireSignature . asWitness . hashKey . vKey $ Cast.alicePay

alicePolicyId :: PolicyID MaryTest
alicePolicyId = PolicyID $ hashScript alicePolicy

tokenSingWitEx1 :: AssetName
tokenSingWitEx1 = AssetName $ BS.pack "tokenSingWitEx1"

-----------------------
-- Mint Alice Tokens --
-----------------------

mintSingWitEx1 :: Value MaryTest
mintSingWitEx1 =
  Value 0 $
    Map.singleton alicePolicyId (Map.singleton tokenSingWitEx1 17)

bobCoinsSingWitEx1 :: Coin
bobCoinsSingWitEx1 = bobInitCoin <-> feeEx

tokensSingWitEx1 :: Value MaryTest
tokensSingWitEx1 = mintSingWitEx1 <+> (Val.inject bobCoinsSingWitEx1)

-- Bob pays the fees, but only alice can witness the minting
txbodySingWitEx1 :: TxBody MaryTest
txbodySingWitEx1 =
  makeTxb
    [TxIn bootstrapTxId 1]
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
    Map.fromList
      [ (TxIn (txid txbodySingWitEx1) 0, TxOut Cast.bobAddr tokensSingWitEx1),
        (TxIn bootstrapTxId 0, TxOut Cast.aliceAddr (Val.inject aliceInitCoin))
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
          testCase "minting, invalid no forge signature" $
            testMaryNoDelegLEDGER
              initUTxO
              txSingWitEx1Invalid
              (ledgerEnv $ SlotNo 0)
              (policyFailure alicePolicyId)
        ]
    ]
