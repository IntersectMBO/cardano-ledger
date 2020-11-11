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
  ( AssetID (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (TxBody (..))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API (LedgerEnv (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Hashing (HashAnnotated (hashAnnotated))
import Shelley.Spec.Ledger.Keys (asWitness)
import Shelley.Spec.Ledger.LedgerState (AccountState (..))
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), emptyPParams)
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
        (ValidityInterval SNothing SNothing)
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

-------------------------------------------------
-- Introduce a new Token Bundle, Purple Tokens --
-------------------------------------------------

-- This is the most lax policy possible, requiring no authorization at all.
purplePolicy :: Timelock MaryTest
purplePolicy = RequireAllOf (StrictSeq.fromList [])

purplePolicyId :: PolicyID MaryTest
purplePolicyId = PolicyID $ hashScript purplePolicy

plum :: AssetID
plum = AssetID $ BS.pack "plum"

amethyst :: AssetID
amethyst = AssetID $ BS.pack "amethyst"

------------------------
-- Mint Purple Tokens --
------------------------

purpleTokensEx1 :: Value MaryTest
purpleTokensEx1 =
  Value 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

feeTx1 :: Coin
feeTx1 = Coin 3

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> feeTx1

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodyEx1 :: TxBody MaryTest
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn bootstrapTxId 0])
    ( StrictSeq.fromList
        [ TxOut Cast.aliceAddr (Val.inject aliceCoinEx1),
          TxOut Cast.aliceAddr purpleTokensEx1
        ]
    )
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx1
    (ValidityInterval SNothing SNothing)
    SNothing
    SNothing
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
  TxBody
    (Set.fromList [TxIn (txid txbodyEx1) 0, TxIn (txid txbodyEx1) 1])
    ( StrictSeq.fromList
        [ TxOut Cast.aliceAddr purpleTokensAliceEx2,
          TxOut Cast.bobAddr purpleTokensBobEx2
        ]
    )
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx1
    (ValidityInterval SNothing SNothing)
    SNothing
    SNothing
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

--
-- Multi-Assets Test Group
--

multiAssetsExample :: TestTree
multiAssetsExample =
  testGroup
    "multi-assets"
    [ testCase "simple minting" $
        testMaryNoDelegLEDGER initUTxO txEx1 (ledgerEnv $ SlotNo 0) (Right expectedUTxOEx1),
      testCase "simple token transfer" $
        testMaryNoDelegLEDGER expectedUTxOEx1 txEx2 (ledgerEnv $ SlotNo 1) (Right expectedUTxOEx2)
    ]
