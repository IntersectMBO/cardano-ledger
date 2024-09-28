{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Mary.Examples.MultiAssets
-- Description : Multi-Assets Examples
--
-- Examples demonstrating the use of multi-assets.
module Test.Cardano.Ledger.Mary.Examples.MultiAssets (
  multiAssetsExample,
)
where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure (..))
import Cardano.Ledger.Allegra.Scripts (
  Timelock (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (asWitness, hashKey)
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
 )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (LedgerEnv (..), ShelleyLEDGER)
import Cardano.Ledger.Shelley.LedgerState (AccountState (..))
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure (..), ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (addrWits)
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Control.Exception (ErrorCall (ErrorCall), evaluate, try)
import Control.State.Transition.Extended (PredicateFailure)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Exts (fromString)
import Lens.Micro
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessesVKey)
import Test.Cardano.Ledger.Mary.Examples (testMaryNoDelegLEDGER)
import qualified Test.Cardano.Ledger.Mary.Examples.Cast as Cast
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

------------------------------
-- Set Up the Initial State --
------------------------------

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10_000_000_000_000_000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1_000_000_000_000_000

unboundedInterval :: ValidityInterval
unboundedInterval = ValidityInterval SNothing SNothing

bootstrapTxId :: TxId StandardCrypto
bootstrapTxId = txIdTxBody txb
  where
    txb :: TxBody Mary
    txb = mkBasicTxBody

initUTxO :: UTxO Mary
initUTxO =
  UTxO $
    Map.fromList
      [ (mkTxInPartial bootstrapTxId 0, ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      , (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

pp :: PParams Mary
pp =
  emptyPParams
    & ppMinFeeAL .~ Coin 0
    & ppMinFeeBL .~ Coin 1
    & ppMaxTxSizeL .~ 16384
    & ppMinUTxOValueL .~ Coin 100

ledgerEnv :: SlotNo -> LedgerEnv Mary
ledgerEnv s = LedgerEnv s minBound pp (AccountState (Coin 0) (Coin 0)) False

feeEx :: Coin
feeEx = Coin 3

-- These examples do not use several of the transaction components,
-- so we can simplify building them.
makeMaryTxBody ::
  [TxIn StandardCrypto] ->
  [ShelleyTxOut Mary] ->
  ValidityInterval ->
  MultiAsset StandardCrypto ->
  TxBody Mary
makeMaryTxBody ins outs interval minted =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList ins
    & outputsTxBodyL .~ StrictSeq.fromList outs
    & feeTxBodyL .~ feeEx
    & vldtTxBodyL .~ interval
    & mintTxBodyL .~ minted

policyFailure ::
  PolicyID StandardCrypto -> Either (NonEmpty (PredicateFailure (ShelleyLEDGER Mary))) (UTxO Mary)
policyFailure p =
  Left . pure . UtxowFailure . ScriptWitnessNotValidatingUTXOW $ Set.singleton (policyID p)

outTooBigFailure ::
  ShelleyTxOut Mary -> Either (NonEmpty (PredicateFailure (ShelleyLEDGER Mary))) (UTxO Mary)
outTooBigFailure out =
  Left . pure . UtxowFailure . UtxoFailure $ OutputTooBigUTxO [out]

----------------------------------------------------
-- Introduce a new Token Bundle, Purple Tokens
--
-- Variables ending with SimpleExN (for a numeral N)
-- refer to this example.
----------------------------------------------------

-- This is the most lax policy possible, requiring no authorization at all.
purplePolicy :: Timelock Mary
purplePolicy = RequireAllOf (StrictSeq.fromList [])

purplePolicyId :: PolicyID StandardCrypto
purplePolicyId = PolicyID $ hashScript @Mary purplePolicy

plum :: AssetName
plum = AssetName "plum"

amethyst :: AssetName
amethyst = AssetName "amethyst"

------------------------
-- Mint Purple Tokens --
------------------------

mintSimpleEx1 :: MultiAsset StandardCrypto
mintSimpleEx1 =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

aliceCoinSimpleEx1 :: Coin
aliceCoinSimpleEx1 = aliceInitCoin <-> feeEx

tokensSimpleEx1 :: MaryValue StandardCrypto
tokensSimpleEx1 = MaryValue mempty mintSimpleEx1 <+> Val.inject aliceCoinSimpleEx1

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodySimpleEx1 :: TxBody Mary
txbodySimpleEx1 =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 0]
    [ShelleyTxOut Cast.aliceAddr tokensSimpleEx1]
    unboundedInterval
    mintSimpleEx1

txSimpleEx1 :: ShelleyTx Mary
txSimpleEx1 =
  mkBasicTx txbodySimpleEx1
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodySimpleEx1) [asWitness Cast.alicePay]
    stw = Map.fromList [(policyID purplePolicyId, purplePolicy)]

expectedUTxOSimpleEx1 :: UTxO Mary
expectedUTxOSimpleEx1 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txIdTxBody txbodySimpleEx1) 0, ShelleyTxOut Cast.aliceAddr tokensSimpleEx1)
      , (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------
-- Transfer Purple Tokens --
----------------------------

minUtxoSimpleEx2 :: Coin
minUtxoSimpleEx2 = Coin 117

aliceCoinsSimpleEx2 :: Coin
aliceCoinsSimpleEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> minUtxoSimpleEx2)

aliceTokensSimpleEx2 :: MaryValue StandardCrypto
aliceTokensSimpleEx2 =
  MaryValue aliceCoinsSimpleEx2 $
    MultiAsset $
      Map.singleton purplePolicyId (Map.fromList [(plum, 8), (amethyst, 2)])

bobTokensSimpleEx2 :: MaryValue StandardCrypto
bobTokensSimpleEx2 =
  MaryValue minUtxoSimpleEx2 $
    MultiAsset $
      Map.singleton purplePolicyId (Map.singleton plum 5)

-- Alice gives five plums to Bob.
txbodySimpleEx2 :: TxBody Mary
txbodySimpleEx2 =
  makeMaryTxBody
    [mkTxInPartial (txIdTxBody txbodySimpleEx1) 0]
    [ ShelleyTxOut Cast.aliceAddr aliceTokensSimpleEx2
    , ShelleyTxOut Cast.bobAddr bobTokensSimpleEx2
    ]
    unboundedInterval
    mempty

txSimpleEx2 :: ShelleyTx Mary
txSimpleEx2 =
  mkBasicTx txbodySimpleEx2
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodySimpleEx2) [asWitness Cast.alicePay]

expectedUTxOSimpleEx2 :: UTxO Mary
expectedUTxOSimpleEx2 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txIdTxBody txbodySimpleEx2) 0, ShelleyTxOut Cast.aliceAddr aliceTokensSimpleEx2)
      , (mkTxInPartial (txIdTxBody txbodySimpleEx2) 1, ShelleyTxOut Cast.bobAddr bobTokensSimpleEx2)
      , (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
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

boundedTimePolicy :: Timelock Mary
boundedTimePolicy =
  RequireAllOf
    ( StrictSeq.fromList
        [ RequireTimeStart startInterval
        , RequireTimeExpire stopInterval
        ]
    )

boundedTimePolicyId :: PolicyID StandardCrypto
boundedTimePolicyId = PolicyID $ hashScript @Mary boundedTimePolicy

tokenTimeEx :: AssetName
tokenTimeEx = AssetName "tokenTimeEx"

------------------------------------
-- Mint Bounded Time Range Tokens --
------------------------------------

mintTimeEx1 :: MultiAsset StandardCrypto
mintTimeEx1 =
  MultiAsset $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx1 :: Coin
aliceCoinsTimeEx1 = aliceInitCoin <-> feeEx

tokensTimeEx1 :: MaryValue StandardCrypto
tokensTimeEx1 = MaryValue mempty mintTimeEx1 <+> Val.inject aliceCoinsTimeEx1

-- Mint tokens
txbodyTimeEx1 :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> TxBody Mary
txbodyTimeEx1 s e =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 0]
    [ShelleyTxOut Cast.aliceAddr tokensTimeEx1]
    (ValidityInterval s e)
    mintTimeEx1

txbodyTimeEx1Valid :: TxBody Mary
txbodyTimeEx1Valid = txbodyTimeEx1 (SJust startInterval) (SJust stopInterval)

txTimeEx1 :: TxBody Mary -> ShelleyTx Mary
txTimeEx1 txbody =
  mkBasicTx txbody
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbody) [asWitness Cast.alicePay]
    stw = Map.fromList [(policyID boundedTimePolicyId, boundedTimePolicy)]

txTimeEx1Valid :: ShelleyTx Mary
txTimeEx1Valid = txTimeEx1 txbodyTimeEx1Valid

txTimeEx1InvalidLHSfixed :: ShelleyTx Mary
txTimeEx1InvalidLHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust beforeStart) (SJust stopInterval)

txTimeEx1InvalidLHSopen :: ShelleyTx Mary
txTimeEx1InvalidLHSopen = txTimeEx1 $ txbodyTimeEx1 SNothing (SJust stopInterval)

txTimeEx1InvalidRHSfixed :: ShelleyTx Mary
txTimeEx1InvalidRHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) (SJust afterStop)

txTimeEx1InvalidRHSopen :: ShelleyTx Mary
txTimeEx1InvalidRHSopen = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) SNothing

expectedUTxOTimeEx1 :: UTxO Mary
expectedUTxOTimeEx1 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txIdTxBody txbodyTimeEx1Valid) 0, ShelleyTxOut Cast.aliceAddr tokensTimeEx1)
      , (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------------------
-- Transfer Bounded Time Range Tokens --
----------------------------------------

mintTimeEx2 :: Coin
mintTimeEx2 = Coin 120

bobTokensTimeEx2 :: MaryValue StandardCrypto
bobTokensTimeEx2 =
  MaryValue mintTimeEx2 $
    MultiAsset $
      Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx2 :: Coin
aliceCoinsTimeEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> mintTimeEx2)

-- Alice gives one token to Bob
txbodyTimeEx2 :: TxBody Mary
txbodyTimeEx2 =
  makeMaryTxBody
    [mkTxInPartial (txIdTxBody txbodyTimeEx1Valid) 0]
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2)
    , ShelleyTxOut Cast.bobAddr bobTokensTimeEx2
    ]
    unboundedInterval
    mempty

txTimeEx2 :: ShelleyTx Mary
txTimeEx2 =
  ShelleyTx
    txbodyTimeEx2
    mempty
      { addrWits =
          mkWitnessesVKey (hashAnnotated txbodyTimeEx2) [asWitness Cast.alicePay]
      }
    SNothing

expectedUTxOTimeEx2 :: UTxO Mary
expectedUTxOTimeEx2 =
  UTxO $
    Map.fromList
      [
        ( mkTxInPartial (txIdTxBody txbodyTimeEx2) 0
        , ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2)
        )
      , (mkTxInPartial (txIdTxBody txbodyTimeEx2) 1, ShelleyTxOut Cast.bobAddr bobTokensTimeEx2)
      , (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

--------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens only Alice can mint
--
-- Variables ending with SingExN (for a numeral N)
-- refer to this example.
--------------------------------------------------------------

alicePolicy :: Timelock Mary
alicePolicy = RequireSignature . asWitness . hashKey . vKey $ Cast.alicePay

alicePolicyId :: PolicyID StandardCrypto
alicePolicyId = PolicyID $ hashScript @Mary alicePolicy

tokenSingWitEx1 :: AssetName
tokenSingWitEx1 = AssetName "tokenSingWitEx1"

-----------------------
-- Mint Alice Tokens --
-----------------------

mintSingWitEx1 :: MultiAsset StandardCrypto
mintSingWitEx1 =
  MultiAsset $
    Map.singleton alicePolicyId (Map.singleton tokenSingWitEx1 17)

bobCoinsSingWitEx1 :: Coin
bobCoinsSingWitEx1 = bobInitCoin <-> feeEx

tokensSingWitEx1 :: MaryValue StandardCrypto
tokensSingWitEx1 = MaryValue mempty mintSingWitEx1 <+> Val.inject bobCoinsSingWitEx1

-- Bob pays the fees, but only alice can witness the minting
txbodySingWitEx1 :: TxBody Mary
txbodySingWitEx1 =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 1]
    [ShelleyTxOut Cast.bobAddr tokensSingWitEx1]
    unboundedInterval
    mintSingWitEx1

txSingWitEx1Valid :: ShelleyTx Mary
txSingWitEx1Valid =
  mkBasicTx txbodySingWitEx1
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodySingWitEx1) [asWitness Cast.bobPay, asWitness Cast.alicePay]
    stw = Map.fromList [(policyID alicePolicyId, alicePolicy)]

expectedUTxOSingWitEx1 :: UTxO Mary
expectedUTxOSingWitEx1 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txIdTxBody txbodySingWitEx1) 0, ShelleyTxOut Cast.bobAddr tokensSingWitEx1)
      , (mkTxInPartial bootstrapTxId 0, ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      ]

txSingWitEx1Invalid :: ShelleyTx Mary
txSingWitEx1Invalid =
  mkBasicTx txbodySingWitEx1
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodySingWitEx1) [asWitness Cast.bobPay]
    stw = Map.fromList [(policyID alicePolicyId, alicePolicy)]

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
mintNegEx1 :: MultiAsset StandardCrypto
mintNegEx1 =
  MultiAsset $
    Map.singleton purplePolicyId (Map.singleton plum (-8))

aliceTokensNegEx1 :: MaryValue StandardCrypto
aliceTokensNegEx1 =
  MaryValue (aliceCoinsSimpleEx2 <-> feeEx) $
    MultiAsset $
      Map.singleton purplePolicyId (Map.singleton amethyst 2)

txbodyNegEx1 :: TxBody Mary
txbodyNegEx1 =
  makeMaryTxBody
    [mkTxInPartial (txIdTxBody txbodySimpleEx2) 0]
    [ShelleyTxOut Cast.aliceAddr aliceTokensNegEx1]
    unboundedInterval
    mintNegEx1

txNegEx1 :: ShelleyTx Mary
txNegEx1 =
  mkBasicTx txbodyNegEx1
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodyNegEx1) [asWitness Cast.alicePay]
    stw = Map.fromList [(policyID purplePolicyId, purplePolicy)]

initialUTxONegEx1 :: UTxO Mary
initialUTxONegEx1 = expectedUTxOSimpleEx2

expectedUTxONegEx1 :: UTxO Mary
expectedUTxONegEx1 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txIdTxBody txbodyNegEx1) 0, ShelleyTxOut Cast.aliceAddr aliceTokensNegEx1)
      , (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      , (mkTxInPartial (txIdTxBody txbodySimpleEx2) 1, ShelleyTxOut Cast.bobAddr bobTokensSimpleEx2)
      ]

--
-- Now attempt to produce negative outputs
--

mintNegEx2 :: MultiAsset StandardCrypto
mintNegEx2 =
  MultiAsset $
    Map.singleton purplePolicyId (Map.singleton plum (-9))

aliceTokensNegEx2 :: MaryValue StandardCrypto
aliceTokensNegEx2 =
  MaryValue (aliceCoinsSimpleEx2 <-> feeEx) $
    MultiAsset $
      Map.singleton purplePolicyId (Map.fromList [(plum, -1), (amethyst, 2)])

-- Mint negative valued tokens
txbodyNegEx2 :: TxBody Mary
txbodyNegEx2 =
  makeMaryTxBody
    [mkTxInPartial (txIdTxBody txbodySimpleEx2) 0]
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

smallValue :: MultiAsset StandardCrypto
smallValue =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

smallOut :: ShelleyTxOut Mary
smallOut =
  ShelleyTxOut Cast.aliceAddr $
    MaryValue mempty smallValue
      <+> Val.inject (aliceInitCoin <-> (feeEx <+> minUtxoBigEx))

numAssets :: Int
numAssets = 1000

bigValue :: MultiAsset StandardCrypto
bigValue =
  MultiAsset $
    Map.singleton
      purplePolicyId
      (Map.fromList $ map (\x -> (AssetName . fromString $ show x, 1)) [1 .. numAssets])

bigOut :: ShelleyTxOut Mary
bigOut = ShelleyTxOut Cast.aliceAddr $ MaryValue mempty bigValue <+> Val.inject minUtxoBigEx

txbodyWithBigValue :: TxBody Mary
txbodyWithBigValue =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 0]
    [smallOut, bigOut]
    unboundedInterval
    (bigValue <> smallValue)

txBigValue :: ShelleyTx Mary
txBigValue =
  mkBasicTx txbodyWithBigValue
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodyWithBigValue) [asWitness Cast.alicePay]
    stw = Map.fromList [(policyID purplePolicyId, purplePolicy)]

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
              (Right expectedUTxOSimpleEx1)
        , testCase "transfer" $
            testMaryNoDelegLEDGER
              expectedUTxOSimpleEx1
              txSimpleEx2
              (ledgerEnv $ SlotNo 1)
              (Right expectedUTxOSimpleEx2)
        ]
    , testGroup
        "bounded time interval"
        [ testCase "minting, valid" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1Valid
              (ledgerEnv startInterval)
              (Right expectedUTxOTimeEx1)
        , testCase "minting, invalid LHS too small" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidLHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId)
        , testCase "minting, invalid LHS unspecified" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidLHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId)
        , testCase "minting, invalid RHS too big" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidRHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId)
        , testCase "minting, invalid RHS unspecified" $
            testMaryNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidRHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId)
        , testCase "transfer, after minting period" $
            testMaryNoDelegLEDGER
              expectedUTxOTimeEx1
              txTimeEx2
              (ledgerEnv afterStop)
              (Right expectedUTxOTimeEx2)
        ]
    , testGroup
        "single key"
        [ testCase "minting, valid" $
            testMaryNoDelegLEDGER
              initUTxO
              txSingWitEx1Valid
              (ledgerEnv $ SlotNo 0)
              (Right expectedUTxOSingWitEx1)
        , testCase "minting, invalid no mint signature" $
            testMaryNoDelegLEDGER
              initUTxO
              txSingWitEx1Invalid
              (ledgerEnv $ SlotNo 0)
              (policyFailure alicePolicyId)
        ]
    , testGroup
        "negative minting"
        [ testCase "remove assets" $
            testMaryNoDelegLEDGER
              initialUTxONegEx1
              txNegEx1
              (ledgerEnv $ SlotNo 3)
              (Right expectedUTxONegEx1)
        , testCase "no negative outputs" testNegEx2
        ]
    , testCase "value too big" $
        testMaryNoDelegLEDGER
          initUTxO
          txBigValue
          (ledgerEnv $ SlotNo 0)
          (outTooBigFailure bigOut)
    ]
