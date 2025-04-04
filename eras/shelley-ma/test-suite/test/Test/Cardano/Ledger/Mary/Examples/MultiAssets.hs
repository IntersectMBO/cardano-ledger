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
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.State
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
 )
import Cardano.Ledger.Shelley.API (LedgerEnv (..), ShelleyLEDGER)
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

bootstrapTxId :: TxId
bootstrapTxId = txIdTxBody txb
  where
    txb :: TxBody MaryEra
    txb = mkBasicTxBody

initUTxO :: UTxO MaryEra
initUTxO =
  UTxO $
    Map.fromList
      [ (mkTxInPartial bootstrapTxId 0, ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      , (mkTxInPartial bootstrapTxId 1, ShelleyTxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

pp :: PParams MaryEra
pp =
  emptyPParams
    & ppMinFeeAL .~ Coin 0
    & ppMinFeeBL .~ Coin 1
    & ppMaxTxSizeL .~ 16384
    & ppMinUTxOValueL .~ Coin 100

ledgerEnv :: SlotNo -> LedgerEnv MaryEra
ledgerEnv s = LedgerEnv s Nothing minBound pp (ChainAccountState (Coin 0) (Coin 0))

feeEx :: Coin
feeEx = Coin 3

-- These examples do not use several of the transaction components,
-- so we can simplify building them.
makeMaryTxBody ::
  [TxIn] ->
  [ShelleyTxOut MaryEra] ->
  ValidityInterval ->
  MultiAsset ->
  TxBody MaryEra
makeMaryTxBody ins outs interval minted =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList ins
    & outputsTxBodyL .~ StrictSeq.fromList outs
    & feeTxBodyL .~ feeEx
    & vldtTxBodyL .~ interval
    & mintTxBodyL .~ minted

policyFailure ::
  PolicyID -> Either (NonEmpty (PredicateFailure (ShelleyLEDGER MaryEra))) (UTxO MaryEra)
policyFailure p =
  Left . pure . UtxowFailure . ScriptWitnessNotValidatingUTXOW $ Set.singleton (policyID p)

outTooBigFailure ::
  ShelleyTxOut MaryEra -> Either (NonEmpty (PredicateFailure (ShelleyLEDGER MaryEra))) (UTxO MaryEra)
outTooBigFailure out =
  Left . pure . UtxowFailure . UtxoFailure $ OutputTooBigUTxO [out]

----------------------------------------------------
-- Introduce a new Token Bundle, Purple Tokens
--
-- Variables ending with SimpleExN (for a numeral N)
-- refer to this example.
----------------------------------------------------

-- This is the most lax policy possible, requiring no authorization at all.
purplePolicy :: Timelock MaryEra
purplePolicy = RequireAllOf (StrictSeq.fromList [])

purplePolicyId :: PolicyID
purplePolicyId = PolicyID $ hashScript @MaryEra purplePolicy

plum :: AssetName
plum = AssetName "plum"

amethyst :: AssetName
amethyst = AssetName "amethyst"

------------------------
-- Mint Purple Tokens --
------------------------

mintSimpleEx1 :: MultiAsset
mintSimpleEx1 =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

aliceCoinSimpleEx1 :: Coin
aliceCoinSimpleEx1 = aliceInitCoin <-> feeEx

tokensSimpleEx1 :: MaryValue
tokensSimpleEx1 = MaryValue mempty mintSimpleEx1 <+> Val.inject aliceCoinSimpleEx1

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodySimpleEx1 :: TxBody MaryEra
txbodySimpleEx1 =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 0]
    [ShelleyTxOut Cast.aliceAddr tokensSimpleEx1]
    unboundedInterval
    mintSimpleEx1

txSimpleEx1 :: ShelleyTx MaryEra
txSimpleEx1 =
  mkBasicTx txbodySimpleEx1
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodySimpleEx1) [asWitness Cast.alicePay]
    stw = Map.fromList [(policyID purplePolicyId, purplePolicy)]

expectedUTxOSimpleEx1 :: UTxO MaryEra
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

aliceTokensSimpleEx2 :: MaryValue
aliceTokensSimpleEx2 =
  MaryValue aliceCoinsSimpleEx2 $
    MultiAsset $
      Map.singleton purplePolicyId (Map.fromList [(plum, 8), (amethyst, 2)])

bobTokensSimpleEx2 :: MaryValue
bobTokensSimpleEx2 =
  MaryValue minUtxoSimpleEx2 $
    MultiAsset $
      Map.singleton purplePolicyId (Map.singleton plum 5)

-- Alice gives five plums to Bob.
txbodySimpleEx2 :: TxBody MaryEra
txbodySimpleEx2 =
  makeMaryTxBody
    [mkTxInPartial (txIdTxBody txbodySimpleEx1) 0]
    [ ShelleyTxOut Cast.aliceAddr aliceTokensSimpleEx2
    , ShelleyTxOut Cast.bobAddr bobTokensSimpleEx2
    ]
    unboundedInterval
    mempty

txSimpleEx2 :: ShelleyTx MaryEra
txSimpleEx2 =
  mkBasicTx txbodySimpleEx2
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodySimpleEx2) [asWitness Cast.alicePay]

expectedUTxOSimpleEx2 :: UTxO MaryEra
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

boundedTimePolicy :: Timelock MaryEra
boundedTimePolicy =
  RequireAllOf
    ( StrictSeq.fromList
        [ RequireTimeStart startInterval
        , RequireTimeExpire stopInterval
        ]
    )

boundedTimePolicyId :: PolicyID
boundedTimePolicyId = PolicyID $ hashScript @MaryEra boundedTimePolicy

tokenTimeEx :: AssetName
tokenTimeEx = AssetName "tokenTimeEx"

------------------------------------
-- Mint Bounded Time Range Tokens --
------------------------------------

mintTimeEx1 :: MultiAsset
mintTimeEx1 =
  MultiAsset $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx1 :: Coin
aliceCoinsTimeEx1 = aliceInitCoin <-> feeEx

tokensTimeEx1 :: MaryValue
tokensTimeEx1 = MaryValue mempty mintTimeEx1 <+> Val.inject aliceCoinsTimeEx1

-- Mint tokens
txbodyTimeEx1 :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> TxBody MaryEra
txbodyTimeEx1 s e =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 0]
    [ShelleyTxOut Cast.aliceAddr tokensTimeEx1]
    (ValidityInterval s e)
    mintTimeEx1

txbodyTimeEx1Valid :: TxBody MaryEra
txbodyTimeEx1Valid = txbodyTimeEx1 (SJust startInterval) (SJust stopInterval)

txTimeEx1 :: TxBody MaryEra -> ShelleyTx MaryEra
txTimeEx1 txbody =
  mkBasicTx txbody
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbody) [asWitness Cast.alicePay]
    stw = Map.fromList [(policyID boundedTimePolicyId, boundedTimePolicy)]

txTimeEx1Valid :: ShelleyTx MaryEra
txTimeEx1Valid = txTimeEx1 txbodyTimeEx1Valid

txTimeEx1InvalidLHSfixed :: ShelleyTx MaryEra
txTimeEx1InvalidLHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust beforeStart) (SJust stopInterval)

txTimeEx1InvalidLHSopen :: ShelleyTx MaryEra
txTimeEx1InvalidLHSopen = txTimeEx1 $ txbodyTimeEx1 SNothing (SJust stopInterval)

txTimeEx1InvalidRHSfixed :: ShelleyTx MaryEra
txTimeEx1InvalidRHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) (SJust afterStop)

txTimeEx1InvalidRHSopen :: ShelleyTx MaryEra
txTimeEx1InvalidRHSopen = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) SNothing

expectedUTxOTimeEx1 :: UTxO MaryEra
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

bobTokensTimeEx2 :: MaryValue
bobTokensTimeEx2 =
  MaryValue mintTimeEx2 $
    MultiAsset $
      Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx2 :: Coin
aliceCoinsTimeEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> mintTimeEx2)

-- Alice gives one token to Bob
txbodyTimeEx2 :: TxBody MaryEra
txbodyTimeEx2 =
  makeMaryTxBody
    [mkTxInPartial (txIdTxBody txbodyTimeEx1Valid) 0]
    [ ShelleyTxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2)
    , ShelleyTxOut Cast.bobAddr bobTokensTimeEx2
    ]
    unboundedInterval
    mempty

txTimeEx2 :: ShelleyTx MaryEra
txTimeEx2 =
  ShelleyTx
    txbodyTimeEx2
    mempty
      { addrWits =
          mkWitnessesVKey (hashAnnotated txbodyTimeEx2) [asWitness Cast.alicePay]
      }
    SNothing

expectedUTxOTimeEx2 :: UTxO MaryEra
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

alicePolicy :: Timelock MaryEra
alicePolicy = RequireSignature . asWitness . hashKey . vKey $ Cast.alicePay

alicePolicyId :: PolicyID
alicePolicyId = PolicyID $ hashScript @MaryEra alicePolicy

tokenSingWitEx1 :: AssetName
tokenSingWitEx1 = AssetName "tokenSingWitEx1"

-----------------------
-- Mint Alice Tokens --
-----------------------

mintSingWitEx1 :: MultiAsset
mintSingWitEx1 =
  MultiAsset $
    Map.singleton alicePolicyId (Map.singleton tokenSingWitEx1 17)

bobCoinsSingWitEx1 :: Coin
bobCoinsSingWitEx1 = bobInitCoin <-> feeEx

tokensSingWitEx1 :: MaryValue
tokensSingWitEx1 = MaryValue mempty mintSingWitEx1 <+> Val.inject bobCoinsSingWitEx1

-- Bob pays the fees, but only alice can witness the minting
txbodySingWitEx1 :: TxBody MaryEra
txbodySingWitEx1 =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 1]
    [ShelleyTxOut Cast.bobAddr tokensSingWitEx1]
    unboundedInterval
    mintSingWitEx1

txSingWitEx1Valid :: ShelleyTx MaryEra
txSingWitEx1Valid =
  mkBasicTx txbodySingWitEx1
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodySingWitEx1) [asWitness Cast.bobPay, asWitness Cast.alicePay]
    stw = Map.fromList [(policyID alicePolicyId, alicePolicy)]

expectedUTxOSingWitEx1 :: UTxO MaryEra
expectedUTxOSingWitEx1 =
  UTxO $
    Map.fromList
      [ (mkTxInPartial (txIdTxBody txbodySingWitEx1) 0, ShelleyTxOut Cast.bobAddr tokensSingWitEx1)
      , (mkTxInPartial bootstrapTxId 0, ShelleyTxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      ]

txSingWitEx1Invalid :: ShelleyTx MaryEra
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
mintNegEx1 :: MultiAsset
mintNegEx1 =
  MultiAsset $
    Map.singleton purplePolicyId (Map.singleton plum (-8))

aliceTokensNegEx1 :: MaryValue
aliceTokensNegEx1 =
  MaryValue (aliceCoinsSimpleEx2 <-> feeEx) $
    MultiAsset $
      Map.singleton purplePolicyId (Map.singleton amethyst 2)

txbodyNegEx1 :: TxBody MaryEra
txbodyNegEx1 =
  makeMaryTxBody
    [mkTxInPartial (txIdTxBody txbodySimpleEx2) 0]
    [ShelleyTxOut Cast.aliceAddr aliceTokensNegEx1]
    unboundedInterval
    mintNegEx1

txNegEx1 :: ShelleyTx MaryEra
txNegEx1 =
  mkBasicTx txbodyNegEx1
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ atw & scriptTxWitsL .~ stw)
  where
    atw = mkWitnessesVKey (hashAnnotated txbodyNegEx1) [asWitness Cast.alicePay]
    stw = Map.fromList [(policyID purplePolicyId, purplePolicy)]

initialUTxONegEx1 :: UTxO MaryEra
initialUTxONegEx1 = expectedUTxOSimpleEx2

expectedUTxONegEx1 :: UTxO MaryEra
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

mintNegEx2 :: MultiAsset
mintNegEx2 =
  MultiAsset $
    Map.singleton purplePolicyId (Map.singleton plum (-9))

aliceTokensNegEx2 :: MaryValue
aliceTokensNegEx2 =
  MaryValue (aliceCoinsSimpleEx2 <-> feeEx) $
    MultiAsset $
      Map.singleton purplePolicyId (Map.fromList [(plum, -1), (amethyst, 2)])

-- Mint negative valued tokens
txbodyNegEx2 :: TxBody MaryEra
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

smallValue :: MultiAsset
smallValue =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

smallOut :: ShelleyTxOut MaryEra
smallOut =
  ShelleyTxOut Cast.aliceAddr $
    MaryValue mempty smallValue
      <+> Val.inject (aliceInitCoin <-> (feeEx <+> minUtxoBigEx))

numAssets :: Int
numAssets = 1000

bigValue :: MultiAsset
bigValue =
  MultiAsset $
    Map.singleton
      purplePolicyId
      (Map.fromList $ map (\x -> (AssetName . fromString $ show x, 1)) [1 .. numAssets])

bigOut :: ShelleyTxOut MaryEra
bigOut = ShelleyTxOut Cast.aliceAddr $ MaryValue mempty bigValue <+> Val.inject minUtxoBigEx

txbodyWithBigValue :: TxBody MaryEra
txbodyWithBigValue =
  makeMaryTxBody
    [mkTxInPartial bootstrapTxId 0]
    [smallOut, bigOut]
    unboundedInterval
    (bigValue <> smallValue)

txBigValue :: ShelleyTx MaryEra
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
