{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Alonzo.Golden
-- Description : Golden Tests for the Alonzo era
module Test.Cardano.Ledger.Alonzo.Golden
  ( goldenUTxOEntryMinAda,
    goldenSerialization,
    goldenMinFee,
    goldenScriptIntegrity,
  )
where

import Cardano.Binary (Annotator (..), FullByteString (Full), fromCBOR, serialize)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams
  ( AlonzoPParams,
    AlonzoPParamsHKD (..),
    LangDepView (..),
    emptyPParams,
    getLanguageView,
  )
import Cardano.Ledger.Alonzo.Rules (utxoEntrySize)
import Cardano.Ledger.Alonzo.Scripts (CostModel, CostModels (..), Prices (..), mkCostModel)
import Cardano.Ledger.Alonzo.Tx (minfee)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), boundRational)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Era (EraSegWits (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), valueFromList)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as B16L
import Data.Either (fromRight)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import Plutus.V1.Ledger.Api (Data (..))
import qualified Plutus.V1.Ledger.Api as PV1 (costModelParamNames)
import qualified Plutus.V2.Ledger.Api as PV2 (costModelParamNames)
import Test.Cardano.Ledger.Alonzo.Examples.Consensus (ledgerExamplesAlonzo)
import Test.Cardano.Ledger.Alonzo.Serialisation.CDDL (readDataFile)
import Test.Cardano.Ledger.EraBuffet (StandardCrypto)
import Test.Cardano.Ledger.Mary.Golden
  ( largestName,
    minUTxO,
    pid1,
    pid2,
    pid3,
    smallName,
    smallestName,
  )
import Test.Cardano.Ledger.Shelley.Examples.Cast (aliceAddr, bobAddr, carlAddr)
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

-- | ada cost of storing a word8 of data as a UTxO entry, assuming no change to minUTxOValue
coinsPerUTxOWordLocal :: Integer
coinsPerUTxOWordLocal = quot minUTxOValueShelleyMA utxoEntrySizeWithoutValLocal
  where
    utxoEntrySizeWithoutValLocal = 29
    Coin minUTxOValueShelleyMA = minUTxO

calcMinUTxO :: AlonzoTxOut (AlonzoEra StandardCrypto) -> Coin
calcMinUTxO tout = Coin (utxoEntrySize tout * coinsPerUTxOWordLocal)

-- | (heapWords of a DataHash) * coinsPerUTxOWordLocal is 344820
goldenUTxOEntryMinAda :: TestTree
goldenUTxOEntryMinAda =
  testGroup
    "golden tests - UTxOEntryMinAda"
    [ testCase "one policy, one (smallest) name, yes datum hash" $
        calcMinUTxO
          ( AlonzoTxOut
              carlAddr
              (valueFromList 1407406 [(pid1, smallestName, 1)])
              (SJust $ hashData @(AlonzoEra StandardCrypto) (Data (List [])))
          )
          @?= Coin 1655136,
      testCase "one policy, one (smallest) name, no datum hash" $
        calcMinUTxO
          ( AlonzoTxOut
              bobAddr
              (valueFromList 1407406 [(pid1, smallestName, 1)])
              SNothing
          )
          @?= Coin 1310316,
      testCase "one policy, one (small) name" $
        calcMinUTxO
          ( AlonzoTxOut
              aliceAddr
              (valueFromList 1444443 [(pid1, smallName 1, 1)])
              SNothing
          )
          @?= Coin 1344798,
      testCase "one policy, three (small) names" $
        calcMinUTxO
          ( AlonzoTxOut
              aliceAddr
              ( valueFromList
                  1555554
                  [ (pid1, smallName 1, 1),
                    (pid1, smallName 2, 1),
                    (pid1, smallName 3, 1)
                  ]
              )
              SNothing
          )
          @?= Coin 1448244,
      testCase "one policy, one (largest) name" $
        calcMinUTxO
          ( AlonzoTxOut
              carlAddr
              (valueFromList 1555554 [(pid1, largestName 65, 1)])
              SNothing
          )
          @?= Coin 1448244,
      testCase "one policy, three (largest) name, with hash" $
        calcMinUTxO
          ( AlonzoTxOut
              carlAddr
              ( valueFromList
                  1962961
                  [ (pid1, largestName 65, 1),
                    (pid1, largestName 66, 1),
                    (pid1, largestName 67, 1)
                  ]
              )
              (SJust $ hashData @(AlonzoEra StandardCrypto) (Data (Constr 0 [Constr 0 []])))
          )
          @?= Coin 2172366,
      testCase "two policies, one (smallest) name" $
        calcMinUTxO
          ( AlonzoTxOut
              aliceAddr
              (valueFromList 1592591 [(pid1, smallestName, 1), (pid2, smallestName, 1)])
              SNothing
          )
          @?= Coin 1482726,
      testCase "two policies, one (smallest) name, with hash" $
        calcMinUTxO
          ( AlonzoTxOut
              aliceAddr
              (valueFromList 1592591 [(pid1, smallestName, 1), (pid2, smallestName, 1)])
              (SJust $ hashData @(AlonzoEra StandardCrypto) (Data (Constr 0 [])))
          )
          @?= Coin 1827546,
      testCase "two policies, two (small) names" $
        calcMinUTxO
          ( AlonzoTxOut
              bobAddr
              (valueFromList 1629628 [(pid1, smallName 1, 1), (pid2, smallName 2, 1)])
              SNothing
          )
          @?= Coin 1517208,
      testCase "three policies, ninety-six (small) names" $
        calcMinUTxO
          ( AlonzoTxOut
              aliceAddr
              ( let f i c = (i, smallName c, 1)
                 in valueFromList 7407400 [f i c | (i, cs) <- [(pid1, [32 .. 63]), (pid2, [64 .. 95]), (pid3, [96 .. 127])], c <- cs]
              )
              SNothing
          )
          @?= Coin 6896400,
      testCase "utxo entry size of ada-only" $
        -- This value, 29, is helpful for comparing the alonzo protocol parameter utxoCostPerWord
        -- with the old parameter minUTxOValue.
        -- If we wish to keep the ada-only, no datum hash, minimum value nearly the same,
        -- we can divide minUTxOValue by 29 and round.
        utxoEntrySize @(AlonzoEra StandardCrypto) (AlonzoTxOut aliceAddr (MaryValue 0 (MultiAsset mempty)) SNothing) @?= 29
    ]

goldenSerialization :: TestTree
goldenSerialization =
  testGroup
    "golden tests - serialization"
    [ testCase "Alonzo Block" $ do
        expected <- readDataFile "golden/block.cbor"
        serialize (SLE.sleBlock ledgerExamplesAlonzo) @?= expected,
      testCase "Alonzo Tx" $ do
        expected <- readDataFile "golden/tx.cbor"
        serialize (SLE.sleTx ledgerExamplesAlonzo) @?= expected
    ]

goldenMinFee :: TestTree
goldenMinFee =
  testGroup
    "golden tests - minimum fee calculation"
    [ testCase "Alonzo Block" $ do
        -- This golden test uses the block from:
        -- https://github.com/input-output-hk/cardano-node/issues/4228#issuecomment-1195707491
        --
        -- The first transaction in this block is invalide due to:
        --   FeeTooSmallUTxO (Coin 1006053) (Coin 1001829)
        --
        -- The correct behavior is for the minimum fee for this transaction
        -- to be 1006053 lovelace, as indicated by the failure above.
        -- Nodes that had the bug determined the minimum fee to be 1001829.
        hex <- readDataFile "golden/hex-block-node-issue-4228.cbor"
        let cborBlock = fromRight mempty (B16L.decode hex)
            (_leftover, Annotator f) =
              fromRight (error "bad golden block 4228") $
                deserialiseFromBytes fromCBOR cborBlock
            _block :: Block (BHeader StandardCrypto) (AlonzoEra StandardCrypto)
            _block@(Block _header txs) = f (Full cborBlock)
            txs' = fromTxSeq @(AlonzoEra StandardCrypto) txs
            firstTx = head $ toList txs'

            -- Below are the relevant protocol parameters that were active
            -- at the time this block was rejected.
            priceMem = fromJust $ boundRational 0.0577
            priceSteps = fromJust $ boundRational 0.0000721
            prices = Prices priceMem priceSteps
            pp = emptyPParams {_minfeeA = 44, _minfeeB = 155381, _prices = prices}

        Coin 1006053 @?= minfee pp firstTx
    ]

fromRightError :: (HasCallStack, Show a) => String -> Either a b -> b
fromRightError errorMsg =
  either (\e -> error $ errorMsg ++ ": " ++ show e) id

-- | A cost model that sets everything as being free
freeCostModel :: HasCallStack => Language -> CostModel
freeCostModel lang =
  fromRightError "freeCostModel is not well-formed" $ mkCostModel lang (cmps lang)
  where
    names PlutusV1 = PV1.costModelParamNames
    names PlutusV2 = PV2.costModelParamNames
    cmps = Map.fromSet (const 0) . names

exPP :: AlonzoPParams (AlonzoEra StandardCrypto)
exPP =
  emptyPParams
    { _costmdls =
        CostModels $ Map.fromList [(l, freeCostModel l) | l <- [PlutusV1, PlutusV2]]
    }

exampleLangDepViewPV1 :: LangDepView
exampleLangDepViewPV1 = LangDepView b1 b2
  where
    b1 =
      fromRightError "invalid hex encoding of the language inside exampleLangDepViewPV1" $
        B16.decode "4100"
    b2 =
      fromRightError "invalid hex encoding of the cost model inside exampleLangDepViewPV1" $
        B16.decode $
          "58a89f0000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "000000000000000000ff"

exampleLangDepViewPV2 :: LangDepView
exampleLangDepViewPV2 = LangDepView b1 b2
  where
    b1 =
      fromRightError "invalid hex encoding of the language inside exampleLangDepViewPV2" $
        B16.decode "01"
    b2 =
      fromRightError "invalid hex encoding of the cost model inside exampleLangDepViewPV2" $
        B16.decode $
          "98af000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000"

testScriptIntegritpHash ::
  AlonzoPParams (AlonzoEra StandardCrypto) ->
  Language ->
  LangDepView ->
  Assertion
testScriptIntegritpHash pp lang view = getLanguageView pp lang @?= view

goldenScriptIntegrity :: TestTree
goldenScriptIntegrity =
  testGroup
    "golden tests - script integrity hash"
    [ testCase "PlutusV1" $ testScriptIntegritpHash exPP PlutusV1 exampleLangDepViewPV1,
      testCase "PlutusV2" $ testScriptIntegritpHash exPP PlutusV2 exampleLangDepViewPV2
    ]
