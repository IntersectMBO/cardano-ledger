{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Mary.Golden
-- Description : Golden Tests for the Mary era
module Test.Cardano.Ledger.Alonzo.Golden
  ( goldenUTxOEntryMinAda,
    goldenSerialization,
    goldenScriptIntegrity,
  )
where

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams
  ( LangDepView (..),
    PParams,
    PParams' (..),
    emptyPParams,
    getLanguageView,
  )
import Cardano.Ledger.Alonzo.Rules.Utxo (utxoEntrySize)
import Cardano.Ledger.Alonzo.Scripts (CostModels (..))
import Cardano.Ledger.Alonzo.TxBody (TxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (Value (..), valueFromList)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import Data.Char (chr)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Plutus.V1.Ledger.Api (Data (..))
import Test.Cardano.Ledger.Alonzo.Examples.Consensus (ledgerExamplesAlonzo)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1, testingCostModelV2)
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
import Test.Tasty.HUnit (testCase, (@?=))

-- | ada cost of storing a word8 of data as a UTxO entry, assuming no change to minUTxOValue
coinsPerUTxOWordLocal :: Integer
coinsPerUTxOWordLocal = quot minUTxOValueShelleyMA utxoEntrySizeWithoutValLocal
  where
    utxoEntrySizeWithoutValLocal = 29
    Coin minUTxOValueShelleyMA = minUTxO

calcMinUTxO :: TxOut (AlonzoEra StandardCrypto) -> Coin
calcMinUTxO tout = Coin (utxoEntrySize tout * coinsPerUTxOWordLocal)

-- | (heapWords of a DataHash) * coinsPerUTxOWordLocal is 344820
goldenUTxOEntryMinAda :: TestTree
goldenUTxOEntryMinAda =
  testGroup
    "golden tests - UTxOEntryMinAda"
    [ testCase "one policy, one (smallest) name, yes datum hash" $
        calcMinUTxO
          ( TxOut
              carlAddr
              (valueFromList 1407406 [(pid1, smallestName, 1)])
              (SJust $ hashData @(AlonzoEra StandardCrypto) (Data (List [])))
          )
          @?= Coin 1655136,
      testCase "one policy, one (smallest) name, no datum hash" $
        calcMinUTxO
          ( TxOut
              bobAddr
              (valueFromList 1407406 [(pid1, smallestName, 1)])
              SNothing
          )
          @?= Coin 1310316,
      testCase "one policy, one (small) name" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1444443 [(pid1, smallName '1', 1)])
              SNothing
          )
          @?= Coin 1344798,
      testCase "one policy, three (small) names" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1555554 [(pid1, smallName '1', 1), (pid1, smallName '2', 1), (pid1, smallName '3', 1)])
              SNothing
          )
          @?= Coin 1448244,
      testCase "one policy, one (largest) name" $
        calcMinUTxO
          ( TxOut
              carlAddr
              (valueFromList 1555554 [(pid1, largestName 'a', 1)])
              SNothing
          )
          @?= Coin 1448244,
      testCase "one policy, three (largest) name, with hash" $
        calcMinUTxO
          ( TxOut
              carlAddr
              ( valueFromList
                  1962961
                  [ (pid1, largestName 'a', 1),
                    (pid1, largestName 'b', 1),
                    (pid1, largestName 'c', 1)
                  ]
              )
              (SJust $ hashData @(AlonzoEra StandardCrypto) (Data (Constr 0 [(Constr 0 [])])))
          )
          @?= Coin 2172366,
      testCase "two policies, one (smallest) name" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1592591 [(pid1, smallestName, 1), (pid2, smallestName, 1)])
              SNothing
          )
          @?= Coin 1482726,
      testCase "two policies, one (smallest) name, with hash" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1592591 [(pid1, smallestName, 1), (pid2, smallestName, 1)])
              (SJust $ hashData @(AlonzoEra StandardCrypto) (Data (Constr 0 [])))
          )
          @?= Coin 1827546,
      testCase "two policies, two (small) names" $
        calcMinUTxO
          ( TxOut
              bobAddr
              (valueFromList 1629628 [(pid1, smallName '1', 1), (pid2, smallName '2', 1)])
              SNothing
          )
          @?= Coin 1517208,
      testCase "three policies, ninety-six (small) names" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              ( let f i c = (i, smallName (chr c), 1)
                 in valueFromList 7407400 [f i c | (i, cs) <- [(pid1, [32 .. 63]), (pid2, [64 .. 95]), (pid3, [96 .. 127])], c <- cs]
              )
              {-
                            ( Value 7407400 $
                                Map.fromList
                                  [ ( pid1,
                                      (Map.fromList $ map ((,1) . smallName . chr) [32 .. 63])
                                    ),
                                    ( pid2,
                                      (Map.fromList $ map ((,1) . smallName . chr) [64 .. 95])
                                    ),
                                    ( pid3,
                                      (Map.fromList $ map ((,1) . smallName . chr) [96 .. 127])
                                    )
                                  ]
                            )
              -}
              SNothing
          )
          @?= Coin 6896400,
      testCase "utxo entry size of ada-only" $
        -- This value, 29, is helpful for comparing the alonzo protocol parameter utxoCostPerWord
        -- with the old parameter minUTxOValue.
        -- If we wish to keep the ada-only, no datum hash, minimum value nearly the same,
        -- we can divide minUTxOValue by 29 and round.
        utxoEntrySize @(AlonzoEra StandardCrypto) (TxOut aliceAddr (Value 0 mempty) SNothing) @?= 29
    ]

goldenSerialization :: TestTree
goldenSerialization =
  testGroup
    "golden tests - serialization"
    [ testCase "Alonzo Block" $ do
        expected <- (BSL.readFile "golden/block.cbor")
        serialize (SLE.sleBlock ledgerExamplesAlonzo) @?= expected,
      testCase "Alonzo Tx" $ do
        expected <- (BSL.readFile "golden/tx.cbor")
        serialize (SLE.sleTx ledgerExamplesAlonzo) @?= expected
    ]

exPP :: PParams (AlonzoEra StandardCrypto)
exPP =
  emptyPParams
    { _costmdls =
        CostModels $
          Map.fromList [(PlutusV1, testingCostModelV1), (PlutusV2, testingCostModelV2)]
    }

exampleLangDepViewPV1 :: LangDepView
exampleLangDepViewPV1 = LangDepView b1 b2
  where
    b1 =
      fromRight (error "invalid hex encoding of the language inside exampleLangDepViewPV1") $
        B16.decode "4100"
    b2 =
      fromRight (error "invalid hex encoding of the cost model inside exampleLangDepViewPV1") $
        B16.decode $
          "5901d59f1a000302590001011a00060bc719026d00011a000249f01903e80001"
            <> "1a000249f018201a0025cea81971f70419744d186419744d186419744d186419"
            <> "744d186419744d186419744d18641864186419744d18641a000249f018201a00"
            <> "0249f018201a000249f018201a000249f01903e800011a000249f018201a0002"
            <> "49f01903e800081a000242201a00067e2318760001011a000249f01903e80008"
            <> "1a000249f01a0001b79818f7011a000249f0192710011a0002155e19052e0119"
            <> "03e81a000249f01903e8011a000249f018201a000249f018201a000249f01820"
            <> "01011a000249f0011a000249f0041a000194af18f8011a000194af18f8011a00"
            <> "02377c190556011a0002bdea1901f1011a000249f018201a000249f018201a00"
            <> "0249f018201a000249f018201a000249f018201a000249f018201a000242201a"
            <> "00067e23187600010119f04c192bd200011a000249f018201a000242201a0006"
            <> "7e2318760001011a000242201a00067e2318760001011a0025cea81971f70400"
            <> "1a000141bb041a000249f019138800011a000249f018201a000302590001011a"
            <> "000249f018201a000249f018201a000249f018201a000249f018201a000249f0"
            <> "18201a000249f018201a000249f018201a00330da70101ff"

exampleLangDepViewPV2 :: LangDepView
exampleLangDepViewPV2 = LangDepView b1 b2
  where
    b1 =
      fromRight (error "invalid hex encoding of the language inside exampleLangDepViewPV2") $
        B16.decode "01"
    b2 =
      fromRight (error "invalid hex encoding of the cost model inside exampleLangDepViewPV2") $
        B16.decode $
          "98a61a000302590001011a00060bc719026d00011a000249f01903e800011a00"
            <> "0249f018201a0025cea81971f70419744d186419744d186419744d186419744d"
            <> "186419744d186419744d18641864186419744d18641a000249f018201a000249"
            <> "f018201a000249f018201a000249f01903e800011a000249f018201a000249f0"
            <> "1903e800081a000242201a00067e2318760001011a000249f01903e800081a00"
            <> "0249f01a0001b79818f7011a000249f0192710011a0002155e19052e011903e8"
            <> "1a000249f01903e8011a000249f018201a000249f018201a000249f018200101"
            <> "1a000249f0011a000249f0041a000194af18f8011a000194af18f8011a000237"
            <> "7c190556011a0002bdea1901f1011a000249f018201a000249f018201a000249"
            <> "f018201a000249f018201a000249f018201a000249f018201a000242201a0006"
            <> "7e23187600010119f04c192bd200011a000249f018201a000242201a00067e23"
            <> "18760001011a000242201a00067e2318760001011a0025cea81971f704001a00"
            <> "0141bb041a000249f019138800011a000249f018201a000302590001011a0002"
            <> "49f018201a000249f018201a000249f018201a000249f018201a000249f01820"
            <> "1a000249f018201a000249f018201a00330da70101"

testScriptIntegritpHash :: PParams (AlonzoEra StandardCrypto) -> Language -> LangDepView -> IO ()
testScriptIntegritpHash pp lang view = getLanguageView pp lang @?= view

goldenScriptIntegrity :: TestTree
goldenScriptIntegrity =
  testGroup
    "golden tests - script integrity hash"
    [ testCase "PlutusV1" $ testScriptIntegritpHash exPP PlutusV1 exampleLangDepViewPV1,
      testCase "PlutusV2" $ testScriptIntegritpHash exPP PlutusV2 exampleLangDepViewPV2
    ]
