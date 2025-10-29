{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Golden Tests for the Alonzo era
module Test.Cardano.Ledger.Alonzo.Golden (
  tests,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoExtraConfig (..), AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.PParams (
  LangDepView (..),
  getLanguageView,
 )
import Cardano.Ledger.Alonzo.Rules (FailureDescription (..), TagMismatchDescription (..))
import Cardano.Ledger.Alonzo.Tx (alonzoMinFeeTx)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), utxoEntrySize)
import Cardano.Ledger.BaseTypes (SlotNo (..), StrictMaybe (..), boundRational)
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import Cardano.Ledger.Binary.Plain as Plain (serialize)
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (valueFromList)
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  CostModels,
  mkCostModel,
  mkCostModels,
 )
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.ExUnits (
  ExUnits (..),
  Prices (..),
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as B16L
import qualified Data.ByteString.Lazy as BSL
import Data.Either (fromRight)
import Data.Int
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Sequence.Strict
import GHC.Stack (HasCallStack)
import Lens.Micro
import Paths_cardano_ledger_alonzo_test
import qualified PlutusLedgerApi.V1 as PV1 (Data (..))
import Test.Cardano.Ledger.Mary.Golden (
  largestName,
  minUTxO,
  pid1,
  pid2,
  pid3,
  smallName,
  smallestName,
 )
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Examples.Cast (aliceAddr, bobAddr, carlAddr)
import Test.Cardano.Protocol.TPraos.Examples (
  LedgerExamples (..),
  ProtocolLedgerExamples (..),
  ledgerExamplesAlonzo,
 )
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile

-- | ada cost of storing a word8 of data as a UTxO entry, assuming no change to minUTxOValue
coinsPerUTxOWordLocal :: Integer
coinsPerUTxOWordLocal = quot minUTxOValueShelleyMA utxoEntrySizeWithoutValLocal
  where
    utxoEntrySizeWithoutValLocal = 29
    Coin minUTxOValueShelleyMA = minUTxO

calcMinUTxO :: AlonzoTxOut AlonzoEra -> Coin
calcMinUTxO tout = Coin (utxoEntrySize tout * coinsPerUTxOWordLocal)

tests :: Spec
tests =
  describe "Alonzo Golden Tests" $ do
    goldenCborSerialization
    goldenJsonSerialization
    goldenMinFee
    goldenScriptIntegrity
    goldenGenesisSerialization
    goldenUTxOEntryMinAda

-- | (heapWords of a DataHash) * coinsPerUTxOWordLocal is 344820
goldenUTxOEntryMinAda :: Spec
goldenUTxOEntryMinAda =
  describe "golden tests - UTxOEntryMinAda" $ do
    it "one policy, one (smallest) name, yes datum hash" $
      calcMinUTxO
        ( AlonzoTxOut
            carlAddr
            (valueFromList (Coin 1407406) [(pid1, smallestName, 1)])
            (SJust $ hashData @AlonzoEra (Data (PV1.List [])))
        )
        `shouldBe` Coin 1655136
    it "one policy, one (smallest) name, no datum hash" $
      calcMinUTxO
        ( AlonzoTxOut
            bobAddr
            (valueFromList (Coin 1407406) [(pid1, smallestName, 1)])
            SNothing
        )
        `shouldBe` Coin 1310316
    it "one policy, one (small) name" $
      calcMinUTxO
        ( AlonzoTxOut
            aliceAddr
            (valueFromList (Coin 1444443) [(pid1, smallName 1, 1)])
            SNothing
        )
        `shouldBe` Coin 1344798
    it "one policy, three (small) names" $
      calcMinUTxO
        ( AlonzoTxOut
            aliceAddr
            ( valueFromList
                (Coin 1555554)
                [ (pid1, smallName 1, 1)
                , (pid1, smallName 2, 1)
                , (pid1, smallName 3, 1)
                ]
            )
            SNothing
        )
        `shouldBe` Coin 1448244
    it "one policy, one (largest) name" $
      calcMinUTxO
        ( AlonzoTxOut
            carlAddr
            (valueFromList (Coin 1555554) [(pid1, largestName 65, 1)])
            SNothing
        )
        `shouldBe` Coin 1448244
    it "one policy, three (largest) name, with hash" $
      calcMinUTxO
        ( AlonzoTxOut
            carlAddr
            ( valueFromList
                (Coin 1962961)
                [ (pid1, largestName 65, 1)
                , (pid1, largestName 66, 1)
                , (pid1, largestName 67, 1)
                ]
            )
            (SJust $ hashData @AlonzoEra (Data (PV1.Constr 0 [PV1.Constr 0 []])))
        )
        `shouldBe` Coin 2172366
    it "two policies, one (smallest) name" $
      calcMinUTxO
        ( AlonzoTxOut
            aliceAddr
            (valueFromList (Coin 1592591) [(pid1, smallestName, 1), (pid2, smallestName, 1)])
            SNothing
        )
        `shouldBe` Coin 1482726
    it "two policies, one (smallest) name, with hash" $
      calcMinUTxO
        ( AlonzoTxOut
            aliceAddr
            (valueFromList (Coin 1592591) [(pid1, smallestName, 1), (pid2, smallestName, 1)])
            (SJust $ hashData @AlonzoEra (Data (PV1.Constr 0 [])))
        )
        `shouldBe` Coin 1827546
    it "two policies, two (small) names" $
      calcMinUTxO
        ( AlonzoTxOut
            bobAddr
            (valueFromList (Coin 1629628) [(pid1, smallName 1, 1), (pid2, smallName 2, 1)])
            SNothing
        )
        `shouldBe` Coin 1517208
    it "three policies, ninety-six (small) names" $
      calcMinUTxO
        ( AlonzoTxOut
            aliceAddr
            ( let f i c = (i, smallName c, 1)
               in valueFromList
                    (Coin 7407400)
                    [ f i c
                    | (i, cs) <-
                        [(pid1, [32 .. 63]), (pid2, [64 .. 95]), (pid3, [96 .. 127])]
                    , c <- cs
                    ]
            )
            SNothing
        )
        `shouldBe` Coin 6896400
    it "utxo entry size of ada-only" $
      -- This value, 29, is helpful for comparing the alonzo protocol parameter utxoCostPerWord
      -- with the old parameter minUTxOValue.
      -- If we wish to keep the ada-only, no datum hash, minimum value nearly the same,
      -- we can divide minUTxOValue by 29 and round.
      utxoEntrySize @AlonzoEra (AlonzoTxOut aliceAddr mempty SNothing) `shouldBe` 29

goldenCborSerialization :: Spec
goldenCborSerialization =
  describe "golden tests - CBOR serialization" $ do
    it "Alonzo Block" $ do
      expected <- readDataFile "golden/block.cbor"
      Plain.serialize (pleBlock ledgerExamplesAlonzo) `shouldBe` expected
    it "Alonzo Tx" $ do
      expected <- readDataFile "golden/tx.cbor"
      Plain.serialize (leTx $ pleLedgerExamples ledgerExamplesAlonzo) `shouldBe` expected

goldenJsonSerialization :: Spec
goldenJsonSerialization =
  describe "golden tests - JSON serialization" $ do
    it "ValidityInterval" $ do
      let value =
            [ ValidityInterval
                { invalidBefore = SNothing
                , invalidHereafter = SNothing
                }
            , ValidityInterval
                { invalidBefore = SJust (SlotNo 12345)
                , invalidHereafter = SNothing
                }
            , ValidityInterval
                { invalidBefore = SNothing
                , invalidHereafter = SJust (SlotNo 12354)
                }
            , ValidityInterval
                { invalidBefore = SJust (SlotNo 12345)
                , invalidHereafter = SJust (SlotNo 12354)
                }
            ]
      expected <- Aeson.throwDecode =<< readDataFile "golden/ValidityInterval.json"
      Aeson.toJSON value `shouldBe` expected
    it "IsValid" $ do
      let value =
            [ IsValid True
            , IsValid False
            ]
      expected <- Aeson.throwDecode =<< readDataFile "golden/IsValid.json"
      Aeson.toJSON value `shouldBe` expected
    it "FailureDescription" $ do
      let value =
            [ PlutusFailure "A description" "A reconstruction"
            ]
      expected <- Aeson.throwDecode =<< readDataFile "golden/FailureDescription.json"
      Aeson.toJSON value `shouldBe` expected
    it "TagMismatchDescription" $ do
      let value =
            [ PassedUnexpectedly
            , FailedUnexpectedly (NE.fromList [PlutusFailure "A description" "A reconstruction"])
            ]
      expected <- Aeson.throwDecode =<< readDataFile "golden/TagMismatchDescription.json"
      Aeson.toJSON value `shouldBe` expected

goldenGenesisSerialization :: Spec
goldenGenesisSerialization =
  describe "golden tests - Alonzo Genesis serialization" $ do
    it "JSON deserialization" $ do
      let file = "golden/mainnet-alonzo-genesis.json"
      deserialized <- (eitherDecodeFileStrict file :: IO (Either String AlonzoGenesis))
      deserialized `shouldBe` Right expectedGenesis

goldenMinFee :: Spec
goldenMinFee =
  describe "golden tests - minimum fee calculation" $ do
    it "Alonzo Block" $ do
      -- This golden test uses the block from:
      -- https://github.com/input-output-hk/cardano-node/issues/4228#issuecomment-1195707491
      --
      -- The first transaction in this block is invalid due to:
      --   FeeTooSmallUTxO (Coin 1006053) (Coin 1001829)
      --
      -- The correct behavior is for the minimum fee for this transaction
      -- to be 1006053 lovelace, as indicated by the failure above.
      -- Nodes that had the bug determined the minimum fee to be 1001829.
      hex <- readDataFile "golden/hex-block-node-issue-4228.cbor"
      let cborBytesBlock =
            case B16L.decode hex of
              Left err -> error err
              Right val -> val
          blockBody =
            case decodeFullAnnotator (eraProtVerHigh @AlonzoEra) "Block" decCBOR cborBytesBlock of
              Left err -> error (show err)
              Right (Block _bHeader bBody :: Block (BHeader StandardCrypto) AlonzoEra) -> bBody
          firstTx =
            case blockBody ^. txSeqBlockBodyL of
              tx :<| _ -> (tx :: Tx TopTx AlonzoEra)
              Empty -> error "Block doesn't have any transactions"

          -- Below are the relevant protocol parameters that were active
          -- at the time this block was rejected.
          priceMem = fromJust $ boundRational 0.0577
          priceSteps = fromJust $ boundRational 0.0000721
          pricesParam = Prices priceMem priceSteps
          pp =
            emptyPParams
              & ppMinFeeAL .~ Coin 44
              & ppMinFeeBL .~ Coin 155381
              & ppPricesL .~ pricesParam

      Coin 1006053 `shouldBe` alonzoMinFeeTx pp firstTx

fromRightError :: (HasCallStack, Show a) => String -> Either a b -> b
fromRightError errorMsg =
  either (\e -> error $ errorMsg ++ ": " ++ show e) id

exPP :: PParams AlonzoEra
exPP =
  emptyPParams
    & ppCostModelsL .~ zeroTestingCostModels [PlutusV1, PlutusV2]

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
  HasCallStack =>
  PParams AlonzoEra ->
  Language ->
  LangDepView ->
  Expectation
testScriptIntegritpHash pp lang view = getLanguageView pp lang `shouldBe` view

goldenScriptIntegrity :: Spec
goldenScriptIntegrity =
  describe "golden tests - script integrity hash" $ do
    it "PlutusV1" $ testScriptIntegritpHash exPP PlutusV1 exampleLangDepViewPV1
    it "PlutusV2" $ testScriptIntegritpHash exPP PlutusV2 exampleLangDepViewPV2

expectedGenesis :: AlonzoGenesis
expectedGenesis =
  AlonzoGenesis
    { agCoinsPerUTxOWord = CoinPerWord $ Coin 34482
    , agPrices = Prices (fromJust $ boundRational 0.0577) (fromJust $ boundRational 0.0000721)
    , agPlutusV1CostModel = expectedCostModel
    , agMaxTxExUnits = ExUnits 10000000 10000000000
    , agMaxBlockExUnits = ExUnits 50000000 40000000000
    , agMaxValSize = 5000
    , agCollateralPercentage = 150
    , agMaxCollateralInputs = 3
    , agExtraConfig = AlonzoExtraConfig $ Just expectedCostModels
    }

expectedCostModels :: CostModels
expectedCostModels =
  mkCostModels
    (Map.fromList [(PlutusV2, expectedCostModelV2)])

expectedCostModel :: CostModel
expectedCostModel =
  fromRight
    (error ("Error creating CostModel from known parameters" <> show expectedPParams))
    (mkCostModel PlutusV1 expectedPParams)

expectedCostModelV2 :: CostModel
expectedCostModelV2 =
  fromRight
    (error ("Error creating CostModel from known PlutusV2 parameters" <> show expectedPParams))
    (mkCostModel PlutusV2 (expectedPParams ++ (replicate 9 0)))

expectedPParams :: [Int64]
expectedPParams =
  [ 197209
  , 0
  , 1
  , 1
  , 396231
  , 621
  , 0
  , 1
  , 150000
  , 1000
  , 0
  , 1
  , 150000
  , 32
  , 2477736
  , 29175
  , 4
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 100
  , 100
  , 29773
  , 100
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 1000
  , 0
  , 1
  , 150000
  , 32
  , 150000
  , 1000
  , 0
  , 8
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 150000
  , 1000
  , 0
  , 8
  , 150000
  , 112536
  , 247
  , 1
  , 150000
  , 10000
  , 1
  , 136542
  , 1326
  , 1
  , 1000
  , 150000
  , 1000
  , 1
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 1
  , 1
  , 150000
  , 1
  , 150000
  , 4
  , 103599
  , 248
  , 1
  , 103599
  , 248
  , 1
  , 145276
  , 1366
  , 1
  , 179690
  , 497
  , 1
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 61516
  , 11218
  , 0
  , 1
  , 150000
  , 32
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 2477736
  , 29175
  , 4
  , 0
  , 82363
  , 4
  , 150000
  , 5000
  , 0
  , 1
  , 150000
  , 32
  , 197209
  , 0
  , 1
  , 1
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 3345831
  , 1
  , 1
  ]
