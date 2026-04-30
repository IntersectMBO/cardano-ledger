{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.GoldenSpec (spec) where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (
  EraTxOut (..),
  PParams,
  dataHashTxOutL,
  emptyPParams,
  mkBasicTxOut,
  ppCostModelsL,
 )
import Cardano.Ledger.Alonzo.PParams (LangDepView (..), getLanguageView)
import Cardano.Ledger.Alonzo.TxBody (utxoEntrySize)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..), valueFromList)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.Slot (SlotNo (..))
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SBS
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word8)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import qualified Test.Cardano.Ledger.Alonzo.Binary.GoldenSpec as BinaryGoldenSpec
import qualified Test.Cardano.Ledger.Alonzo.JSON.GoldenSpec as JsonGoldenSpec
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkKeyPair)
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

spec :: Spec
spec =
  describe "Golden" $ do
    describe "JSON" JsonGoldenSpec.spec
    describe "CBOR" BinaryGoldenSpec.spec
    goldenScriptIntegrity
    goldenUTxOEntryMinAda

goldenScriptIntegrity :: Spec
goldenScriptIntegrity =
  describe "Script integrity hash" $ do
    it "PlutusV1" $ testScriptIntegrityHash exPP PlutusV1 exampleLangDepViewPV1
    it "PlutusV2" $ testScriptIntegrityHash exPP PlutusV2 exampleLangDepViewPV2
  where
    testScriptIntegrityHash ::
      HasCallStack =>
      PParams AlonzoEra ->
      Language ->
      LangDepView ->
      Expectation
    testScriptIntegrityHash pp lang view = getLanguageView pp lang `shouldBe` view

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

    exPP :: PParams AlonzoEra
    exPP =
      emptyPParams
        & ppCostModelsL .~ zeroTestingCostModels [PlutusV1, PlutusV2]

    fromRightError :: (HasCallStack, Show a) => String -> Either a b -> b
    fromRightError errorMsg =
      either (\e -> error $ errorMsg ++ ": " ++ show e) id

-- | (heapWords of a DataHash) * coinsPerUTxOWordLocal is 344820
goldenUTxOEntryMinAda :: Spec
goldenUTxOEntryMinAda =
  describe "golden tests - UTxOEntryMinAda" $ do
    it "one policy, one (smallest) name, yes datum hash" $
      calcMinUTxO
        ( mkBasicTxOut carlAddr (valueFromList (Coin 1407406) [(pid1, smallestName, 1)])
            & dataHashTxOutL .~ SJust (hashData @AlonzoEra (Data (PV1.List [])))
        )
        `shouldBe` Coin 1655136
    it "one policy, one (smallest) name, no datum hash" $
      calcMinUTxO
        (mkBasicTxOut bobAddr (valueFromList (Coin 1407406) [(pid1, smallestName, 1)]))
        `shouldBe` Coin 1310316
    it "one policy, one (small) name" $
      calcMinUTxO
        (mkBasicTxOut aliceAddr (valueFromList (Coin 1444443) [(pid1, smallName 1, 1)]))
        `shouldBe` Coin 1344798
    it "one policy, three (small) names" $
      calcMinUTxO
        ( mkBasicTxOut
            aliceAddr
            ( valueFromList
                (Coin 1555554)
                [ (pid1, smallName 1, 1)
                , (pid1, smallName 2, 1)
                , (pid1, smallName 3, 1)
                ]
            )
        )
        `shouldBe` Coin 1448244
    it "one policy, one (largest) name" $
      calcMinUTxO
        (mkBasicTxOut carlAddr (valueFromList (Coin 1555554) [(pid1, largestName 65, 1)]))
        `shouldBe` Coin 1448244
    it "one policy, three (largest) name, with hash" $
      calcMinUTxO
        ( mkBasicTxOut
            carlAddr
            ( valueFromList
                (Coin 1962961)
                [ (pid1, largestName 65, 1)
                , (pid1, largestName 66, 1)
                , (pid1, largestName 67, 1)
                ]
            )
            & dataHashTxOutL .~ SJust (hashData @AlonzoEra (Data (PV1.Constr 0 [PV1.Constr 0 []])))
        )
        `shouldBe` Coin 2172366
    it "two policies, one (smallest) name" $
      calcMinUTxO
        ( mkBasicTxOut
            aliceAddr
            (valueFromList (Coin 1592591) [(pid1, smallestName, 1), (pid2, smallestName, 1)])
        )
        `shouldBe` Coin 1482726
    it "two policies, one (smallest) name, with hash" $
      calcMinUTxO
        ( mkBasicTxOut
            aliceAddr
            (valueFromList (Coin 1592591) [(pid1, smallestName, 1), (pid2, smallestName, 1)])
            & dataHashTxOutL .~ SJust (hashData @AlonzoEra (Data (PV1.Constr 0 [])))
        )
        `shouldBe` Coin 1827546
    it "two policies, two (small) names" $
      calcMinUTxO
        (mkBasicTxOut bobAddr (valueFromList (Coin 1629628) [(pid1, smallName 1, 1), (pid2, smallName 2, 1)]))
        `shouldBe` Coin 1517208
    it "three policies, ninety-six (small) names" $
      calcMinUTxO
        ( mkBasicTxOut
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
        )
        `shouldBe` Coin 6896400
    it "utxo entry size of ada-only" $
      -- This value, 29, is helpful for comparing the alonzo protocol parameter utxoCostPerWord
      -- with the old parameter minUTxOValue.
      -- If we wish to keep the ada-only, no datum hash, minimum value nearly the same,
      -- we can divide minUTxOValue by 29 and round.
      utxoEntrySize @AlonzoEra (mkBasicTxOut aliceAddr mempty) `shouldBe` 29
  where
    calcMinUTxO :: TxOut AlonzoEra -> Coin
    calcMinUTxO tout = Coin (utxoEntrySize tout * coinsPerUTxOWordLocal)

    -- \| ada cost of storing a word8 of data as a UTxO entry, assuming no change to minUTxOValue
    coinsPerUTxOWordLocal :: Integer
    coinsPerUTxOWordLocal = quot minUTxOValueShelleyMA utxoEntrySizeWithoutValLocal
      where
        utxoEntrySizeWithoutValLocal = 29
        Coin minUTxOValueShelleyMA = minUTxO

-- Helpers inlined from Test.Cardano.Ledger.Mary.Golden
-- (cardano-ledger-shelley-ma-test cannot be a dep due to cyclic dependency via cardano-protocol-tpraos)

pid1 :: PolicyID
pid1 =
  PolicyID $
    hashScript @MaryEra $
      RequireAllOf (StrictSeq.fromList [])

pid2 :: PolicyID
pid2 =
  PolicyID $
    hashScript @MaryEra $
      RequireAllOf (StrictSeq.fromList [RequireTimeStart (SlotNo 1)])

pid3 :: PolicyID
pid3 =
  PolicyID $
    hashScript @MaryEra $
      RequireAllOf (StrictSeq.fromList [RequireTimeExpire (SlotNo 1)])

smallestName :: AssetName
smallestName = AssetName $ SBS.pack []

smallName :: Word8 -> AssetName
smallName c = AssetName $ SBS.pack [c]

largestName :: Word8 -> AssetName
largestName c = AssetName . SBS.pack $ c : [1 .. 31]

minUTxO :: Coin
minUTxO = Coin $ 1000 * 1000

-- Addresses inlined from Test.Cardano.Ledger.Shelley.Examples.Cast
-- (cardano-ledger-shelley-test cannot be a dep due to cyclic dependency via cardano-protocol-tpraos)

aliceAddr :: Addr
aliceAddr = mkAddr (mkKeyPair @Payment 0) (mkKeyPair @Staking 1)

bobAddr :: Addr
bobAddr = mkAddr (mkKeyPair @Payment 2) (mkKeyPair @Staking 3)

carlAddr :: Addr
carlAddr = mkAddr (mkKeyPair @Payment 4) (mkKeyPair @Staking 5)
