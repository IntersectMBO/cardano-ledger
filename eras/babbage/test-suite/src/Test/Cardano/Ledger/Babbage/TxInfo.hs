{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babbage.TxInfo where

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), dataToBinaryData)
import Cardano.Ledger.Alonzo.TxInfo (
  ExtendedUTxO (..),
  TranslationError (..),
  TxOutSource (..),
  VersionedTxInfo (..),
  txInfo,
 )
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.Core (BabbageEraTxOut (..))
import Cardano.Ledger.Babbage.TxBody (Datum (..))
import qualified Cardano.Ledger.Babbage.TxBody as B
import Cardano.Ledger.Babbage.TxInfo (txInfoInV2, txInfoOutV2)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Stack
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Shelley.Address.Bootstrap (aliceByronAddr)
import Test.Cardano.Ledger.Shelley.Examples.Cast (alicePHK)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

byronAddr :: Addr c
byronAddr = AddrBootstrap (BootstrapAddress aliceByronAddr)

shelleyAddr :: Crypto c => Addr c
shelleyAddr = Addr Testnet alicePHK StakeRefNull

ei :: EpochInfo (Either a)
ei = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

ss :: SystemStart
ss = SystemStart $ posixSecondsToUTCTime 0

-- This input is only a "Byron input" in the sense
-- that we attach it to a Byron output in the UTxO created below.
byronInput :: Crypto c => TxIn c
byronInput = mkTxInPartial genesisId 0

-- This input is only unknown in the sense
-- that it is not present in the UTxO created below.
unknownInput :: Crypto c => TxIn c
unknownInput = mkTxInPartial genesisId 1

byronOutput :: forall era. EraTxOut era => TxOut era
byronOutput = mkBasicTxOut byronAddr (Val.inject $ Coin 1)

shelleyOutput :: forall era. EraTxOut era => TxOut era
shelleyOutput = mkBasicTxOut (shelleyAddr) (Val.inject $ Coin 2)

datumEx :: forall era. Era era => Datum era
datumEx = Datum . dataToBinaryData . Data . PV1.I $ 123

inlineDatumOutput ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  TxOut era
inlineDatumOutput =
  mkBasicTxOut (shelleyAddr) (Val.inject $ Coin 3)
    & datumTxOutL .~ datumEx

type BabbageTxInfoTests era =
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  , Script era ~ AlonzoScript era
  )

refScriptOutput :: forall era. BabbageTxInfoTests era => TxOut era
refScriptOutput =
  mkBasicTxOut (shelleyAddr) (Val.inject $ Coin 3)
    & referenceScriptTxOutL .~ (SJust $ alwaysSucceeds PlutusV2 3)

-- This input is only a "Shelley input" in the sense
-- that we attach it to a Shelley output in the UTxO created below.
shelleyInput :: Crypto c => TxIn c
shelleyInput = mkTxInPartial genesisId 2

inputWithInlineDatum :: Crypto c => TxIn c
inputWithInlineDatum = mkTxInPartial genesisId 3

inputWithRefScript :: Crypto c => TxIn c
inputWithRefScript = mkTxInPartial genesisId 4

utxo :: forall era. BabbageTxInfoTests era => UTxO era
utxo =
  UTxO $
    Map.fromList
      [ (byronInput, byronOutput)
      , (shelleyInput, shelleyOutput)
      , (inputWithInlineDatum, inlineDatumOutput)
      , (inputWithRefScript, refScriptOutput)
      ]

txb ::
  forall era.
  B.BabbageEraTxBody era =>
  TxIn (EraCrypto era) ->
  Maybe (TxIn (EraCrypto era)) ->
  TxOut era ->
  TxBody era
txb i mRefInp o =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton i
    & B.referenceInputsTxBodyL .~ maybe mempty Set.singleton mRefInp
    & outputsTxBodyL .~ StrictSeq.singleton o
    & feeTxBodyL .~ Coin 2

txBare ::
  forall era.
  (EraTx era, B.BabbageEraTxBody era) =>
  TxIn (EraCrypto era) ->
  TxOut era ->
  Tx era
txBare i o = mkBasicTx (txb i Nothing o)

txRefInput :: forall era. (EraTx era, B.BabbageEraTxBody era) => TxIn (EraCrypto era) -> Tx era
txRefInput refInput = mkBasicTx (txb shelleyInput (Just refInput) shelleyOutput)

hasReferenceInput :: VersionedTxInfo -> Bool
hasReferenceInput (TxInfoPV1 _) = False
hasReferenceInput (TxInfoPV2 info) = PV2.txInfoReferenceInputs info /= mempty
hasReferenceInput (TxInfoPV3 info) = PV3.txInfoReferenceInputs info /= mempty

expectOneInput :: PV2.TxInInfo -> VersionedTxInfo -> Bool
expectOneInput _ (TxInfoPV1 _) = False
expectOneInput i (TxInfoPV2 info) = PV2.txInfoInputs info == [i]
expectOneInput i (TxInfoPV3 info) = PV3.txInfoInputs info == [i]

expectOneOutput :: PV2.TxOut -> VersionedTxInfo -> Bool
expectOneOutput _ (TxInfoPV1 _) = False
expectOneOutput o (TxInfoPV2 info) = PV2.txInfoOutputs info == [o]
expectOneOutput o (TxInfoPV3 info) = PV3.txInfoOutputs info == [o]

successfulTranslation ::
  (ExtendedUTxO era, BabbageTxInfoTests era) =>
  Language ->
  Tx era ->
  (VersionedTxInfo -> Bool) ->
  Assertion
successfulTranslation lang tx f =
  case ctx of
    Right info -> assertBool "unexpected transaction info" (f info)
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e
  where
    ctx = txInfo def lang ei ss utxo tx

expectTranslationError ::
  (ExtendedUTxO era, BabbageTxInfoTests era) =>
  Language ->
  Tx era ->
  TranslationError (EraCrypto era) ->
  Assertion
expectTranslationError lang tx expected =
  case ctx of
    Right _ -> assertFailure "This translation was expected to fail, but it succeeded."
    Left e -> e @?= expected
  where
    ctx = txInfo def lang ei ss utxo tx

expectV1TranslationError ::
  (ExtendedUTxO era, BabbageTxInfoTests era) =>
  Tx era ->
  TranslationError (EraCrypto era) ->
  Assertion
expectV1TranslationError = expectTranslationError PlutusV1

errorTranslate :: (HasCallStack, Show a) => String -> Either a b -> b
errorTranslate exampleName =
  either (\err -> error $ exampleName ++ " failed: " ++ show err) id

translatedInputEx1 ::
  forall era.
  BabbageTxInfoTests era =>
  Proxy era ->
  PV2.TxInInfo
translatedInputEx1 _ =
  errorTranslate "translatedInputEx1" $ (txInfoInV2 @era) utxo inputWithInlineDatum

translatedInputEx2 ::
  forall era.
  BabbageTxInfoTests era =>
  Proxy era ->
  PV2.TxInInfo
translatedInputEx2 _ =
  errorTranslate "translatedInputEx2" $ (txInfoInV2 @era) utxo inputWithRefScript

translatedOutputEx1 :: forall era. (BabbageEraTxOut era, Value era ~ MaryValue (EraCrypto era)) => Proxy era -> PV2.TxOut
translatedOutputEx1 _ =
  errorTranslate "translatedOutputEx1" $ (txInfoOutV2 @era) (TxOutFromOutput minBound) inlineDatumOutput

translatedOutputEx2 ::
  forall era.
  BabbageTxInfoTests era =>
  Proxy era ->
  PV2.TxOut
translatedOutputEx2 _ =
  errorTranslate "translatedOutputEx2" $ (txInfoOutV2 @era) (TxOutFromOutput minBound) refScriptOutput

expectLanguageNotSupported ::
  forall era.
  (ExtendedUTxO era, EraTx era, B.BabbageEraTxBody era, BabbageTxInfoTests era) =>
  Proxy era ->
  Language ->
  Assertion
expectLanguageNotSupported _ lang =
  expectTranslationError @era
    lang
    (txBare shelleyInput shelleyOutput)
    (LanguageNotSupported lang)

txInfoTestsV1 ::
  forall era.
  (ExtendedUTxO era, EraTx era, B.BabbageEraTxBody era, BabbageTxInfoTests era) =>
  Proxy era ->
  TestTree
txInfoTestsV1 _ =
  testGroup
    "Plutus V1"
    [ testCase "translation error on byron txout" $
        expectV1TranslationError @era
          (txBare shelleyInput byronOutput)
          (ByronTxOutInContext (TxOutFromOutput minBound))
    , testCase "translation error on byron txin" $
        expectV1TranslationError @era
          (txBare byronInput shelleyOutput)
          (ByronTxOutInContext (TxOutFromInput byronInput))
    , testCase "translation error on unknown txin (logic error)" $
        expectV1TranslationError @era
          (txBare unknownInput shelleyOutput)
          (TranslationLogicMissingInput unknownInput)
    , testCase "translation error on reference input" $
        expectV1TranslationError @era
          (txRefInput shelleyInput)
          (ReferenceInputsNotSupported (Set.singleton shelleyInput))
    , testCase "translation error on inline datum in input" $
        expectV1TranslationError @era
          (txBare inputWithInlineDatum shelleyOutput)
          (InlineDatumsNotSupported (TxOutFromInput inputWithInlineDatum))
    , testCase "translation error on inline datum in output" $
        expectV1TranslationError @era
          (txBare shelleyInput inlineDatumOutput)
          (InlineDatumsNotSupported (TxOutFromOutput minBound))
    , testCase "translation error on reference script in input" $
        expectV1TranslationError @era
          (txBare inputWithRefScript shelleyOutput)
          (ReferenceScriptsNotSupported (TxOutFromInput inputWithRefScript))
    , testCase "translation error on reference script in output" $
        expectV1TranslationError @era
          (txBare shelleyInput refScriptOutput)
          (ReferenceScriptsNotSupported (TxOutFromOutput minBound))
    ]

txInfoTestsV2 ::
  forall era.
  (ExtendedUTxO era, EraTx era, B.BabbageEraTxBody era, BabbageTxInfoTests era) =>
  Proxy era ->
  Language -> -- Conway V2 script context semantics are the same as V2
  TestTree
txInfoTestsV2 p lang =
  testGroup
    (show lang)
    [ testCase "translation error on byron txout" $
        expectTranslationError @era
          lang
          (txBare shelleyInput byronOutput)
          (ByronTxOutInContext (TxOutFromOutput minBound))
    , testCase "translation error on byron txin" $
        expectTranslationError @era
          lang
          (txBare byronInput shelleyOutput)
          (ByronTxOutInContext (TxOutFromInput byronInput))
    , testCase "translation error on unknown txin (logic error)" $
        expectTranslationError @era
          lang
          (txBare unknownInput shelleyOutput)
          (TranslationLogicMissingInput unknownInput)
    , testCase "use reference input starting in Babbage" $
        successfulTranslation @era
          lang
          (txRefInput shelleyInput)
          hasReferenceInput
    , testCase "use inline datum in input" $
        successfulTranslation @era
          lang
          (txBare inputWithInlineDatum shelleyOutput)
          (expectOneInput (translatedInputEx1 p))
    , testCase "use inline datum in output" $
        successfulTranslation @era
          lang
          (txBare shelleyInput inlineDatumOutput)
          (expectOneOutput (translatedOutputEx1 p))
    , testCase "use reference script in input" $
        successfulTranslation @era
          lang
          (txBare inputWithRefScript shelleyOutput)
          (expectOneInput (translatedInputEx2 p))
    , testCase "use reference script in output" $
        successfulTranslation @era
          lang
          (txBare shelleyInput refScriptOutput)
          (expectOneOutput (translatedOutputEx2 p))
    ]

txInfoTests ::
  forall era.
  (ExtendedUTxO era, EraTx era, B.BabbageEraTxBody era, BabbageTxInfoTests era) =>
  Proxy era ->
  TestTree
txInfoTests p = testGroup "txInfo translation" [txInfoTestsV1 p, txInfoTestsV2 p PlutusV2]

txInfoTestsBabbageOnly :: TestTree
txInfoTestsBabbageOnly =
  testGroup
    "Plutus VX, X > 2"
    [ testCase "translation error for V3 in Babbage" (expectLanguageNotSupported (Proxy @Babbage) PlutusV3)
    ]
