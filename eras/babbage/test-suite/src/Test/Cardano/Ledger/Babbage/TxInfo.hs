{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babbage.TxInfo where

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusTxInfo (toPlutusTxInfo),
  PlutusTxInfo,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..), TxOutSource (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..), transTxInInfoV2, transTxOutV2)
import Cardano.Ledger.BaseTypes (Inject (..), Network (..), StrictMaybe (..), natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), dataToBinaryData)
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO (UTxO (..))
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
import qualified PlutusLedgerApi.V4 as PV4
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
byronOutput = mkBasicTxOut byronAddr (inject $ Coin 1)

shelleyOutput :: forall era. EraTxOut era => TxOut era
shelleyOutput = mkBasicTxOut (shelleyAddr) (inject $ Coin 2)

datumEx :: forall era. Era era => Datum era
datumEx = Datum . dataToBinaryData . Data . PV1.I $ 123

inlineDatumOutput ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  TxOut era
inlineDatumOutput =
  mkBasicTxOut (shelleyAddr) (inject $ Coin 3)
    & datumTxOutL
    .~ datumEx

refScriptOutput :: BabbageEraTxOut era => TxOut era
refScriptOutput =
  mkBasicTxOut (shelleyAddr) (inject $ Coin 3)
    & referenceScriptTxOutL
    .~ (SJust $ alwaysSucceeds @'PlutusV2 3)

-- This input is only a "Shelley input" in the sense
-- that we attach it to a Shelley output in the UTxO created below.
shelleyInput :: Crypto c => TxIn c
shelleyInput = mkTxInPartial genesisId 2

inputWithInlineDatum :: Crypto c => TxIn c
inputWithInlineDatum = mkTxInPartial genesisId 3

inputWithRefScript :: Crypto c => TxIn c
inputWithRefScript = mkTxInPartial genesisId 4

utxo ::
  ( BabbageEraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  UTxO era
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
  BabbageEraTxBody era =>
  TxIn (EraCrypto era) ->
  Maybe (TxIn (EraCrypto era)) ->
  TxOut era ->
  TxBody era
txb i mRefInp o =
  mkBasicTxBody
    & inputsTxBodyL
    .~ Set.singleton i
    & referenceInputsTxBodyL
    .~ maybe mempty Set.singleton mRefInp
    & outputsTxBodyL
    .~ StrictSeq.singleton o
    & feeTxBodyL
    .~ Coin 2

txBare ::
  forall era.
  (EraTx era, BabbageEraTxBody era) =>
  TxIn (EraCrypto era) ->
  TxOut era ->
  Tx era
txBare i o = mkBasicTx (txb i Nothing o)

txRefInput :: forall era. (EraTx era, BabbageEraTxBody era) => TxIn (EraCrypto era) -> Tx era
txRefInput refInput = mkBasicTx (txb shelleyInput (Just refInput) shelleyOutput)

hasReferenceInput :: SLanguage l -> PlutusTxInfo l -> Bool
hasReferenceInput slang txInfo =
  case slang of
    SPlutusV1 -> False
    SPlutusV2 -> PV2.txInfoReferenceInputs txInfo /= mempty
    SPlutusV3 -> PV3.txInfoReferenceInputs txInfo /= mempty
    SPlutusV4 -> PV4.txInfoReferenceInputs txInfo /= mempty

expectOneInput :: PV2.TxInInfo -> SLanguage l -> PlutusTxInfo l -> Bool
expectOneInput i slang txInfo =
  case slang of
    SPlutusV1 -> False
    SPlutusV2 -> PV2.txInfoInputs txInfo == [i]
    SPlutusV3 -> False
    SPlutusV4 -> False

expectOneOutput :: PV2.TxOut -> SLanguage l -> PlutusTxInfo l -> Bool
expectOneOutput o slang txInfo =
  case slang of
    SPlutusV1 -> False
    SPlutusV2 -> PV2.txInfoOutputs txInfo == [o]
    SPlutusV3 -> PV3.txInfoOutputs txInfo == [o]
    SPlutusV4 -> PV4.txInfoOutputs txInfo == [o]

successfulTranslation ::
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  SLanguage l ->
  Tx era ->
  (SLanguage l -> PlutusTxInfo l -> Bool) ->
  Assertion
successfulTranslation slang tx f =
  case toPlutusTxInfo slang def ei ss utxo tx of
    Right txInfo -> assertBool "unexpected transaction info" (f slang txInfo)
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e

expectTranslationError ::
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  SLanguage l ->
  Tx era ->
  ContextError era ->
  Assertion
expectTranslationError slang tx expected =
  case toPlutusTxInfo slang def ei ss utxo tx of
    Right _ -> assertFailure "This translation was expected to fail, but it succeeded."
    Left e -> e @?= expected

expectV1TranslationError ::
  ( BabbageEraTxOut era
  , EraPlutusTxInfo 'PlutusV1 era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Tx era ->
  ContextError era ->
  Assertion
expectV1TranslationError = expectTranslationError SPlutusV1

errorTranslate ::
  forall era b.
  (HasCallStack, Show (ContextError era)) =>
  String ->
  Either (ContextError era) b ->
  b
errorTranslate exampleName =
  either (\err -> error $ exampleName ++ " failed: " ++ show err) id

translatedInputEx1 ::
  forall era.
  ( BabbageEraTxOut era
  , Show (ContextError era)
  , Value era ~ MaryValue (EraCrypto era)
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Proxy era ->
  PV2.TxInInfo
translatedInputEx1 _ =
  errorTranslate @era "translatedInputEx1" $ transTxInInfoV2 @era utxo inputWithInlineDatum

translatedInputEx2 ::
  forall era.
  ( BabbageEraTxOut era
  , Show (ContextError era)
  , Value era ~ MaryValue (EraCrypto era)
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Proxy era ->
  PV2.TxInInfo
translatedInputEx2 _ =
  errorTranslate @era "translatedInputEx2" $ transTxInInfoV2 @era utxo inputWithRefScript

translatedOutputEx1 ::
  forall era.
  ( BabbageEraTxOut era
  , Show (ContextError era)
  , Value era ~ MaryValue (EraCrypto era)
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Proxy era ->
  PV2.TxOut
translatedOutputEx1 _ =
  errorTranslate @era "translatedOutputEx1" $
    transTxOutV2 @era (TxOutFromOutput minBound) inlineDatumOutput

translatedOutputEx2 ::
  forall era.
  ( BabbageEraTxOut era
  , Show (ContextError era)
  , Value era ~ MaryValue (EraCrypto era)
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Proxy era ->
  PV2.TxOut
translatedOutputEx2 _ =
  errorTranslate @era "translatedOutputEx2" $
    transTxOutV2 @era (TxOutFromOutput minBound) refScriptOutput

txInfoTestsV1 ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , EraPlutusTxInfo 'PlutusV1 era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Proxy era ->
  TestTree
txInfoTestsV1 _ =
  testGroup
    "Plutus V1"
    $ [ testCase "translation error on byron txout" $
          expectV1TranslationError @era
            (txBare shelleyInput byronOutput)
            (inject $ ByronTxOutInContext @era (TxOutFromOutput minBound))
      , testCase "translation error on byron txin" $
          expectV1TranslationError @era
            (txBare byronInput shelleyOutput)
            (inject $ ByronTxOutInContext @era (TxOutFromInput byronInput))
      , testCase "translation error on unknown txin (logic error)" $
          expectV1TranslationError @era
            (txBare unknownInput shelleyOutput)
            (inject $ AlonzoContextError $ TranslationLogicMissingInput @era unknownInput)
      , testCase "translation error on inline datum in input" $
          expectV1TranslationError @era
            (txBare inputWithInlineDatum shelleyOutput)
            (inject $ InlineDatumsNotSupported @era (TxOutFromInput inputWithInlineDatum))
      , testCase "translation error on inline datum in output" $
          expectV1TranslationError @era
            (txBare shelleyInput inlineDatumOutput)
            (inject $ InlineDatumsNotSupported @era (TxOutFromOutput minBound))
      ]
      ++ if eraProtVerLow @era < natVersion @9
        then
          [ testCase "translation error on reference script in input" $
              expectV1TranslationError @era
                (txBare inputWithRefScript shelleyOutput)
                (inject $ ReferenceScriptsNotSupported @era (TxOutFromInput inputWithRefScript))
          , testCase "translation error on reference script in output" $
              expectV1TranslationError @era
                (txBare shelleyInput refScriptOutput)
                (inject $ ReferenceScriptsNotSupported @era (TxOutFromOutput minBound))
          , testCase "translation error on reference input" $
              expectV1TranslationError @era
                (txRefInput shelleyInput)
                (inject $ ReferenceInputsNotSupported @era (Set.singleton shelleyInput))
          ]
        else []

txInfoTestsV2 ::
  forall era l.
  ( EraTx era
  , EraPlutusTxInfo l era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Proxy era ->
  SLanguage l ->
  TestTree
txInfoTestsV2 p lang =
  testGroup
    (show lang)
    [ testCase "translation error on byron txout" $
        expectTranslationError @era
          lang
          (txBare shelleyInput byronOutput)
          (inject $ ByronTxOutInContext @era (TxOutFromOutput minBound))
    , testCase "translation error on byron txin" $
        expectTranslationError @era
          lang
          (txBare byronInput shelleyOutput)
          (inject $ ByronTxOutInContext @era (TxOutFromInput byronInput))
    , testCase "translation error on unknown txin (logic error)" $
        expectTranslationError @era
          lang
          (txBare unknownInput shelleyOutput)
          (inject $ AlonzoContextError $ TranslationLogicMissingInput @era unknownInput)
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
  ( EraTx era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , Inject (BabbageContextError era) (ContextError era)
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  ) =>
  Proxy era ->
  TestTree
txInfoTests p = testGroup "txInfo translation" [txInfoTestsV1 p, txInfoTestsV2 p SPlutusV2]
