{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babbage.TxInfoSpec (txInfoSpec, spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInInfo,
  PlutusTxInfo,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..), TxOutSource (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxInfo (
  BabbageContextError (..),
  transTxOutV2,
 )
import Cardano.Ledger.BaseTypes (
  Inject (..),
  Network (..),
  ProtVer (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), dataToBinaryData)
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..), plutusLanguage)
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Stack
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkCredential, mkKeyPair)
import Test.Cardano.Ledger.Shelley.Examples (exampleByronAddress)

shelleyAddr :: Addr
shelleyAddr = Addr Testnet pk StakeRefNull
  where
    pk = mkCredential (mkKeyPair 0 :: KeyPair Payment)

ei :: EpochInfo (Either a)
ei = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

ss :: SystemStart
ss = SystemStart $ posixSecondsToUTCTime 0

-- This input is only a "Byron input" in the sense
-- that we attach it to a Byron output in the UTxO created below.
byronInput :: TxIn
byronInput = mkTxInPartial genesisId 0

-- This input is only unknown in the sense
-- that it is not present in the UTxO created below.
unknownInput :: TxIn
unknownInput = mkTxInPartial genesisId 1

byronOutput :: forall era. EraTxOut era => TxOut era
byronOutput = mkBasicTxOut exampleByronAddress (inject $ Coin 1)

shelleyOutput :: forall era. EraTxOut era => TxOut era
shelleyOutput = mkBasicTxOut shelleyAddr (inject $ Coin 2)

datumEx :: forall era. Era era => Datum era
datumEx = Datum . dataToBinaryData . Data . PV1.I $ 123

inlineDatumOutput ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  ) =>
  TxOut era
inlineDatumOutput =
  mkBasicTxOut shelleyAddr (inject $ Coin 3)
    & datumTxOutL .~ datumEx

refScriptOutput :: (BabbageEraTxOut era, EraPlutusTxInfo 'PlutusV2 era) => TxOut era
refScriptOutput =
  mkBasicTxOut shelleyAddr (inject $ Coin 3)
    & referenceScriptTxOutL .~ (SJust $ alwaysSucceeds @'PlutusV2 3)

-- This input is only a "Shelley input" in the sense
-- that we attach it to a Shelley output in the UTxO created below.
shelleyInput :: TxIn
shelleyInput = mkTxInPartial genesisId 2

inputWithInlineDatum :: TxIn
inputWithInlineDatum = mkTxInPartial genesisId 3

inputWithRefScript :: TxIn
inputWithRefScript = mkTxInPartial genesisId 4

exampleUTxO ::
  ( BabbageEraTxOut era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  ) =>
  UTxO era
exampleUTxO =
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
  TxIn ->
  Maybe TxIn ->
  TxOut era ->
  TxBody TopTx era
txb i mRefInp o =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton i
    & referenceInputsTxBodyL .~ maybe mempty Set.singleton mRefInp
    & outputsTxBodyL .~ StrictSeq.singleton o
    & feeTxBodyL .~ Coin 2

txBare ::
  forall era.
  (EraTx era, BabbageEraTxBody era) =>
  TxIn ->
  TxOut era ->
  Tx TopTx era
txBare i o = mkBasicTx (txb i Nothing o)

txRefInput :: forall era. (EraTx era, BabbageEraTxBody era) => TxIn -> Tx TopTx era
txRefInput refInput = mkBasicTx (txb shelleyInput (Just refInput) shelleyOutput)

hasReferenceInput :: SLanguage l -> PlutusTxInfo l -> Expectation
hasReferenceInput slang txInfo =
  case slang of
    SPlutusV1 -> expectationFailure "PlutusV1 does not have reference inputs"
    SPlutusV2 -> PV2.txInfoReferenceInputs txInfo `shouldNotBe` mempty
    SPlutusV3 -> PV3.txInfoReferenceInputs txInfo `shouldNotBe` mempty
    SPlutusV4 -> PV3.txInfoReferenceInputs txInfo `shouldNotBe` mempty

plutusTxInInfoInputs ::
  forall era l. HasCallStack => SLanguage l -> PlutusTxInfo l -> [PlutusTxInInfo era l]
plutusTxInInfoInputs slang txInfo =
  case slang of
    SPlutusV1 -> error "PlutusV1 not supported"
    SPlutusV2 -> PV2.txInfoInputs txInfo
    SPlutusV3 -> PV3.txInfoInputs txInfo
    SPlutusV4 -> PV3.txInfoInputs txInfo

expectOneInput ::
  forall era l.
  ( HasCallStack
  , Show (PlutusTxInInfo era l)
  , Eq (PlutusTxInInfo era l)
  ) =>
  SLanguage l ->
  PlutusTxInInfo era l ->
  PlutusTxInfo l ->
  Expectation
expectOneInput l i txInfo = plutusTxInInfoInputs @era l txInfo `shouldBe` [i]

expectOneOutput :: PV2.TxOut -> SLanguage l -> PlutusTxInfo l -> Expectation
expectOneOutput o slang txInfo =
  case slang of
    SPlutusV1 -> expectationFailure "PlutusV1 not supported"
    SPlutusV2 -> PV2.txInfoOutputs txInfo `shouldBe` [o]
    SPlutusV3 -> PV3.txInfoOutputs txInfo `shouldBe` [o]
    SPlutusV4 -> PV3.txInfoOutputs txInfo `shouldBe` [o]

successfulTranslation ::
  forall era l.
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  ) =>
  SLanguage l ->
  Tx TopTx era ->
  (SLanguage l -> PlutusTxInfo l -> Expectation) ->
  Expectation
successfulTranslation slang tx f =
  let lti =
        LedgerTxInfo
          { ltiProtVer = ProtVer (eraProtVerLow @era) 0
          , ltiEpochInfo = ei
          , ltiSystemStart = ss
          , ltiUTxO = exampleUTxO
          , ltiTx = tx
          }
   in case toPlutusTxInfo slang lti of
        Right txInfo -> f slang txInfo
        Left e -> assertFailure $ "no translation error was expected, but got: " <> show e

expectTranslationError ::
  forall era l.
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  ) =>
  SLanguage l ->
  Tx TopTx era ->
  ContextError era ->
  Expectation
expectTranslationError slang tx expected =
  let lti =
        LedgerTxInfo
          { ltiProtVer = ProtVer (eraProtVerLow @era) 0
          , ltiEpochInfo = ei
          , ltiSystemStart = ss
          , ltiUTxO = exampleUTxO
          , ltiTx = tx
          }
   in case toPlutusTxInfo slang lti of
        Right _ -> assertFailure "This translation was expected to fail, but it succeeded."
        Left e -> e `shouldBe` expected

expectV1TranslationError ::
  ( BabbageEraTxOut era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era ->
  ContextError era ->
  Expectation
expectV1TranslationError = expectTranslationError SPlutusV1

errorTranslate ::
  forall era b.
  (HasCallStack, Show (ContextError era)) =>
  String ->
  Either (ContextError era) b ->
  b
errorTranslate exampleName =
  either (\err -> error $ exampleName ++ " failed: " ++ show err) id

translatedOutputEx1 ::
  forall era.
  ( BabbageEraTxOut era
  , Show (ContextError era)
  , Value era ~ MaryValue
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  PV2.TxOut
translatedOutputEx1 =
  errorTranslate @era "translatedOutputEx1" $
    transTxOutV2 @era (TxOutFromOutput minBound) inlineDatumOutput

translatedOutputEx2 ::
  forall era.
  ( BabbageEraTxOut era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  PV2.TxOut
translatedOutputEx2 =
  errorTranslate @era "translatedOutputEx2" $
    transTxOutV2 @era (TxOutFromOutput minBound) refScriptOutput

txInfoSpecV1 ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Spec
txInfoSpecV1 =
  describe "Plutus V1" $ do
    it "translation error on byron txout" $
      expectV1TranslationError @era
        (txBare shelleyInput byronOutput)
        (inject $ ByronTxOutInContext @era (TxOutFromOutput minBound))
    it "translation error on byron txin" $
      expectV1TranslationError @era
        (txBare byronInput shelleyOutput)
        (inject $ ByronTxOutInContext @era (TxOutFromInput byronInput))
    it "translation error on unknown txin (logic error)" $
      expectV1TranslationError @era
        (txBare unknownInput shelleyOutput)
        (inject $ AlonzoContextError $ TranslationLogicMissingInput @era unknownInput)
    it "translation error on inline datum in input" $
      expectV1TranslationError @era
        (txBare inputWithInlineDatum shelleyOutput)
        (inject $ InlineDatumsNotSupported @era (TxOutFromInput inputWithInlineDatum))
    it "translation error on inline datum in output" $
      expectV1TranslationError @era
        (txBare shelleyInput inlineDatumOutput)
        (inject $ InlineDatumsNotSupported @era (TxOutFromOutput minBound))

txInfoSpec ::
  forall era l.
  ( EraTx era
  , EraPlutusTxInfo l era
  , EraPlutusTxInfo 'PlutusV2 era
  , BabbageEraTxBody era
  , Value era ~ MaryValue
  , Inject (BabbageContextError era) (ContextError era)
  , Show (PlutusTxInInfo era l)
  , Eq (PlutusTxInInfo era l)
  ) =>
  SLanguage l ->
  Spec
txInfoSpec lang =
  describe (show lang) $ do
    it "translation error on byron txout" $
      expectTranslationError @era
        lang
        (txBare shelleyInput byronOutput)
        (inject $ ByronTxOutInContext @era (TxOutFromOutput minBound))
    it "translation error on byron txin" $
      expectTranslationError @era
        lang
        (txBare byronInput shelleyOutput)
        (inject $ ByronTxOutInContext @era (TxOutFromInput byronInput))
    it "translation error on unknown txin (logic error)" $
      expectTranslationError @era
        lang
        (txBare unknownInput shelleyOutput)
        (inject $ AlonzoContextError $ TranslationLogicMissingInput @era unknownInput)
    -- This test will fail in PlutusV3 because of ReferenceInputsNotDisjointFromInputs
    when (plutusLanguage lang == PlutusV2) $
      it "use reference input starting in Babbage" $
        successfulTranslation @era
          lang
          (txRefInput shelleyInput)
          hasReferenceInput
    it "use inline datum in input" $
      successfulTranslation @era
        lang
        (txBare inputWithInlineDatum shelleyOutput)
        ( \l txInfo -> do
            txInInfo <- expectRight $ toPlutusTxInInfo @_ @era l exampleUTxO inputWithInlineDatum
            expectOneInput @era l txInInfo txInfo
        )
    it "use inline datum in output" $
      successfulTranslation @era
        lang
        (txBare shelleyInput inlineDatumOutput)
        (expectOneOutput (translatedOutputEx1 @era))
    it "use reference script in input" $
      successfulTranslation @era
        lang
        (txBare inputWithRefScript shelleyOutput)
        ( \l txInfo -> do
            txInInfo <- expectRight $ toPlutusTxInInfo @_ @era l exampleUTxO inputWithRefScript
            expectOneInput @era l txInInfo txInfo
        )
    it "use reference script in output" $
      successfulTranslation @era
        lang
        (txBare shelleyInput refScriptOutput)
        (expectOneOutput (translatedOutputEx2 @era))

spec ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  , Value era ~ MaryValue
  , Inject (BabbageContextError era) (ContextError era)
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  ) =>
  Spec
spec =
  describe "txInfo translation" $ do
    txInfoSpecV1 @era
    txInfoSpec @era SPlutusV2

genesisId :: TxId
genesisId = TxId (unsafeMakeSafeHash (mkDummyHash (0 :: Int)))
