{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Orphan instances for 'EraPlutusTxOut (V2/V3/V4) that require
-- 'BabbageEraTxOut', which is unavailable in the Alonzo testlib.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.TxInfoSpec
  ( txInfoV1Spec
  , txInfoSpec
  , spec
  , -- ** Assertions
  expectReferenceInputs,
  ) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInInfo,
  PlutusTxInfo,
  toPlutusTxInfoForPurpose,
 )
import Cardano.Ledger.Alonzo.TxWits (TxDats)
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  AlonzoContextError (..),
  TxOutSource (..),
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo (transValidityInterval)
import Data.Proxy (Proxy (..))
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..))
import Cardano.Ledger.Babbage (BabbageEra)
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
import qualified PlutusLedgerApi.V3 as PV4
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import qualified Test.Cardano.Ledger.Alonzo.TxInfoSpec as AlonzoTxInfoSpec
import Test.Cardano.Ledger.Alonzo.TxInfoSpec (EraPlutusTxOut (..), EraTranslateValidityInterval (..))
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkCredential, mkKeyPair)
import Test.Cardano.Ledger.Era ()
import Test.Cardano.Ledger.Shelley.Examples (exampleByronAddress)

-- ---------------------------------------------------------------------------
-- Orphan instances for Babbage era.
-- ---------------------------------------------------------------------------

-- | Babbage uses the same validity interval translation as Alonzo.
instance EraTranslateValidityInterval BabbageEra where
  translateVI = Alonzo.transValidityInterval (Proxy @BabbageEra)

-- ---------------------------------------------------------------------------
-- Orphan instances: 'EraPlutusTxOut' for PlutusV2/V3/V4.
--
-- These cannot live in the Alonzo testlib because they require 'BabbageEraTxOut'
-- and 'transTxOutV2', which are only available in the Babbage package.
-- ---------------------------------------------------------------------------

instance
  ( Value era ~ MaryValue
  , BabbageEraTxOut era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  EraPlutusTxOut 'PlutusV2 era
  where
  toPlutusTxOut _ txOut =
    either (const Nothing) Just $ transTxOutV2 @era (TxOutFromOutput minBound) txOut

instance
  ( Value era ~ MaryValue
  , BabbageEraTxOut era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  EraPlutusTxOut 'PlutusV3 era
  where
  toPlutusTxOut _ txOut =
    either (const Nothing) Just $ transTxOutV2 @era (TxOutFromOutput minBound) txOut

instance
  ( Value era ~ MaryValue
  , BabbageEraTxOut era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  EraPlutusTxOut 'PlutusV4 era
  where
  toPlutusTxOut _ txOut =
    either (const Nothing) Just $ transTxOutV2 @era (TxOutFromOutput minBound) txOut

spec ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  , Value era ~ MaryValue
  , Inject (BabbageContextError era) (ContextError era)
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , EraTranslateValidityInterval era
  , AtMostEra "Conway" era
  , AlonzoEraTxWits era
  , Inject (AlonzoContextError era) (ContextError era)
  , Arbitrary (TxCert era)
  ) =>
  Spec
spec =
  describe "txInfo translation" $ do
    txInfoV1Spec @era
    AlonzoTxInfoSpec.txInfoSpec @era SPlutusV1
    AlonzoTxInfoSpec.txInfoSpec @era SPlutusV2
    AlonzoTxInfoSpec.txInfoSignersSpec @era SPlutusV1
    AlonzoTxInfoSpec.txInfoSignersSpec @era SPlutusV2
    AlonzoTxInfoSpec.txInfoCertsSpec @era SPlutusV1
    AlonzoTxInfoSpec.txInfoCertsSpec @era SPlutusV2
    txInfoSpec @era SPlutusV2

txInfoV1Spec ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo 'PlutusV1 era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Spec
txInfoV1Spec = do
  let slang = SPlutusV1
  describe (show slang) $ do
    -- We include here differences wrt to the previous era
    it "translation error on byron txout" $
      expectTranslationError @era slang
        (txBare shelleyInput byronOutput)
        (inject $ ByronTxOutInContext @era (TxOutFromOutput minBound))
    it "translation error on byron txin" $
      expectTranslationError @era slang
        (txBare byronInput shelleyOutput)
        (inject $ ByronTxOutInContext @era (TxOutFromInput byronInput))
    it "translation error on unknown txin (logic error)" $
      expectTranslationError @era slang
        (txBare unknownInput shelleyOutput)
        (inject $ AlonzoContextError $ TranslationLogicMissingInput @era unknownInput)
    it "translation error on inline datum in input" $
      expectTranslationError @era slang
        (txBare inputWithInlineDatum shelleyOutput)
        (inject $ InlineDatumsNotSupported @era (TxOutFromInput inputWithInlineDatum))
    it "translation error on inline datum in output" $
      expectTranslationError @era slang
        (txBare shelleyInput inlineDatumOutput)
        (inject $ InlineDatumsNotSupported @era (TxOutFromOutput minBound))

txInfoSpec ::
  forall era l.
  ( EraTx era
  , EraPlutusTxInfo l era
  , EraPlutusTxOut l era
  , BabbageEraTxBody era
  , AlonzoEraTxWits era
  , Value era ~ MaryValue
  , Inject (BabbageContextError era) (ContextError era)
  , EraTranslateValidityInterval era
  , AtMostEra "Conway" era
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
    it "use reference input starting in Babbage" $
      successfulTranslation @era
        lang
        (exampleUTxO lang)
        (txRefInput shelleyInputTwo)
        hasReferenceInput
    -- This test will fail in PlutusV3 because of ReferenceInputsNotDisjointFromInputs
    when (plutusLanguage lang == PlutusV2) $
      it "use reference input already present in spending inputs in Babbage" $
        successfulTranslation @era
          lang
          (exampleUTxO lang)
          (txRefInput shelleyInput)
          hasReferenceInput
    it "use inline datum in input" $
      successfulTranslation @era
        lang
        (exampleUTxO lang)
        (txBare inputWithInlineDatum shelleyOutput)
        ( \l txInfo -> do
            txInInfo <- expectRight $ toPlutusTxInInfo @_ @era l (exampleUTxO lang) inputWithInlineDatum
            expectOneInput @era l txInInfo txInfo
        )
    it "use inline datum in output" $
      successfulTranslation @era
        lang
        (exampleUTxO lang)
        (txBare shelleyInput inlineDatumOutput)
        (expectOneOutput (translatedOutputEx1 @era))
    it "use reference script in input" $
      successfulTranslation @era
        lang
        (exampleUTxO lang)
        (txBare inputWithRefScript shelleyOutput)
        ( \l txInfo -> do
            txInInfo <- expectRight $ toPlutusTxInInfo @_ @era l (exampleUTxO lang) inputWithRefScript
            expectOneInput @era l txInInfo txInfo
        )
    it "use reference script in output" $
      successfulTranslation @era
        lang
        (exampleUTxO lang)
        (txBare shelleyInput $ refScriptOutput lang)
        (expectOneOutput (translatedOutputEx2 @era lang))
    prop "correctly translate tx with babbage-era features" $ do
      inputs <- listOf1 genShelleyUtxoEntry
      refInputs <- listOf1 genShelleyUtxoEntry
      outputs <- listOf1 genShelleyTxOut
      feeCoin <- arbitrary
      mintValue <- arbitrary
      wdrls <- arbitrary
      datums <- arbitrary @(TxDats era)
      validityRange <- arbitrary
      signers <- arbitrary
      let utxoSet = UTxO $ Map.fromList $ inputs <> refInputs
          txIns = Set.fromList $ fmap fst inputs
          txRefIns = Set.fromList $ fmap fst refInputs
          txBody =
              mkBasicTxBody
                & inputsTxBodyL .~ txIns
                & referenceInputsTxBodyL .~ txRefIns
                & outputsTxBodyL .~ StrictSeq.fromList outputs
                & feeTxBodyL .~ feeCoin
                & mintTxBodyL .~ mintValue
                & withdrawalsTxBodyL .~ wdrls
                & vldtTxBodyL .~ validityRange
                & reqSignerHashesTxBodyL .~ signers
          tx = mkBasicTx txBody & witsTxL . datsTxWitsL .~ datums
      pure
        $ successfulTranslation @era
          lang
          utxoSet
          tx
        -- Assertions
        $ \slang txInfo -> do
          AlonzoTxInfoSpec.expectTxIns slang utxoSet txIns txInfo

          -- Assertions on the transaction reference inputs of the TxInfo
          expectReferenceInputs slang utxoSet txRefIns txInfo

          -- Outputs with Byron addresses are also silently dropped in Alonzo.
          AlonzoTxInfoSpec.expectTxOutputs slang outputs txInfo

          -- Assertions on the fee in the TxInfo
          AlonzoTxInfoSpec.expectFee slang feeCoin txInfo

          -- Assertions on the mint value in the TxInfo
          AlonzoTxInfoSpec.expectMintValue slang mintValue txInfo

          -- Assertions on the withdrawals in the TxInfo
          AlonzoTxInfoSpec.expectWithdrawals slang wdrls txInfo

          -- Assertions on the datums in the TxInfo
          AlonzoTxInfoSpec.expectDatums slang (tx ^. witsTxL) txInfo

          -- Assertions on the txId in the TxInfo
          AlonzoTxInfoSpec.expectTxId slang txBody txInfo

          -- Assertions on the validity range in the TxInfo
          AlonzoTxInfoSpec.expectValidityRange @era slang validityRange txInfo

          -- Assertions on the required signatories in the TxInfo
          AlonzoTxInfoSpec.expectSignatories signers slang txInfo

          -- TODO
          -- expectRedeemers slang redeemers txInfo

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

refScriptOutput :: forall era l. (BabbageEraTxOut era, EraPlutusTxInfo l era) => SLanguage l -> TxOut era
refScriptOutput _slang =
  mkBasicTxOut shelleyAddr (inject $ Coin 3)
    & referenceScriptTxOutL .~ SJust (alwaysSucceeds @l 3)

-- This input is only a "Shelley input" in the sense
-- that we attach it to a Shelley output in the UTxO created below.
shelleyInput :: TxIn
shelleyInput = mkTxInPartial genesisId 2

shelleyInputTwo :: TxIn
shelleyInputTwo = mkTxInPartial genesisId 3

inputWithInlineDatum :: TxIn
inputWithInlineDatum = mkTxInPartial genesisId 4

inputWithRefScript :: TxIn
inputWithRefScript = mkTxInPartial genesisId 5

exampleUTxO ::
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
  , Value era ~ MaryValue
  ) =>
  SLanguage l ->
  UTxO era
exampleUTxO slang =
  UTxO $
    Map.fromList
      [ (byronInput, byronOutput)
      , (shelleyInput, shelleyOutput)
      , (shelleyInputTwo, shelleyOutput)
      , (inputWithInlineDatum, inlineDatumOutput)
      , (inputWithRefScript, refScriptOutput slang)
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
    SPlutusV4 -> PV4.txInfoReferenceInputs txInfo `shouldNotBe` mempty

plutusTxInInfoInputs ::
  forall era l. HasCallStack => SLanguage l -> PlutusTxInfo l -> [PlutusTxInInfo era l]
plutusTxInInfoInputs slang txInfo =
  case slang of
    SPlutusV1 -> error "PlutusV1 not supported"
    SPlutusV2 -> PV2.txInfoInputs txInfo
    SPlutusV3 -> PV3.txInfoInputs txInfo
    SPlutusV4 -> PV4.txInfoInputs txInfo

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
    SPlutusV4 -> PV4.txInfoOutputs txInfo `shouldBe` [o]

expectReferenceInputs ::
  forall era l.
  ( EraPlutusTxInfo l era
  ) =>
  SLanguage l ->
  UTxO era ->
  Set.Set TxIn ->
  PlutusTxInfo l ->
  Expectation
expectReferenceInputs slang utxoSet txIns txInfo =
  case slang of
    SPlutusV1 -> expectationFailure "PlutusV1 not supported"
    SPlutusV2 -> do
      expectedInputs <- traverse (expectRight . toPlutusTxInInfo slang utxoSet) (Set.toList txIns)
      PV2.txInfoReferenceInputs txInfo `shouldBe` expectedInputs
    SPlutusV3 -> do
      expectedInputs <- traverse (expectRight . toPlutusTxInInfo slang utxoSet) (Set.toList txIns)
      PV3.txInfoReferenceInputs txInfo `shouldBe` expectedInputs
    SPlutusV4 -> do
      expectedInputs <- traverse (expectRight . toPlutusTxInInfo slang utxoSet) (Set.toList txIns)
      PV4.txInfoReferenceInputs txInfo `shouldBe` expectedInputs

successfulTranslation ::
  forall era l.
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
  ) =>
  SLanguage l ->
  UTxO era ->
  Tx TopTx era ->
  (SLanguage l -> PlutusTxInfo l -> Expectation) ->
  Expectation
successfulTranslation slang utxo tx f =
  let lti =
        LedgerTxInfo
          { ltiProtVer = ProtVer (eraProtVerLow @era) 0
          , ltiEpochInfo = ei
          , ltiSystemStart = ss
          , ltiUTxO = utxo
          , ltiTx = tx
          }
   in case toPlutusTxInfoForPurpose slang lti (SpendingPurpose AsPurpose) of
        Right txInfo -> f slang txInfo
        Left e -> assertFailure $ "No translation error was expected, but got: " <> show e

expectTranslationError ::
  forall era l.
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
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
          , ltiUTxO = exampleUTxO slang
          , ltiTx = tx
          }
   in case toPlutusTxInfoForPurpose slang lti (SpendingPurpose AsPurpose) of
        Right txInfo ->
          assertFailure $ "This translation was expected to fail, but it succeeded: " <> show txInfo
        Left e -> e `shouldBe` expected

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
  forall era l.
  ( BabbageEraTxOut era
  , EraPlutusTxInfo l era
  , Value era ~ MaryValue
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  SLanguage l ->
  PV2.TxOut
translatedOutputEx2 slang =
  errorTranslate @era "translatedOutputEx2" $
    transTxOutV2 @era (TxOutFromOutput minBound) $ refScriptOutput slang

genesisId :: TxId
genesisId = TxId (unsafeMakeSafeHash (mkDummyHash (0 :: Int)))

genShelleyUtxoEntry :: (AlonzoEraTxOut era, Arbitrary (Value era)) => Gen (TxIn, TxOut era)
genShelleyUtxoEntry = do
  txIn <- arbitrary
  txOut <- genShelleyTxOut
  pure (txIn, txOut)

genShelleyTxOut :: forall era. (AlonzoEraTxOut era, Arbitrary (Value era)) => Gen (TxOut era)
genShelleyTxOut = do
  dataHash <- arbitrary
  txOut <-
    mkBasicTxOut
      <$> genShelleyAddr
      <*> scale (`div` 15) arbitrary
  pure $ txOut & dataHashTxOutL .~ dataHash

genShelleyAddr :: Gen Addr
genShelleyAddr = Addr <$> arbitrary <*> arbitrary <*> arbitrary
